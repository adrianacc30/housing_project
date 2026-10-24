# Optional: rebuild aggregate CSVs from the original City of Mannheim PDFs.
# Based on the population, household, housing-stock, and completions extractors.
# The default plotting workflow uses the already bundled CSVs.

source("scripts/00_helpers.R")
check_packages(c("pdftools", "readr", "sf"))

raw_dir <- file.path("data", "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

sources <- data.frame(
  filename = c("d202601_einwohnerbestand_2025.pdf",
               "d202502_privathaushalte_2024.pdf",
               "d202602_privathaushalte_2025.pdf",
               "d202509_wohnungsbau_2024.pdf",
               "d202609_wohnungsbau_2025.pdf"),
  url = c(
    "https://www.mannheim.de/sites/default/files/2026-03/d202601_einwohnerbestand_2025.pdf",
    "https://www.mannheim.de/sites/default/files/2025-03/d202502_privathaushalte_2024.pdf",
    "https://www.mannheim.de/sites/default/files/2026-04/d202602_privathaushalte_2025.pdf",
    "https://www.mannheim.de/sites/default/files/2025-09/d202509_wohnungsbau_2024.pdf",
    "https://www.mannheim.de/sites/default/files/2026-08/d202609_wohnungsbau_2025.pdf"
  ), stringsAsFactors = FALSE
)

for (i in seq_len(nrow(sources))) {
  destination <- file.path(raw_dir, sources$filename[i])
  if (!file.exists(destination)) {
    utils::download.file(sources$url[i], destination, mode = "wb", quiet = TRUE)
  }
}

geometry <- sf::st_read(file.path(data_dir, "neighbourhoods.geojson"), quiet = TRUE)
name_lookup <- setNames(geometry$name, as.character(geometry$code))

parse_de_integer <- function(values) {
  as.numeric(gsub(".", "", values, fixed = TRUE))
}

extract_table <- function(filename, page, numeric_columns) {
  text <- pdftools::pdf_text(file.path(raw_dir, filename))[page]
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  rows <- list()
  n_columns <- length(numeric_columns)
  for (line in lines) {
    tokens <- strsplit(trimws(line), "[[:space:]]+")[[1]]
    if (length(tokens) < n_columns + 2L || !grepl("^[0-9]{2,3}$", tokens[1])) next
    values <- tail(tokens, n_columns)
    if (!all(grepl("^[0-9]+([.][0-9]{3})*$", values))) {
      stop("Unexpected numeric fields in ", filename, ": ", line)
    }
    row <- data.frame(code = tokens[1],
                      name = paste(tokens[2:(length(tokens) - n_columns)], collapse = " "),
                      stringsAsFactors = FALSE)
    for (i in seq_along(numeric_columns)) row[[numeric_columns[i]]] <- parse_de_integer(values[i])
    rows[[length(rows) + 1L]] <- row
  }
  result <- do.call(rbind, rows)
  if (nrow(result) != 55L) stop("Expected 17 districts and 38 neighbourhoods in ", filename)
  result <- result[nchar(result$code) == 3L, ]
  stopifnot(nrow(result) == 38L, !anyDuplicated(result$code),
            setequal(result$code, names(name_lookup)))
  result$name <- unname(name_lookup[result$code])
  result[order(result$code), ]
}

age_columns <- c("total", "u18", "a18_25", "a25_30", "a30_65", "a65_80", "o80")
household_columns <- c("total_hh", "single_hh", "multi_no_kids", "multi_with_kids")
dwelling_columns <- c("total", "w1", "w2", "w3", "w4", "w5", "w6", "w7plus", "total_rooms")

tables <- list(
  age_2025 = extract_table(sources$filename[1], 8, age_columns),
  households_2024 = extract_table(sources$filename[2], 9, household_columns),
  households_2025 = extract_table(sources$filename[3], 9, household_columns),
  dwellings_2024 = extract_table(sources$filename[4], 5, dwelling_columns),
  dwellings_2025 = extract_table(sources$filename[5], 5, dwelling_columns)
)

for (name in names(tables)) {
  table <- tables[[name]]
  if (grepl("^age", name)) {
    stopifnot(all(rowSums(table[, age_columns[-1]]) == table$total))
  } else if (grepl("^households", name)) {
    stopifnot(all(rowSums(table[, household_columns[-1]]) == table$total_hh))
  } else {
    stopifnot(all(rowSums(table[, dwelling_columns[2:8]]) == table$total))
  }
}

stopifnot(sum(tables$age_2025$total) == 333778,
          sum(tables$households_2024$total_hh) == 178589,
          sum(tables$households_2025$total_hh) == 179498,
          sum(tables$dwellings_2024$total) == 174979,
          sum(tables$dwellings_2025$total) == 175608)

completion_text <- pdftools::pdf_text(file.path(raw_dir, sources$filename[5]))[15]
completion_lines <- strsplit(completion_text, "\n", fixed = TRUE)[[1]]
row <- completion_lines[grepl("^[[:space:]]*2025[[:space:]]+", completion_lines)]
stopifnot(length(row) == 1L)
tokens <- strsplit(trimws(row), "[[:space:]]+")[[1]]
values <- parse_de_integer(tokens[-1])
stopifnot(length(values) == 9L, values[1] == 691, sum(values[2:8]) == values[1])
completions <- data.frame(
  category = c("1 room", "2 rooms", "3 rooms", "4 rooms", "5 rooms", "6 rooms", "7+ rooms"),
  completions_count = values[2:8]
)

# Write only after all tables and totals pass validation.
for (name in names(tables)) {
  readr::write_csv(tables[[name]], file.path(data_dir, paste0(name, ".csv")))
}
readr::write_csv(completions, file.path(data_dir, "completions_2025.csv"))
message("Aggregate source tables validated and written to data/processed/.")
