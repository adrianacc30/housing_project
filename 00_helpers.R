# Shared paths and input checks. Run scripts from the repository root.

if (!dir.exists(file.path("data", "processed"))) {
  stop("Run this project from the repository root, where README.md is located.")
}

data_dir <- file.path("data", "processed")
figure_dir <- "figures"
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

check_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Run install_packages.R once; see RUNNING.md.")
  }
}

read_areas <- function(filename, required_columns) {
  check_packages("readr")
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("Missing input: ", path)
  result <- readr::read_csv(
    path,
    col_types = readr::cols(code = readr::col_character()),
    show_col_types = FALSE
  )
  missing <- setdiff(c("code", "name", required_columns), names(result))
  if (length(missing)) stop("Missing columns in ", filename, ": ", paste(missing, collapse = ", "))
  result <- result[grepl("^[0-9]{3}$", result$code), ]
  if (nrow(result) != 38L || anyDuplicated(result$code)) {
    stop(filename, " must contain exactly 38 distinct Stadtteil codes.")
  }
  for (column in required_columns) {
    if (!is.numeric(result[[column]]) || anyNA(result[[column]])) {
      stop("Missing or non-numeric values in ", filename, ": ", column)
    }
  }
  result
}

join_areas <- function(left, right) {
  check_packages("dplyr")
  if (anyDuplicated(left$code) || anyDuplicated(right$code) ||
      !setequal(left$code, right$code)) {
    stop("Neighbourhood join requires the same distinct codes on both sides.")
  }
  dplyr::inner_join(left, right, by = "code")
}

percentage_growth <- function(current, previous) {
  ifelse(previous > 0, 100 * (current / previous - 1), NA_real_)
}
