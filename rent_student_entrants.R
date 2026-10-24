# Rent growth and first-time student entrants, Germany (exploratory)
# Run from the project root: Rscript rent_student_entrants.R
# Personal file paths have been removed. See DATA.md before interpreting results.
# University-to-district matches are proposals and still require substantive review.
# This public copy has passed syntax checks, but has not been executed with its inputs.

required_packages <- c("readr", "dplyr", "stringr", "tidyr", "stringi",
                       "purrr", "readxl", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                             logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install missing packages: ",
                                  paste(missing_packages, collapse = ", "))

manifest_path <- file.path("data", "population_files.csv")
if (!file.exists(manifest_path)) stop("Missing data/population_files.csv")
population_manifest <- readr::read_csv(manifest_path, show_col_types = FALSE,
                                      col_types = readr::cols(year = readr::col_integer(),
                                                             file = readr::col_character()))
stopifnot(all(c("year", "file") %in% names(population_manifest)),
          nrow(population_manifest) > 0,
          !anyNA(population_manifest$year),
          !anyNA(population_manifest$file),
          !anyDuplicated(population_manifest$year),
          all(c(2015L, 2024L) %in% population_manifest$year))
population_manifest$path <- file.path("data", "raw", population_manifest$file)
required_files <- c(population_manifest$path,
                    file.path("data", "raw", "21311-0011_de.csv"),
                    file.path("data", "raw", "RWIGEOREDX_APRENT_V17_PUF_YEAR_ABS.xlsx"))
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) stop("Add the source files described in DATA.md: ",
                               paste(missing_files, collapse = ", "))

# =============================================================
# build_entrants_kreis_population.R
#   - encoding-safe
#   - normalises Kreis codes (4-digit -> 5-digit, RB rows dropped)
#   - city + Kreis dictionary
#   - population years are declared explicitly in data/population_files.csv
# =============================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
  library(stringi); library(purrr)
})

path_fa <- file.path("data", "raw", "21311-0011_de.csv")
path_pop <- population_manifest$path[which.max(population_manifest$year)]
out_dir <- file.path("outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------
# 0a) Kreis-code normalisation (NA-safe, UTF-8-safe)
# -------------------------------------------------------------
normalize_kreis_code <- function(code) {
  code <- as.character(code)
  code <- gsub("[^0-9]", "", code, useBytes = TRUE)
  is4 <- !is.na(code) & nchar(code) == 4L
  code[is4] <- paste0("0", code[is4])
  code[!is.na(code) & code == ""] <- NA_character_
  # Canonical district codes for city-states, for the rent-data join.
  code[!is.na(code) & code == "02"] <- "02000"
  code[!is.na(code) & code == "11"] <- "11000"
  code
}
stopifnot(
  identical(normalize_kreis_code(c("53","5315","05315","02","11",NA,"")),
            c("53","05315","05315","02000","11000",NA_character_, NA_character_))
)

# -------------------------------------------------------------
# 0b) Helper: which codes count as Kreis-level
#     (5-digit codes plus the two city-states "02"/"11")
# -------------------------------------------------------------
# Used by both the population builders and the matcher.
is_kreis_row <- function(code) {
  (!is.na(code) & nchar(code) == 5) | (!is.na(code) & code %in% c("02", "11"))
}

# -------------------------------------------------------------
# 0c) Encoding-safe normalisation
# -------------------------------------------------------------
repair_utf8 <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  bad <- is.na(y) & !is.na(x); y[bad] <- x[bad]; y
}
ascii_key <- function(x) {
  x <- as.character(x)
  x <- repair_utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- str_trim(gsub("\\s+", " ", x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# -------------------------------------------------------------
# 1) Studienanfänger
# -------------------------------------------------------------
lines <- read_lines(path_fa, locale = locale(encoding = "UTF-8"))
ws_lines <- lines[str_detect(lines, "^WS\\s+\\d{4}/\\d{2};HS")]

fa_raw <- read_delim(
  I(paste(ws_lines, collapse = "\n")),
  delim = ";", col_names = FALSE, na = c("", "-"), trim_ws = TRUE,
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8")
)

if (nrow(fa_raw) == 0 || ncol(fa_raw) < 20) {
  stop("No compatible winter-semester rows; check the student CSV export format.")
}
raw_counts <- fa_raw$X20
if (any(!is.na(raw_counts) & !grepl("^[0-9]+$", raw_counts))) {
  stop("Column 20 has non-integer values. Inspect the export header and number format.")
}

fa_all <- fa_raw %>%
  transmute(
    semester         = X1,
    hs               = X2,
    hochschule       = X3,
    studienanfaenger = suppressWarnings(as.numeric(X20))
  ) %>%
  filter(str_detect(semester, "^WS\\s+\\d{4}/\\d{2}$")) %>%
  mutate(jahr = as.integer(str_sub(semester, 4L, 7L))) %>%
  filter(jahr %in% population_manifest$year)
if (nrow(fa_all) == 0 || !any(!is.na(fa_all$studienanfaenger))) {
  stop("No usable entrants for the declared population years.")
}
if (anyDuplicated(fa_all[c("semester", "hs")])) {
  stop("Duplicate university/semester rows; verify the selected CSV dimensions.")
}
# jahr is KEPT so section 7 can join on it

fa_dropped <- fa_all %>% group_by(hs) %>%
  filter(!any(!is.na(studienanfaenger) & studienanfaenger > 0)) %>% ungroup()
fa <- fa_all %>% group_by(hs) %>%
  filter(any(!is.na(studienanfaenger) & studienanfaenger > 0)) %>% ungroup()

cat("Universities with data :", n_distinct(fa$hs), "\n")
cat("Dropped (all-NA/all-0):", n_distinct(fa_dropped$hs), "\n\n")
write_excel_csv(fa_dropped %>% distinct(hs, hochschule),
                file.path(out_dir, "dropped_no_entrants.csv"), na = "")

# -------------------------------------------------------------
# 2) Population — latest declared year supplies the matcher dictionary
# -------------------------------------------------------------
raw_lines <- readLines(path_pop, encoding = "UTF-8", warn = FALSE)
start_idx <- which(str_detect(raw_lines, "^(DG|[0-9]{2});"))[1]
end_idx   <- which(str_detect(raw_lines, "^(_{3,}|©|Stand:)"))[1]
if (is.na(end_idx)) end_idx <- length(raw_lines) + 1
if (is.na(start_idx) || end_idx <= start_idx) stop("Population CSV body not found.")
body <- raw_lines[start_idx:(end_idx - 1)]

tmp_csv <- tempfile(fileext = ".csv"); writeLines(body, tmp_csv, useBytes = TRUE)

pop_raw <- read_delim(
  tmp_csv, delim = ";",
  col_names = c("code","name","age","total","male","female"),
  col_types = cols(code = col_character(), name = col_character(),
                   age = col_character(), total = col_double(),
                   male = col_double(), female = col_double()),
  na = c("-","","NA"), trim_ws = TRUE,
  locale = locale(encoding = "UTF-8")
) %>%
  filter(!is.na(age)) %>%
  mutate(code = normalize_kreis_code(code),
         across(c(total, male, female), ~ as.numeric(.x)))

pop_kreis <- pop_raw %>%
  filter(str_detect(age, "^\\d{2} bis unter \\d{2} Jahre$")) %>%
  mutate(age_low = as.integer(str_extract(age, "^\\d{2}"))) %>%
  filter(age_low >= 18, age_low <= 25) %>%
  filter(is_kreis_row(code), !is.na(total)) %>%
  group_by(code, name) %>%
  summarise(pop_18_25 = sum(total, na.rm = TRUE), .groups = "drop") %>%
  filter(pop_18_25 > 0)

pop_18 <- pop_raw %>%
  filter(age == "18 bis unter 19 Jahre") %>%
  filter(is_kreis_row(code), !is.na(total)) %>%
  select(code, pop_18 = total)

pop_kreis <- pop_kreis %>% left_join(pop_18, by = "code")

# -------------------------------------------------------------
# 2b) Year-matched population from the explicit manifest.
# Never infer a file's reference year from browser-added filename suffixes.
# -------------------------------------------------------------
file_specs <- population_manifest %>% select(year, path)

read_pop_yearly <- function(path, year) {
  rl <- readLines(path, encoding = "UTF-8", warn = FALSE)
  s  <- which(str_detect(rl, "^(DG|[0-9]{2});"))[1]
  e  <- which(str_detect(rl, "^(_{3,}|©|Stand:)"))[1]
  if (is.na(s)) stop("No data start in: ", path)
  if (is.na(e)) e <- length(rl) + 1
  if (e <= s) stop("Population CSV end precedes its body: ", path)
  
  tmp <- tempfile(fileext = ".csv")
  writeLines(rl[s:(e - 1)], tmp, useBytes = TRUE)
  
  pop <- read_delim(
    tmp, delim = ";",
    col_names = c("code","name","age","total","male","female"),
    col_types = cols(code = col_character(), name = col_character(),
                     age = col_character(), total = col_double(),
                     male = col_double(), female = col_double()),
    na = c("-","","NA"), trim_ws = TRUE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    filter(!is.na(age)) %>%
    mutate(code = normalize_kreis_code(code),
           across(c(total, male, female), ~ as.numeric(.x))) %>%
    filter(is_kreis_row(code), !is.na(total))
  
  p1825 <- pop %>%
    filter(str_detect(age, "^\\d{2} bis unter \\d{2} Jahre$")) %>%
    mutate(age_low = as.integer(str_extract(age, "^\\d{2}"))) %>%
    filter(age_low >= 18, age_low <= 25) %>%
    group_by(code) %>%
    summarise(pop_18_25 = sum(total, na.rm = TRUE), .groups = "drop")
  
  p18 <- pop %>%
    filter(age == "18 bis unter 19 Jahre") %>%
    select(code, pop_18 = total)
  
  ptot <- pop %>%
    filter(age == "Insgesamt") %>%
    select(code, pop_total = total)
  
  p1825 %>%
    left_join(p18,   by = "code") %>%
    left_join(ptot,  by = "code") %>%
    mutate(year = as.integer(year), .before = 1)
}

pop_kreis_yearly <- map2_dfr(file_specs$path, file_specs$year, read_pop_yearly)
if (anyDuplicated(pop_kreis_yearly[c("year", "code")])) {
  stop("Duplicate population district/year rows; inspect source files.")
}

glimpse(pop_kreis_yearly)

# -------------------------------------------------------------
# 3) Kreis / city dictionaries
# -------------------------------------------------------------
strip_kreis_qualifiers <- function(name) {
  name %>%
    str_remove(",\\s*(Stadtkreis|kreisfreie Stadt|Landkreis|Hansestadt|Landeshauptstadt|Stadt|Kreis|Regionalverband|Städteregion)\\s*$") %>%
    str_remove("^(Landkreis|Region|Städteregion|Regionalverband|Kreis|Hansestadt|Landeshauptstadt)\\s+") %>%
    str_remove("\\s*\\([^)]*\\)\\s*$") %>%
    str_remove("\\s+im\\s+\\S+$") %>%
    str_remove("\\s+am\\s+\\S+$") %>%
    str_remove("\\s+in\\s+\\S+$") %>%
    str_trim()
}
is_kreisfrei <- function(name)
  str_detect(name, "kreisfreie Stadt|Stadtkreis|Landeshauptstadt|Hansestadt")

kreis_meta <- pop_kreis %>%
  mutate(base_raw = strip_kreis_qualifiers(name),
         base_key = ascii_key(base_raw),
         is_city  = is_kreisfrei(name)) %>%
  filter(!is.na(base_key), nchar(base_key) >= 3)

kreis_dict <- kreis_meta %>% select(code, kreis = name, base_key, is_city)

city_from_kreisfrei <- kreis_meta %>% filter(is_city) %>%
  transmute(city_key = base_key, code, priority = 1L)

split_compound <- kreis_meta %>%
  filter(!is_city, str_detect(base_raw, "-")) %>%
  mutate(part = str_split(base_raw, "\\s*-\\s*")) %>% unnest(part) %>%
  mutate(city_key = ascii_key(part)) %>%
  filter(!is.na(city_key), nchar(city_key) >= 3) %>%
  transmute(city_key, code, priority = 2L)

manual_cities <- tribble(
  ~city,                     ~kreis_code,
  "Garching",                "09184", "Weihenstephan",           "09178",
  "Freising",                "09178", "Bad Honnef",              "05382",
  "Sankt Augustin",          "05382", "Kamp-Lintfort",           "05170",
  "Vallendar",               "07137", "Oestrich-Winkel",         "06439",
  "Geisenheim",              "06439", "Horb",                    "08237",
  "Bad Mergentheim",         "08128", "Villingen-Schwenningen",  "08326",
  "Furtwangen",              "08326", "Tuttlingen",              "08327",
  "Triesdorf",               "09571", "Rothenburg",              "09571",
  "Feuchtwangen",            "09571", "Weißenburg",              "09577",
  "Neuburg",                 "09185", "Mühldorf",                "09183",
  "Burghausen",              "09171", "Münchberg",               "09464",
  "Cham",                    "09372", "Künzelsau",               "08126",
  "Schwäbisch Hall",         "08127", "Nürtingen",               "08116",
  "Geislingen",              "08117", "Iserlohn",                "05962",
  "Meschede",                "05958", "Gummersbach",             "05374",
  "Lemgo",                   "05766", "Detmold",                 "05766",
  "Höxter",                  "05762", "Steinfurt",               "05566",
  "Soest",                   "05974", "Elsfleth",                "03461",
  "Senftenberg",             "12066", "Bernburg",                "15089",
  "Köthen",                  "15082", "Mittweida",               "14522",
  "Freiberg",                "14522", "Zittau",                  "14626",
  "Wernigerode",             "15085", "Halberstadt",             "15085",
  "Idar-Oberstein",          "07134", "Birkenfeld",              "07134",
  "Remagen",                 "07131", "Höhr-Grenzhausen",        "07143",
  "Lingen",                  "03454", "Bocholt",                 "05554",
  "Edenkoben",               "07337", "Mayen",                   "07137",
  "Hachenburg",              "07143", "Aachen",                  "05334"
) %>%
  mutate(city_key = ascii_key(city), priority = 0L) %>%
  filter(!is.na(city_key)) %>%
  select(city_key, code = kreis_code, priority)

city_dict <- bind_rows(manual_cities, city_from_kreisfrei, split_compound) %>%
  filter(!is.na(city_key), nchar(city_key) >= 3) %>%
  arrange(city_key, priority) %>%
  group_by(city_key) %>% slice(1) %>% ungroup()

cat("city_dict entries :", nrow(city_dict),  "\n")
cat("kreis_dict entries:", nrow(kreis_dict), "\n\n")

# -------------------------------------------------------------
# 4) Matcher
# -------------------------------------------------------------
extract_city_candidates <- function(uni) {
  pat <- "([\\p{L}][\\p{L}\\-\\.]*(?:\\s+[\\p{L}][\\p{L}\\-\\.]*)*)"
  m <- str_match(uni, paste0("(?i)\\bin\\s+", pat))[,2]; kind <- "in"
  if (is.na(m)) { m <- str_match(uni, paste0("(?i)\\bim\\s+Landkreis\\s+", pat))[,2]; kind <- "landkreis" }
  if (is.na(m)) { m <- str_match(uni, paste0("(?i)\\bi\\.\\s*", pat))[,2]; kind <- "in" }
  if (is.na(m)) return(list(candidates = character(0), kind = NA_character_))
  m <- str_trim(str_remove(m, "\\s*\\([^)]*\\)\\s*$"))
  words <- str_split(m, "\\s+")[[1]]
  n <- min(length(words), 3)
  cands <- vapply(rev(seq_len(n)), function(k) paste(words[1:k], collapse = " "), "")
  list(candidates = cands, kind = kind)
}

find_best_key <- function(uni_key, keys) {
  hits <- str_detect(uni_key, paste0("\\b", str_escape(keys), "\\b"))
  matched <- keys[hits]
  if (length(matched) == 0) return(NA_character_)
  matched[which.max(nchar(matched))]
}

match_kreis <- function(uni, city_dict, kreis_dict) {
  empty <- list(code = NA_character_, method = NA_character_)
  if (is.na(uni) || uni == "") return(empty)
  ex <- extract_city_candidates(uni)
  if (length(ex$candidates) > 0) {
    for (cand in ex$candidates) {
      key <- ascii_key(cand); if (is.na(key)) next
      if (identical(ex$kind, "landkreis")) {
        hit <- kreis_dict %>% filter(base_key == key, !is_city)
        if (nrow(hit) > 0) return(list(code = hit$code[1], method = "campus suffix ('im Landkreis X') → kreis"))
      }
      hit <- city_dict %>% filter(city_key == key)
      if (nrow(hit) > 0) return(list(code = hit$code[1], method = "campus suffix ('in X') → city"))
      hit <- kreis_dict %>% filter(base_key == key)
      if (nrow(hit) > 0) return(list(code = hit$code[1], method = "campus suffix ('in X') → kreis"))
    }
    return(list(code = NA_character_, method = "campus suffix unresolved"))
  }
  uni_key <- ascii_key(uni); if (is.na(uni_key)) return(empty)
  city_hit  <- find_best_key(uni_key, city_dict$city_key)
  kreis_hit <- find_best_key(uni_key, kreis_dict$base_key)
  if (!is.na(city_hit) && !is.na(kreis_hit)) {
    if (nchar(city_hit) >= nchar(kreis_hit))
      return(list(code = city_dict$code[match(city_hit, city_dict$city_key)][1], method = "word match → city"))
    return(list(code = kreis_dict$code[match(kreis_hit, kreis_dict$base_key)][1], method = "word match → kreis"))
  }
  if (!is.na(city_hit))  return(list(code = city_dict$code[match(city_hit, city_dict$city_key)][1], method = "word match → city"))
  if (!is.na(kreis_hit)) return(list(code = kreis_dict$code[match(kreis_hit, kreis_dict$base_key)][1], method = "word match → kreis"))
  empty
}

# -------------------------------------------------------------
# 5) Apply matcher
# -------------------------------------------------------------
uni_list <- fa %>% distinct(hs, hochschule)
res <- lapply(uni_list$hochschule, match_kreis,
              city_dict = city_dict, kreis_dict = kreis_dict)

mapping <- uni_list %>%
  mutate(
    kreis_code = vapply(res, `[[`, character(1), "code"),
    method     = vapply(res, `[[`, character(1), "method"),
    status     = if_else(is.na(kreis_code), "empty", "matched")
  ) %>%
  left_join(pop_kreis %>% select(code, kreis = name, pop_18_25, pop_18),
            by = c("kreis_code" = "code")) %>%
  arrange(status, hs)

cat("Auto-matched:", sum(mapping$status == "matched"), "of", nrow(mapping), "\n")
cat("Still empty :", sum(mapping$status == "empty"),  "\n\n")
write_excel_csv(mapping %>% filter(status == "matched"),
                file.path(out_dir, "matched_hs_check.csv"), na = "")
write_excel_csv(mapping %>% filter(status == "empty"),
                file.path(out_dir, "unmatched_hs.csv"),     na = "")

# -------------------------------------------------------------
# 6) Manual overrides
# -------------------------------------------------------------
overrides <- tribble(
  ~hs,      ~kreis_code,
  "HS1632", "09178", "HS1633", "09184", "HS1634", "09263",
  "HS1323", "09184",
  "HS0810", "09184", "HS0811", "09184", "HS0812", "09184",
  "HS0012", "06636", "HS0372", "14628", "HS0610", "14626",
  "HS6741", "08326", "HS6742", "08326", "HS6743", "08326",
  "HS8171", "08125", "HS8174", "08237", "HS817C", "08326"
)

mapping_fixed <- mapping %>%
  left_join(overrides %>% rename(override = kreis_code), by = "hs") %>%
  mutate(
    kreis_code = coalesce(override, kreis_code),
    method     = if_else(!is.na(override), "manual override", method),
    status     = if_else(is.na(kreis_code), "empty", "matched")
  ) %>%
  select(-override, -kreis, -pop_18_25, -pop_18) %>%
  left_join(pop_kreis %>% select(code, kreis = name, pop_18_25, pop_18),
            by = c("kreis_code" = "code"))

hs_kreis_mapping <- mapping_fixed %>%
  select(hs, hochschule, kreis_code, kreis) %>% arrange(hs)
write_excel_csv(hs_kreis_mapping,
                file.path(out_dir, "hs_kreis_mapping.csv"), na = "")

# -------------------------------------------------------------
# 7) Final joined dataset — year-matched
# -------------------------------------------------------------
result <- fa %>%                                                 # fa already
  left_join(mapping_fixed, by = c("hs", "hochschule")) %>%       # has jahr
  select(-pop_18_25, -pop_18) %>%                                # drop dictionary-year values
  left_join(pop_kreis_yearly,
            by = c("kreis_code" = "code", "jahr" = "year")) %>%
  select(semester, jahr, hs, hochschule, kreis_code, kreis,
         studienanfaenger, pop_18_25, pop_18, pop_total,
         method, status) %>%
  arrange(semester, hs)

write_excel_csv(result, file.path(out_dir, "studienanfaenger_kreis.csv"), na = "")
saveRDS(result, file.path(out_dir, "studienanfaenger_kreis.rds"))

# -------------------------------------------------------------
# 8) Sanity checks
# -------------------------------------------------------------
cat("\n--- sanity checks ---\n")
cat("Rows in result           :", nrow(result), "\n")
cat("Rows with NA pop_18_25   :", sum(is.na(result$pop_18_25)), "\n")
cat("Years in result          :",
    paste(range(result$jahr, na.rm = TRUE), collapse = ".."), "\n")

cat("\nMatched but missing population (bug if any):\n")
print(result %>%
        filter(status == "matched", !is.na(studienanfaenger),
               studienanfaenger > 0, is.na(pop_18_25)) %>%
        distinct(hs, kreis_code, jahr) %>%
        arrange(jahr, hs))

cat("\nResidual empty codes with >0 students:\n")
print(result %>%
        filter(status == "empty", !is.na(studienanfaenger), studienanfaenger > 0) %>%
        distinct(hs, hochschule) %>% arrange(hs), n = 40)

cat("\nPer-year coverage:\n")
print(result %>% count(jahr, wt = !is.na(pop_18_25), name = "with_pop"))

# =============================================================
# Scatterplot: rent growth 2015–2024  vs.
#              Δ first-time entrants per 100 young residents
# =============================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl)
  library(ggplot2); library(stringr); library(readr); library(purrr)
})

path_pop <- population_manifest$path[which.max(population_manifest$year)]
kid_path <- file.path("data", "raw", "RWIGEOREDX_APRENT_V17_PUF_YEAR_ABS.xlsx")
out_dir <- file.path("outputs")

# -------------------------------------------------------------
# 1. Patch the 11 biggest missing universities
# -------------------------------------------------------------
override_big <- tribble(
  ~hs,      ~override_code, ~override_name,
  "HS1100", "05111", "Düsseldorf, kreisfreie Stadt",
  "HS1110", "05315", "Köln, kreisfreie Stadt",
  "HS1120", "05515", "Münster, kreisfreie Stadt",
  "HS1270", "08416", "Tübingen, Landkreis",
  "HS1330", "09663", "Würzburg, kreisfreie Stadt",
  "HS1030", "03159", "Göttingen, Landkreis",
  "HS1320", "09162", "München, kreisfreie Stadt",   # umbrella 2011–2016
  "HS1322", "09162", "München, kreisfreie Stadt",   # 2017+
  "HS1360", "10041", "Regionalverband Saarbrücken",
  "HS0990", "03355", "Lüneburg, Landkreis",
  "HS1010", "01003", "Lübeck, kreisfreie Stadt"
)

result_patched <- result %>%
  left_join(override_big, by = "hs") %>%
  mutate(
    kreis_code = coalesce(override_code, kreis_code),
    kreis      = coalesce(override_name, kreis),
    method     = if_else(!is.na(override_code), "manual override (plot stage)", method),
    status     = if_else(is.na(kreis_code), "empty", "matched")
  ) %>%
  select(-override_code, -override_name) %>%
  # Drop dictionary-year population columns and refresh from the yearly table.
  select(-any_of(c("pop_18_25", "pop_18", "pop_total"))) %>%
  left_join(pop_kreis_yearly,
            by = c("kreis_code" = "code", "jahr" = "year"))

# -------------------------------------------------------------
# 2. Entrants side — 2015 and 2024 only
# -------------------------------------------------------------
write_excel_csv(result_patched %>%
                  distinct(hs, hochschule, kreis_code, kreis, method, status),
                file.path(out_dir, "final_mapping_check.csv"), na = "")
write_excel_csv(result_patched %>%
                  group_by(jahr, status) %>%
                  summarise(institutions = n_distinct(hs),
                            known_entrants = sum(studienanfaenger, na.rm = TRUE),
                            missing_counts = sum(is.na(studienanfaenger)),
                            .groups = "drop"),
                file.path(out_dir, "mapping_coverage.csv"), na = "")

enrol_side <- result_patched %>%
  filter(status == "matched", !is.na(pop_18_25), jahr %in% c(2015, 2024)) %>%
  group_by(kreis_code, jahr) %>%
  summarise(
    E     = if (anyNA(studienanfaenger)) NA_real_ else sum(studienanfaenger),
    P     = first(pop_18_25),
    kreis = first(kreis),
    .groups = "drop"
  )

kreis_wide <- enrol_side %>%
  pivot_wider(names_from = jahr, values_from = c(E, P), names_sep = "_") %>%
  filter(!is.na(E_2015), !is.na(E_2024),
         !is.na(P_2015), !is.na(P_2024), P_2015 > 0, P_2024 > 0) %>%
  mutate(
    enrol_2015  = 100 * E_2015 / P_2015,
    enrol_2024  = 100 * E_2024 / P_2024,
    delta_enrol = enrol_2024 - enrol_2015
  )

# -------------------------------------------------------------
# 3. Rent side — pindex2015 / pindex2024 (lowercase in sheet)
# -------------------------------------------------------------
rent_raw <- read_excel(kid_path, sheet = "Distr_RegionEff_abs_yearly")

required_rent_columns <- c("kid2019", "pindex2015", "pindex2024")
if (!all(required_rent_columns %in% names(rent_raw))) {
  stop("Expected rent columns missing. Check the workbook version and worksheet.")
}

rent_side <- rent_raw %>%
  mutate(kid2019 = normalize_kreis_code(kid2019)) %>%
  transmute(
    kreis_code = kid2019,
    R_2015     = suppressWarnings(as.numeric(pindex2015)),
    R_2024     = suppressWarnings(as.numeric(pindex2024))
  ) %>%
  filter(!is.na(R_2015), !is.na(R_2024), R_2015 > 0)

if (nrow(rent_side) == 0 || anyDuplicated(rent_side$kreis_code)) {
  stop("Rent data must contain one usable row per district.")
}

message("rent_side rows: ", nrow(rent_side),
        " | R_2015 range: ",
        paste(round(range(rent_side$R_2015, na.rm = TRUE), 2), collapse = ".."),
        " | R_2024 range: ",
        paste(round(range(rent_side$R_2024, na.rm = TRUE), 2), collapse = ".."))

# -------------------------------------------------------------
# 4. Merge and compute the two axes
# -------------------------------------------------------------
plot_df <- kreis_wide %>%
  inner_join(rent_side, by = "kreis_code") %>%
  mutate(rent_growth = 100 * (R_2024 / R_2015 - 1)) %>%
  filter(!is.na(rent_growth), !is.na(delta_enrol))

if (nrow(plot_df) == 0) stop("No districts have both endpoint data and rent indices.")
write_excel_csv(plot_df, file.path(out_dir, "scatter_data.csv"), na = "")
write_excel_csv(anti_join(kreis_wide, rent_side, by = "kreis_code"),
                file.path(out_dir, "districts_without_rent.csv"), na = "")
message("Dots in scatter: ", nrow(plot_df))
message("Kreise with universities dropped (still unmatched): ",
        result_patched %>%
          filter(status == "empty", !is.na(studienanfaenger),
                 studienanfaenger > 0) %>%
          distinct(hs) %>% nrow())

# -------------------------------------------------------------
# 5. Scatterplot
# -------------------------------------------------------------
mannheim <- plot_df %>% filter(kreis_code == "08222")

p <- ggplot(plot_df, aes(x = rent_growth, y = delta_enrol)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_point(alpha = 0.55, size = 1.8, colour = "steelblue") +
  geom_point(data = mannheim, colour = "red", size = 3.5) +
  geom_label(data = mannheim,
             aes(label = "Mannheim"),
             nudge_y = 0.4, colour = "red", fill = "white",
             label.size = 0.2, size = 3.2) +
  labs(
    x = "Rent growth 2015\u20132024 (%)",
    y = "Change in entrants per 100 residents aged 18–25 (2015–2024)",
    title    = "Rent growth and student entrants in German university districts",
    subtitle = sprintf("Exploratory matching: %d districts; geography not fully audited", nrow(plot_df)),
    caption  = paste(
      "Population: Destatis 12411-04-02-4 (31.12.).",
      "Entrants: supplied 21311-0011 export, winter semesters. Rent: RWI-GEO-REDX APRENT."
    )
  ) +
  theme_minimal(base_size = 11)

print(p)

ggsave(file.path(out_dir, "scatter_rent_vs_enrolment.png"),
       p, width = 8, height = 6, dpi = 200)

# -------------------------------------------------------------
# 6. Diagnostic — biggest remaining gaps
# -------------------------------------------------------------
miss <- result_patched %>%
  filter(status == "empty", !is.na(studienanfaenger),
         studienanfaenger > 0, jahr == 2024) %>%
  group_by(hs, hochschule) %>%
  summarise(E_2024 = sum(studienanfaenger, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(E_2024))

head(miss, 20)
