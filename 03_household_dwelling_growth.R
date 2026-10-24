# Percentage changes in household types and dwelling sizes, 2024 to 2025.

source("scripts/00_helpers.R")
check_packages(c("dplyr", "readr", "ggplot2", "ggrepel", "patchwork"))
library(dplyr)
library(ggplot2)

hh24 <- read_areas("households_2024.csv", c("single_hh", "multi_with_kids")) |>
  select(code, name, singles24 = single_hh, families24 = multi_with_kids)
hh25 <- read_areas("households_2025.csv", c("single_hh", "multi_with_kids")) |>
  select(code, singles25 = single_hh, families25 = multi_with_kids)

housing_counts <- function(filename, suffix) {
  result <- read_areas(filename, c("w1", "w2", "w4", "w5", "w6", "w7plus")) |>
    transmute(code, large = w4 + w5 + w6 + w7plus, small = w1 + w2)
  names(result)[2:3] <- paste0(c("large", "small"), suffix)
  result
}

growth <- join_areas(hh24, hh25) |>
  join_areas(housing_counts("dwellings_2024.csv", "24")) |>
  join_areas(housing_counts("dwellings_2025.csv", "25")) |>
  mutate(
    families = percentage_growth(families25, families24),
    singles = percentage_growth(singles25, singles24),
    large = percentage_growth(large25, large24),
    small = percentage_growth(small25, small24)
  )

growth_panel <- function(x, y, title, x_label, y_label, colour, label_codes) {
  labelled <- growth |> filter(code %in% label_codes)
  ggplot(growth, aes(x = .data[[x]], y = .data[[y]])) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "grey75") +
    geom_point(colour = colour, size = 2.8, alpha = 0.85) +
    ggrepel::geom_text_repel(data = labelled, aes(label = name), size = 3,
                            seed = 42, max.overlaps = Inf) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 13, face = "bold"), panel.grid.minor = element_blank())
}

families <- growth_panel(
  "families", "large", "Families and larger dwellings",
  "Households with children: growth (%)", "Dwellings with 4+ rooms: growth (%)", "#277da8",
  c("124", "125", "062", "123")
)
singles <- growth_panel(
  "singles", "small", "One-person households and smaller dwellings",
  "One-person households: growth (%)", "Dwellings with 1-2 rooms: growth (%)", "#c9514e",
  c("124", "125", "062", "123", "110", "072")
)

combined <- patchwork::wrap_plots(families, singles) +
  patchwork::plot_annotation(
    title = "Household and dwelling growth in Mannheim, 2024-2025",
    subtitle = "All 38 Stadtteile. Selected neighbourhoods labelled; dotted line denotes equal percentage growth.",
    caption = "Source: City of Mannheim, year-end household and dwelling-stock tables. Rooms include kitchens.",
    theme = theme(plot.title = element_text(size = 18, face = "bold"),
                  plot.caption = element_text(hjust = 0, size = 9))
  )
ggsave(file.path(figure_dir, "03_household_dwelling_growth.png"), combined,
       width = 14, height = 6, dpi = 300, bg = "white")
