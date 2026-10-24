# Two bivariate maps, using neighbourhood thirds on each axis.
# Adapted from the project's household/dwelling and young-adult/dwelling scripts.

source("scripts/00_helpers.R")
check_packages(c("sf", "dplyr", "readr", "ggplot2", "patchwork"))
library(dplyr)
library(ggplot2)

households <- read_areas("households_2025.csv", c("total_hh", "multi_with_kids"))
age <- read_areas("age_2025.csv", c("total", "a18_25", "a25_30"))
dwellings <- read_areas("dwellings_2025.csv", c("total", "w1", "w2", "w4", "w5", "w6", "w7plus"))

stopifnot(all(households$total_hh > 0), all(age$total > 0), all(dwellings$total > 0))

profiles <- join_areas(
  households |> select(code, name, total_hh, multi_with_kids),
  age |> select(code, population = total, a18_25, a25_30)
) |>
  join_areas(dwellings |> select(-name)) |>
  mutate(
    family_share = 100 * multi_with_kids / total_hh,
    young_share = 100 * (a18_25 + a25_30) / population,
    large_share = 100 * (w4 + w5 + w6 + w7plus) / total,
    small_share = 100 * (w1 + w2) / total
  )

geometry <- sf::st_read(file.path(data_dir, "neighbourhoods.geojson"), quiet = TRUE) |>
  mutate(code = as.character(code))
stopifnot(nrow(geometry) == 38L, !anyDuplicated(geometry$code),
          setequal(geometry$code, profiles$code))

palette <- matrix(c(
  "#e8e8e8", "#ace4e4", "#5ac8c8",
  "#dfb0d6", "#a5add3", "#5698b9",
  "#be64ac", "#8c62aa", "#3b4994"
), nrow = 3, byrow = TRUE)

thirds <- function(values) {
  breaks <- quantile(values, probs = seq(0, 1, length.out = 4), names = FALSE, type = 7)
  if (length(unique(breaks)) != 4L) stop("Quantile breaks are not distinct.")
  as.integer(cut(values, breaks = breaks, include.lowest = TRUE, labels = FALSE))
}

make_map <- function(x, y, title, subtitle, x_label, y_label, output) {
  classified <- profiles |>
    mutate(
      x_class = thirds(.data[[x]]),
      y_class = thirds(.data[[y]]),
      colour = palette[cbind(y_class, x_class)]
    )
  mapped <- geometry |>
    left_join(classified |> select(code, colour), by = "code")
  stopifnot(!anyNA(mapped$colour))

  map_plot <- ggplot(mapped) +
    geom_sf(aes(fill = colour), colour = "white", linewidth = 0.25) +
    scale_fill_identity() +
    coord_sf(datum = NA) +
    theme_void()

  legend_data <- expand.grid(x = 1:3, y = 1:3)
  legend_data$colour <- palette[cbind(legend_data$y, legend_data$x)]
  legend_plot <- ggplot(legend_data, aes(x, y, fill = colour)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_identity() +
    scale_x_continuous(breaks = 1:3, labels = c("Low", "Mid", "High")) +
    scale_y_continuous(breaks = 1:3, labels = c("Low", "Mid", "High")) +
    coord_fixed() +
    labs(title = "Neighbourhood thirds", x = x_label, y = y_label) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 11, face = "bold"))

  combined <- patchwork::wrap_plots(map_plot, legend_plot, widths = c(3.2, 1.3)) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = "38 Stadtteile; 31 December 2025. Source: City of Mannheim. Each axis uses three quantile groups.",
      theme = theme(plot.title = element_text(size = 19, face = "bold"),
                    plot.subtitle = element_text(size = 11),
                    plot.caption = element_text(hjust = 0, size = 9),
                    plot.background = element_rect(fill = "white", colour = NA))
    )
  ggsave(file.path(figure_dir, output), combined, width = 12, height = 9, dpi = 300, bg = "white")
}

make_map("family_share", "large_share", "Mannheim: families and larger dwellings",
         "Households with children and dwellings with 4+ rooms",
         "Households with children", "Dwellings with 4+ rooms",
         "01_families_large_dwellings.png")

make_map("young_share", "small_share", "Mannheim: young adults and smaller dwellings",
         "Residents aged 18 to under 30 and dwellings with 1-2 rooms",
         "Residents aged 18-29", "Dwellings with 1-2 rooms",
         "02_young_adults_small_dwellings.png")
