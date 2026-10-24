# Compare citywide room-size shares in end-2024 stock and 2025 completions.

source("scripts/00_helpers.R")
check_packages(c("dplyr", "readr", "tidyr", "ggplot2", "scales"))
library(dplyr)
library(ggplot2)

room_columns <- c("w1", "w2", "w3", "w4", "w5", "w6", "w7plus")
categories <- c("1 room", "2 rooms", "3 rooms", "4 rooms", "5 rooms", "6 rooms", "7+ rooms")
stock <- read_areas("dwellings_2024.csv", c("total", room_columns))
stopifnot(all(rowSums(as.data.frame(stock[, room_columns])) == stock$total))
stock_counts <- colSums(as.data.frame(stock[, room_columns]))

completions <- readr::read_csv(file.path(data_dir, "completions_2025.csv"), show_col_types = FALSE)
stopifnot(nrow(completions) == 7L, !anyDuplicated(completions$category),
          setequal(categories, completions$category),
          all(is.finite(completions$completions_count)),
          all(completions$completions_count >= 0))

plot_data <- data.frame(category = categories, stock_count = as.numeric(stock_counts)) |>
  left_join(completions, by = "category") |>
  mutate(
    stock_share = 100 * stock_count / sum(stock_count),
    completion_share = 100 * completions_count / sum(completions_count),
    category = factor(category, levels = rev(categories))
  )

long <- plot_data |>
  select(category, stock_share, completion_share) |>
  tidyr::pivot_longer(c(stock_share, completion_share), names_to = "type", values_to = "share") |>
  mutate(type = if_else(type == "stock_share", "Stock, end-2024", "Completions, 2025"))

plot <- ggplot() +
  geom_segment(data = plot_data, aes(x = stock_share, xend = completion_share,
                                    y = category, yend = category),
               colour = "#d8dfe5", linewidth = 3, lineend = "round") +
  geom_point(data = long, aes(share, category, colour = type), size = 4) +
  geom_text(data = plot_data, aes(stock_share, category, label = sprintf("%.1f%%", stock_share)),
            vjust = 2, size = 3.3, colour = "#62747d") +
  geom_text(data = plot_data, aes(completion_share, category, label = sprintf("%.1f%%", completion_share)),
            vjust = -0.9, size = 3.3, colour = "#216d93") +
  scale_colour_manual(values = c("Stock, end-2024" = "#8797a0", "Completions, 2025" = "#277da8")) +
  scale_x_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 40)) +
  labs(title = "What Mannheim is adding to its housing stock",
       subtitle = "Room-size mix: existing stock versus completed dwellings",
       x = "Share of dwellings", y = NULL, colour = NULL,
       caption = paste("Source: City of Mannheim. Rooms include kitchens.",
                       "Completions include work on existing buildings and exclude demolitions; they are not net stock growth.",
                       sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8))

ggsave(file.path(figure_dir, "04_stock_vs_completions.png"), plot,
       width = 11, height = 6.7, dpi = 300, bg = "white")
