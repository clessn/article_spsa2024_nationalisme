# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(yob = year - ses_age,
         generation = case_when(
           yob %in% 1925:1946 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation))

# Years and response scales -----------------------------------------------
years_to_keep <- c(1974, 1979, 1984, 1988, 1993, 1997, 2000,
                   2004, 2008, 2015, 2019, 2021, 2022)

response_scales <- c(

  `1974` = "dichotomous",
  `1979` = "point4",
  `1984` = "dichotomous",
  `1988` = "point4",
  `1993` = "point4",
  `1997` = "point4",
  `2000` = "point4",
  `2004` = "point4",
  `2008` = "point4",
  `2015` = "point4",
  `2019` = "point4",
  `2021` = "point4",
  `2022` = "point4"
)

# Calculate descriptive stats by year and generation ----------------------

## Create unified sovereignty variable
Data <- Data %>%
  mutate(
    # Use iss_souv for dichotomous years, iss_souv2 for likert
    souv_unified = case_when(
      year %in% c(1974, 1984) ~ iss_souv,
      TRUE ~ iss_souv2
    )
  )

## Available generations per year (only include when members are 18+)
## Same as in figure1_evolution_past_article.R
available_generations <- list(
  "1974" = c("preboomer", "boomer"),
  "1979" = c("preboomer", "boomer"),
  "1984" = c("preboomer", "boomer", "x"),
  "1988" = c("preboomer", "boomer", "x"),
  "1993" = c("preboomer", "boomer", "x"),
  "1997" = c("preboomer", "boomer", "x"),
  "2000" = c("preboomer", "boomer", "x"),
  "2004" = c("preboomer", "boomer", "x", "y"),
  "2008" = c("preboomer", "boomer", "x", "y"),
  "2015" = c("preboomer", "boomer", "x", "y", "z"),
  "2019" = c("preboomer", "boomer", "x", "y", "z"),
  "2021" = c("preboomer", "boomer", "x", "y", "z"),
  "2022" = c("preboomer", "boomer", "x", "y", "z")
)

## Calculate means and 95% CI by year and generation
for (i in 1:length(years_to_keep)) {
  yeari <- years_to_keep[i]
  statsi <- Data %>%
    filter(year == yeari,
           !is.na(souv_unified),
           generation %in% available_generations[[as.character(yeari)]]) %>%
    group_by(generation) %>%
    summarise(
      n = n(),
      estimate = mean(souv_unified, na.rm = TRUE),
      sd = sd(souv_unified, na.rm = TRUE),
      se = sd / sqrt(n),
      conf.low = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se,
      .groups = "drop"
    ) %>%
    mutate(year = yeari,
           response_scale = response_scales[as.character(yeari)])
  if (i == 1) {
    graphsData <- statsi
  } else {
    graphsData <- bind_rows(graphsData, statsi)
  }
  message(yeari)
}

## Check sample sizes
table(graphsData$year, graphsData$generation)

# Total n for caption (same filtering logic as graphsData)
total_n <- sum(graphsData$n)

# Graphs ------------------------------------------------------------------

## 1. Generate auxiliary data ----------------------------------------------

### important events
events_df <- data.frame(
  year = c(1980, 1990, 1995),
  label = c("Referendum", "Meech\nFailure", "Referendum")
)

### generations labels
generations <- c("boomer", "preboomer", "x", "y", "z")
generations_labels <- c("Boomer", "Pre-boomer", "X", "Y", "Z")
names(generations_labels) <- generations

### generations birth year ranges (for age labels)
birth_year_range <- list(
  "preboomer" = c(1925, 1946),
  "boomer" = c(1947, 1961),
  "x" = c(1962, 1976),
  "y" = c(1977, 1991),
  "z" = c(1992, 2003)
)

calculate_age_range <- function(year, birth_start, birth_end) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste0(age_start, "-", age_end))
}

# 2. Make one graph by generation ----------------------------------------

generation_plot <- function(gen_highlight){
  graph_data <- as.data.frame(graphsData) |>
    mutate(highlight = ifelse(generation == gen_highlight, "yes", "no"))
  lab_y <- ifelse(gen_highlight == "preboomer", "Mean Position on\nIndependentist Scale", "\n")
  # Add age labels
  break_start <- min(graph_data$year[graph_data$generation == gen_highlight])
  break_start <- round(break_start / 5) * 5
  break_years <- seq(from = break_start, to = 2020, by = 5)
  plot <- ggplot(
    data = graph_data,
    aes(
      x = year, y = estimate
    )) +
    geom_vline(data = events_df, aes(xintercept = year),
               linetype = "dashed", color = "grey40") +
    geom_text(data = events_df, aes(label = label, x = year + 0.5),
              y = 1.4, angle = 90, hjust = 1, vjust = 1,
              color = "grey40", lineheight = 0.75, size = 3.5) +
    geom_ribbon(
      data = graph_data |> filter(highlight == "yes"),
      aes(
        ymin = conf.low,
        ymax = conf.high),
      alpha = 0.35, fill = "gray"
    ) +
    geom_line(
      data = graph_data |> filter(highlight == "no"),
      aes(group = generation),
      linewidth = 0.25, color = "grey70"
    ) +
    geom_line(
      data = graph_data |> filter(highlight == "yes"),
      color = "black", linewidth = 1
    ) +
    geom_point(
      data = graph_data |> filter(highlight == "yes"),
      color = "black",
      size = 2.75,
      aes(shape = response_scale),
      show.legend = FALSE
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.2, 0.5, 1.3),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    labs(
      x = "",
      y = lab_y,
      title = generations_labels[gen_highlight]
    ) +
    scale_shape_manual(
      values = c("dichotomous" = 16, "point4" = 18),
      labels = c(
        "dichotomous" = "Dichotomous",
        "point4" = "Likert")
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 20))
  for (year in break_years) {
    if (year - birth_year_range[[gen_highlight]][1] > 0){
      age_label <- calculate_age_range(year, birth_year_range[[gen_highlight]][1], birth_year_range[[gen_highlight]][2])
      plot <- plot +
        annotate(geom = "point", size = 4, shape = 15, color = "white", y = -0.3, x = year - 1) +
        annotate(geom = "point", size = 4, shape = 15, color = "white", y = -0.3, x = year) +
        annotate(geom = "point", size = 4, shape = 15, color = "white", y = -0.3, x = year + 1) +
        annotate("text", x = year, y = -0.3, label = age_label, vjust = 0.5, hjust = 0.5, size = 2.5)
    }
  }
  return(plot)
}

# 3. Patchwork it all together -------------------------------------------

design <- c(
  area(1, 1, 1, 4),
  area(1, 5, 1, 8),
  area(1, 9, 1, 13),
  area(2, 2, 2, 5),
  area(2, 8, 2, 11)
)

generation_plot("preboomer") +
  generation_plot("boomer") +
  generation_plot("x") +
  generation_plot("y") +
  generation_plot("z") +
  plot_layout(design = design) +
  plot_annotation(
    caption = paste0("Mean support for Quebec independence by generation with 95% confidence intervals.\nDiamonds indicate Likert scale questions; circles indicate dichotomous questions. n = ", total_n)
  ) &
  theme(plot.caption = element_text(family = "Roboto"))

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure1_descriptive.png",
  width = 15, height = 8
)
