# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  filter(
    year >= 2021 &
    source_id %in% c("january", "february", "march", "april", "may", "june")
  ) |> 
  mutate(yob = year - ses_age,
         generation = case_when(
           yob %in% 1925:1946 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation)) |>
  tidyr::drop_na(generation, iss_idcan)

data |> 
  group_by(generation, iss_idcan) |> 
  summarise(
    n = n()
  ) |> 
  group_by(generation) |> 
  mutate(
    total = sum(n),
    prop = n / total,
    margin_error = 1.96 * sqrt((prop * (1 - prop)) / total),
    ci_lower = prop - margin_error,
    ci_upper = prop + margin_error,
    generation = factor(
      generation,
      levels = rev(c("preboomer", "boomer", "x", "y", "z")),
      labels = rev(c("Preboomer", "Boomer", "X", "Y", "Z"))
    )
  ) |> 
  filter(iss_idcan == 0) |> 
  ggplot(
    aes(x = prop, y = generation)
  ) +
  geom_point() +
  geom_linerange(
    aes(xmin = ci_lower, xmax = ci_upper),
    linewidth = 0.1
  ) +
  clessnize::theme_clean_light() +
  scale_x_continuous(
    limits = c(0.45, 0.75),
    breaks = seq(0, 100, by = 10)/100,
    labels = scales::percent(seq(0, 100, by = 10)/100),
    name = "\nProportion of respondents identifying as Québécois before Canadian\n"
  ) +
  ylab("") +
  labs(caption = paste0("Proportion of respondents identifying as Québécois before Canadian, by generation,\nwith 95% confidence intervals. Data from 2022, n = ", nrow(data), ".")) +
  geom_text(
    aes(
      x = prop,
      label = paste0(round(prop * 100), "%")
    ),
    vjust = -0.65
  ) +
  theme(
    panel.grid.major.x = element_line(linewidth = 0.2, color = "grey90"),
    panel.grid.major.y = element_line(linewidth = 0.2, color = "grey90"),
    axis.text.y = element_text(size = 12),
    plot.caption = element_text(hjust = 1)
  )

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure4_id_qc_can.png",
  width = 8, height = 5.5
)
