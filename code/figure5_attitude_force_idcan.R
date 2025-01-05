# model that predicts if 1 = strong attitude and 0 = weak attitude
# predict by generation

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
         generation = factor(generation))

table(data$iss_souv2)

## Create binary variable where
  ## 0 = 0.25, 0.33, 0.5, 0.66, 0.75
  ## 1 = 0, 1
data$attitude_strength <- NA
data$attitude_strength[data$iss_souv2 %in% c(0, 1)] <- 1
data$attitude_strength[data$iss_souv2 %in% c(0.25, 0.33, 0.5, 0.66, 0.75)] <- 0
table(data$attitude_strength)

# Model ------------------------------------------------------------------

model_data <- data |> 
  select(
    attitude_strength,
    generation, ses_lang.1,
    ses_gender,
    ses_family_income_centile_cat,
    ses_origin_from_canada.1,
    ses_educ, iss_idcan
  )

model <- glm(
  attitude_strength ~ generation * iss_idcan + .,
  data = model_data,
  family = binomial()
)

# Graph ------------------------------------------------------------------

preds <- marginaleffects::predictions(
  model = model,
  type = "response",
  newdata = marginaleffects::datagrid(
    model = model,
    generation = c("preboomer", "boomer", "x", "y", "z"),
    iss_idcan = c(0, 1)
  )
) |> 
  mutate(
    generation = factor(
      generation,
      levels = c("preboomer", "boomer", "x", "y", "z"),
      labels = c("Preboomer", "Boomer", "X", "Y", "Z")
    ),
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high),
    iss_idcan = factor(
      iss_idcan,
      levels = c(0, 1),
      labels = c("Quebecois\nbefore", "Canadian\nbefore")
    )
  )

ggplot(
  preds,
  aes(
    x = generation,
    y = estimate,
    group = iss_idcan,
    shape = iss_idcan
  )) +
    geom_linerange(
      aes(ymin = conf.low, ymax = conf.high),
      position = position_dodge(width = 0.2),
      color = "grey30"
    ) +
    geom_point(
      position = position_dodge(width = 0.2),
      fill = "black", size = 3
    ) +
    scale_shape_manual(
      values = c(
        "Quebecois\nbefore" = 23,
        "Canadian\nbefore" = 22
      )
    ) +
    clessnize::theme_clean_light() +
    xlab("") +
    labs(
      caption = "Estimated average marginal effects of generation and language on the predicted attitude strength\non the independentist scale, controlling for all other variables."
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1)),
      name = "Probability of Strong Attitude\non Independentist Scale\n"
    ) +
    guides(color = "none") +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.2, color = "grey90"),
      plot.caption = element_text(hjust = 1)
    )

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure5_attitude_strength_idcan.png",
  width = 9, height = 6
)
