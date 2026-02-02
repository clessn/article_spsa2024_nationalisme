# model that predicts if 1 = strong attitude and 0 = weak attitude
# predict by generation

# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(survey)  # for svyglm with proper weighting and clustering
library(emmeans)  # for predicted values from svyglm

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds") %>%
  filter(year >= 2021) |>
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
  mutate(
    year_f = factor(year)
  ) |>
  select(
    attitude_strength,
    generation, ses_lang.1,
    ses_gender,
    ses_family_income_centile_cat,
    ses_origin_from_canada.1,
    ses_educ,
    year_f,
    weight_trimmed
  ) |>
  tidyr::drop_na()

# Create survey design with clustering by year
des <- svydesign(
  ids = ~year_f,  # cluster by survey year
  weights = ~weight_trimmed,
  data = model_data
)

model <- svyglm(
  attitude_strength ~ generation * ses_lang.1 +
    ses_gender + ses_family_income_centile_cat +
    ses_origin_from_canada.1 + ses_educ +
    year_f,
  design = des,
  family = quasibinomial()  # for binary outcome with survey weights
)

n_obs <- nobs(model)

# Graph ------------------------------------------------------------------

# Use emmeans for predictions (works with svyglm)
# type = "response" gives probabilities for quasibinomial
em <- emmeans(model, ~ generation | ses_lang.1,
              at = list(
                year_f = levels(model_data$year_f)[1]
              ),
              nuisance = c("ses_gender", "ses_family_income_centile_cat", "ses_educ"),
              type = "response")

# Extract as dataframe and calculate CIs manually (svyglm gives negative df)
preds <- as.data.frame(summary(em))
n <- nrow(model_data)
k <- length(coef(model))
df_error <- max(n - k, 1)

# For response scale, use delta method approximation for CIs
# emmeans returns prob and SE on probability scale when type="response"
preds$conf.low <- preds$prob - qt(0.975, df_error) * preds$SE
preds$conf.high <- preds$prob + qt(0.975, df_error) * preds$SE

# Rename for consistency
preds$estimate <- preds$prob

preds <- preds |>
  mutate(
    generation = factor(
      generation,
      levels = c("preboomer", "boomer", "x", "y", "z"),
      labels = c("Preboomer", "Boomer", "X", "Y", "Z")
    ),
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high),
    ses_lang.1 = factor(
      ses_lang.1,
      levels = c("french", "english", "other"),
      labels = c("French", "English", "Other")
    )
  )

ggplot(preds, aes(x = ses_lang.1, y = estimate, color = ses_lang.1)) +
    facet_wrap(~generation, nrow = 1) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
    geom_point() +
    scale_color_manual(
      values = c(
        "French" = "black",
        "English" = "grey30",
        "Other" = "grey60"
      )
    ) +
    clessnize::theme_clean_light() +
    xlab("") +
    labs(
      caption = paste0("Predicted probability of having an extreme position on the independence scale with interaction between generation and language\nwhile controlling for other socio-demographic variables, holding them constant. Data from 2021 to 2023, n = ", n_obs, ".")
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1)),
      expand = c(0, 0),
      name = "Probability of Extreme Attitude\non Independentist Scale\n"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    guides(color = "none") +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.2, color = "grey90"),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5),
      strip.text.x = element_text(size = 12),
      panel.background = element_rect(fill = NA, color = "grey75"),
      plot.caption = element_text(hjust = 1)
    )

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure4_attitude_strength_by_generation.png",
  width = 9, height = 6
)
