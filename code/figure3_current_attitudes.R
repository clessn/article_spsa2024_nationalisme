# show how language is not the same predictor among different generations

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
table(data$iss_souv2, data$year)
### Selon le sondage, on a 4 ou 5 niveaux likert
### On va donc tester 2 lm et les comparer:
#### 1. en assignant de façon random les neutres à une position nuancée
#### 2. en faisant une lm sur la variable raw

## VD 1
set.seed(123)
data$vd1 <- data$iss_souv2
data$vd1 <- ifelse(data$vd1 == 0.25, 0.33,
                   ifelse(data$vd1 == 0.75, 0.66, data$vd1))
data$vd1[!is.na(data$vd1) & data$vd1 == 0.5] <- sample(
  c(0.33, 0.66),
  size = sum(!is.na(data$vd1) & data$vd1 == 0.5),
  replace = TRUE
)
table(data$vd1)

## VD 2
data$vd2 <- data$iss_souv2
data$vd2[data$vd2 == 0.33] <- 0.25
data$vd2[data$vd2 == 0.66] <- 0.75
table(data$vd2)

# Functions --------------------------------------------------------------

create_model <- function(
  data,
  vd
){
  data$vd <- data[[vd]]
  model_data <- data |>
    mutate(
      year_f = factor(year)
    ) |>
    select(
      vd,
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
    vd ~ generation * ses_lang.1 +
      ses_gender + ses_family_income_centile_cat +
      ses_origin_from_canada.1 + ses_educ +
      year_f,
    design = des
  )
  return(list(model = model, model_data = model_data))
}

create_figure2 <- function(
  data, vd
){
  result <- create_model(
    data, vd
  )
  model <- result$model
  model_data <- result$model_data
  n_obs <- nobs(model)
  print(n_obs)

  # Use emmeans for predictions (works with svyglm)
  # Fix reference levels for nuisance factors
  em <- emmeans(model, ~ generation | ses_lang.1,
                at = list(
                  year_f = levels(model_data$year_f)[1]
                ),
                nuisance = c("ses_gender", "ses_family_income_centile_cat", "ses_educ"))

  # Extract as dataframe and calculate CIs manually (svyglm gives negative df)
  preds <- as.data.frame(summary(em))
  n <- nrow(model_data)
  k <- length(coef(model))
  df_error <- max(n - k, 1)
  preds$conf.low <- preds$emmean - qt(0.975, df_error) * preds$SE
  preds$conf.high <- preds$emmean + qt(0.975, df_error) * preds$SE

  # Rename for consistency
  preds$estimate <- preds$emmean

  preds <- preds |>
    mutate(
      generation = factor(
        generation,
        levels = c("preboomer", "boomer", "x", "y", "z"),
        labels = c("Preboomer", "Boomer", "X", "Y", "Z")
      ),
      ses_lang.1 = factor(
        ses_lang.1,
        levels = c("french", "english", "other"),
        labels = c("French", "English", "Other")
      )
    )

  print(as.data.frame(preds))

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
      caption = paste0("Predicted position on the independence scale with interaction between generation and language\nwhile controlling for other socio-demographic variables, holding them constant. Data from 2021 to 2023, n = ", n_obs, ".")
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      expand = c(0, 0),
      name = "Predicted Position on\nIndependence Scale\n"
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    annotate(
      geom = "text",
      x = 0.725, y = 0.99,
      label = "More\nSeparatist",
      hjust = 1, angle = 90,
      size = 2.25, lineheight = 0.75
    ) +
    annotate(
      geom = "text",
      x = 0.725, y = 0.01,
      label = "More\nFederalist",
      hjust = 0, angle = 90,
      size = 2.25, lineheight = 0.75
    ) +
    guides(color = "none") +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.2, color = "grey90"),
      #axis.text.y = element_text(angle = 90, hjust = 0.5),
      axis.text.y = element_blank(),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5),
      strip.text.x = element_text(size = 12),
      panel.background = element_rect(fill = NA, color = "grey75"),
      plot.caption = element_text(hjust = 1)
    )
}

# Graph and save -------------------------------------------------------------------

create_figure2(data, "vd1")
ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure3_language_effect_by_generation_4point.png",
  width = 9, height = 6
)

create_figure2(data, "vd2")
ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure3_language_effect_by_generation_5point.png",
  width = 9, height = 6
)
