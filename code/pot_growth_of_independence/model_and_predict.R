# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  filter(year >= 2021 &
           source_id != "pes_elxn_2022_text") %>% 
  mutate(yob = year - ses_age,
         generation = case_when(
           yob <= 1947 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation),
         region = case_when(
           ses_geoloc.1 == "montreal" ~ "mtl",
           ses_geoloc.1 == "region" ~ "region",
           ses_geoloc.1 == "quebec" ~ "qc",
           ses_geoloc.1 == "suburbs" ~ "rmr"
         ),
         origin = case_when(
           ses_origin_from_canada.1 == 1 ~ "from_canada",
           ses_origin_from_canada.1 == 0 ~ "not_from_canada"
           )
         ) %>% 
  select(iss_souv = iss_souv2,
         generation,
         region,
         gender = ses_gender,
         langue = ses_lang.1,
         origin,
         income = ses_family_income_centile_cat) %>% 
  tidyr::drop_na()

Strat <- readRDS("SharedFolder_spsa_article_nationalisme/data/census/poststrat_large.rds") %>% 
  mutate(region = ifelse(large_region == "other", "region", large_region),
         gender = ifelse(gender == "men", "male", "female"))


# Model -------------------------------------------------------------------

model <- lm(iss_souv ~ . + generation * region * langue,
            data = Data)
summary(model)

marginaleffects::predictions(model, by = "generation")


# Predict -----------------------------------------------------------------

Preds <- marginaleffects::predictions(model, newdata = Strat)

# Aggregate -------------------------------------------------------------------------

PredsAgg <- Preds %>% 
  mutate(weighted_estimate = estimate * prct_region,
         weighted_stderr = std.error^2 * prct_region^2) %>% 
  group_by(region, generation, langue) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct_region),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct_region)^2 )) %>% 
  mutate(margin_error_95 = 1.96 * weighted_stderr,
         conf_low = weighted_mean_estimate - margin_error_95,
         conf_high = weighted_mean_estimate + margin_error_95,
         generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z")),
         conf_low = ifelse(conf_low < 0, 0, conf_low),
         conf_high = ifelse(conf_high > 1, 1, conf_high),
         weighted_mean_estimate = ifelse(weighted_mean_estimate < 0, 0, weighted_mean_estimate),
         weighted_mean_estimate = ifelse(weighted_mean_estimate > 1, 1, weighted_mean_estimate),
         langue = factor(langue, levels = c("french", "english", "other")))

# Graphs ------------------------------------------------------------------

common_theme <- function() {
  list(
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey"),
    geom_point(position = position_dodge2(width = 0.4)),
    geom_linerange(aes(ymin = conf_low, ymax = conf_high), position = position_dodge2(width = 0.4)),
    scale_color_grey(),
    scale_y_continuous(limits = c(0, 1)),
    ylab("Predicted mean position\non independence scale"),
    clessnverse::theme_clean_light(),
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          panel.background = element_rect(fill = NA, color = "lightgrey"))
  )
}

## aggregate
ggplot(PredsAgg,
       aes(x = region,
           y = weighted_mean_estimate,
           color = langue)) +
  facet_wrap(~ generation,
             nrow = 1) +
  common_theme()

ggsave("SharedFolder_spsa_article_nationalisme/graphs/models/potgrowth/aggregate/facet_generation.png",
       width = 8, height = 4.5)

ggplot(PredsAgg,
       aes(x = generation,
           y = weighted_mean_estimate,
           color = langue)) +
  facet_wrap(~ region,
             nrow = 1) +
  common_theme()

ggsave("SharedFolder_spsa_article_nationalisme/graphs/models/potgrowth/aggregate/facet_region.png",
       width = 8, height = 4.5)

ggplot(PredsAgg,
       aes(x = generation,
           y = weighted_mean_estimate,
           color = region)) +
  facet_wrap(~ langue,
             nrow = 1) +
  common_theme()

ggsave("SharedFolder_spsa_article_nationalisme/graphs/models/potgrowth/aggregate/facet_langue.png",
       width = 8, height = 4.5)




### Distribution estimée (en utilisant les marges d'erreur)
