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

Preds <- marginaleffects::predictions(model, newdata = Strat) %>% 
  mutate(langue = factor(langue, levels = c("french", "english", "other")))

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
         weighted_mean_estimate = ifelse(weighted_mean_estimate > 1, 1, weighted_mean_estimate))

# Graphs ------------------------------------------------------------------

common_theme <- function() {
  list(
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey"),
    geom_point(position = position_dodge(width = 0.4)),
    geom_linerange(aes(ymin = conf_low, ymax = conf_high), position = position_dodge2(width = 0.4)),
    scale_color_grey(),
    scale_y_continuous(limits = c(0, 1),
                       expand = c(0,0),
                       breaks = c(0.13, 0.5, 0.87),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")),
    ylab("\nWeighted average of predicted\npositions on independence scale\n"),
    clessnverse::theme_clean_light(),
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title.y = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.background = element_rect(fill = NA, color = "lightgrey"))
  )
}



## Aggregate ---------------------------------------------------------------

PredsAgg %>%
  mutate(generation = case_when(
    generation == "preboomer" ~ "Pre-boomer\n77-99 years old",
    generation == "boomer" ~ "Boomer\n63-76 years old",
    generation == "x" ~ "X's\n48-62 years old",
    generation == "y" ~ "Y's\n33-47 years old",
    generation == "z" ~ "Z's\n18-32 years old"
  ),
  generation = factor(generation, levels = c("Pre-boomer\n77-99 years old",
                                             "Boomer\n63-76 years old",
                                             "X's\n48-62 years old",
                                             "Y's\n33-47 years old",
                                             "Z's\n18-32 years old")),
  langue = case_when(
    langue == "french" ~ "Francophone",
    langue == "english" ~ "Anglophone",
    langue == "other" ~ "Allophone"
  ),
  langue = factor(langue, levels = c("Francophone", "Anglophone", "Allophone")),
  region = case_when(
    region == "mtl" ~ "Montreal",
    region == "rmr" ~ "Greater\nMontreal Area",
    region == "qc" ~ "Quebec City",
    region == "region" ~ "Regions"
  ),
  region = factor(region, levels = c("Montreal", "Quebec City",
                                     "Regions", "Greater\nMontreal Area"))) %>% 
  ggplot(aes(x = region,
           y = weighted_mean_estimate,
           color = langue)) +
  facet_wrap(~ generation,
             nrow = 1) +
  xlab("") +
  labs(title = "Current Attitudes on Quebec Sovereignty",
       subtitle = "By Region, Generation and Language Group",
       caption = "Survey data from 2021 to 2023, n = 6687. The Y-axis shows the region-wise weighted average of\nattitudes towards Quebec sovereignty based on a linear model and 2021 census data.") +
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



## Disaggregated -----------------------------------------------------------

Preds %>% 
  ggplot(aes(x = region, y = estimate)) +
  facet_grid(rows = vars(langue),
             cols = vars(generation)) +
  geom_point(aes(size = prct_region,
                 alpha = prct_region)) +
  clessnverse::theme_clean_light()


# Distributions estimées --------------------------------------------------

for (i in 1:nrow(Preds)){
  mean <- Preds$estimate[i]
  sd <- Preds$std.error[i]
  n <- Preds$n[i]
  if (n != 0){
    souvi <- rnorm(mean = mean, sd = sd, n = n)
    souvi[souvi < 0] <- 0
    souvi[souvi > 1] <- 1
    Simi <- data.frame(
      id = i,
      pred_souv = souvi,
      generation = Preds$generation[i],
      region = Preds$region[i],
      langue = Preds$langue[i],
      origin = Preds$origin[i],
      income = Preds$income[i],
      gender = Preds$gender[i]
    )
    if (i == 1){
      Sims <- list()
      Sims[[i]] <- Simi
    } else {
      Sims[[i]] <- Simi
    }
    if (i %% 50 == 0){
      message(i)
    }
  }
}

df <- bind_rows(Sims)

ggplot(df, aes(x = pred_souv, y = generation)) +
  ggridges::geom_density_ridges() +
  facet_grid(rows = vars(region),
             cols = vars(langue))









