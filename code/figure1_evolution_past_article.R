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

table(Data$generation)
table(Data$ses_geoloc.1)


# Train models ------------------------------------------------------------

### years to keep with their names being 1 if binary question only and 2 if likert
years_to_keep <- c("1" = 1974,
                   "2" = 1979,
                   "1" = 1984,
                   "2" = 1988,
                   "2" = 1993,
                   "2" = 2004,
                   "2" = 2008,
                   "2" = 2015,
                   "2" = 2019,
                   "2" = 2021,
                   "2" = 2022)

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

for (i in 1:length(years_to_keep)){
  yeari <- years_to_keep[i]
  if (names(years_to_keep[years_to_keep == yeari]) == "1"){ ## code to execute if only binary question available
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv,
                     generation,
                     int_pol,
                     ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_origin_from_canada.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv ~ generation * ses_lang.1 + ses_geoloc.1 +
                  ses_gender + ses_family_income_centile_cat +
                  ses_origin_from_canada.1 + int_pol,
                data = mdata)
  } else if (yeari == 1993){ ## exception for 1993. GO HABS GO
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv2, generation, int_pol,ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv2 ~ generation * ses_lang.1 + ses_geoloc.1 +
                  ses_gender + ses_family_income_centile_cat + int_pol,
                data = mdata)
  } else if (yeari == 2023){ ## exception for 2023
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      ## use education as proxy for political interest
      mutate(int_pol = as.numeric(as.character(ses_educ))) %>% 
      tidyr::drop_na(iss_souv2,
                     generation,
                     int_pol,
                     ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_origin_from_canada.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv2 ~ generation * ses_lang.1 + ses_geoloc.1 +
                  ses_gender + ses_family_income_centile_cat +
                  ses_origin_from_canada.1 + int_pol,
                data = mdata)
  } else { ## for the rest
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv2,
                     generation,
                     int_pol,
                     ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_origin_from_canada.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv2 ~ generation * ses_lang.1 + ses_geoloc.1 +
                  ses_gender + ses_family_income_centile_cat +
                  ses_origin_from_canada.1 + int_pol,
                data = mdata)
  }
  if (i == 1){
    linear_models <- list()
    linear_models[[as.character(yeari)]] <- model
  } else {
    linear_models[[as.character(yeari)]] <- model
  }
  message(yeari)
}


### after this loop, the object linear_models contains all the models
summary(linear_models[["1974"]])
summary(linear_models[["2022"]])


# Modeling without region for 1997-2000 -------------------------------------

for (i in c(1997, 2000)){
  mdata <- Data %>% 
    filter(year == i) %>% 
    tidyr::drop_na(iss_souv,
                   generation,
                   int_pol,
                   ses_gender,
                   ses_lang.1,
                   ses_origin_from_canada.1,
                   ses_family_income_centile_cat)
  model <- lm(iss_souv ~ generation * ses_lang.1 +
                ses_gender + ses_family_income_centile_cat +
                ses_origin_from_canada.1 + int_pol,
              data = mdata)
  if (i == 1997){
    linear_models_19972000 <- list()
    linear_models_19972000[[as.character(i)]] <- model
  } else {
    linear_models_19972000[[as.character(i)]] <- model
  }
}

#### to check complete cases

for (i in years_to_keep){
  completecasesi <- linear_models[[as.character(i)]]$model %>% 
    mutate(year = i)
  if (i == 1974){
    completecases <- completecasesi
  } else {
    completecases <- bind_rows(completecases, completecasesi)
  }
}

for (i in c(1997, 2000)){
  completecasesi <- linear_models_19972000[[as.character(i)]]$model %>% 
    mutate(year = i)
  completecases <- bind_rows(completecases, completecasesi)
}

table(completecases$year)

# Predict models ------------------------------------------------------------------

## fix the available generations for each year
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

available_generations[["1974"]]
available_generations[["1993"]]

### Faire une loop par année pour avoir le jeu de données du graphique
for (i in 1:length(linear_models)){
  yeari <- names(linear_models)[i]
  newdata <- marginaleffects::datagrid(model = linear_models[[i]],
                                       generation = available_generations[[yeari]])
  predsi <- marginaleffects::predictions(linear_models[[i]],
                                         newdata = newdata) %>% 
    mutate(year = yeari)
  if (i == 1){
    graphsData <- predsi
  } else {
    graphsData <- bind_rows(graphsData, predsi)
  }
  message(yeari)
}

graphsData$year <- as.numeric(graphsData$year)

### graphsData contains the main data for the graphs!

## Create 1997-2000 graphdata ----------------------------------------------

newdata <- marginaleffects::datagrid(model = linear_models_19972000[["1997"]],
                                     generation = available_generations[["1997"]])

preds1997 <- marginaleffects::predictions(linear_models_19972000[["1997"]],
                                       newdata = newdata) %>% 
  mutate(year = 1997,
         ses_geoloc.1 = "all")

preds2000 <- marginaleffects::predictions(linear_models_19972000[["2000"]],
                                          newdata = newdata) %>% 
  mutate(year = 2000,
         ses_geoloc.1 = "all")

preds19972000 <- rbind(preds1997, preds2000)
graphsData2 <- bind_rows(graphsData, preds19972000) |> 
  mutate(response_scale = response_scales[as.character(year)])

# graphs -------------------------------------------------------------------

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

### generations ages
breaks <- seq(from = 1975,
              to = 2025,
              by = 5)
generations_yob <- list(
  "preboomer" = c(1920, 1945),
  "boomer" = c(1946, 1960),
  "x" = c(1961, 1979),
  "y" = c(1980, 1995),
  "z" = c(1995, 2005)
)

# 2. Make one graph by generation ----------------------------------------
## Highlight one generation, put others in background

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

calculate_age_range(2022, birth_year_range$z[1], birth_year_range$z[2])
calculate_age_range(1980, birth_year_range$boomer[1], birth_year_range$boomer[2])

generation_plot <- function(gen_highlight){
  lab_y <- ifelse(gen_highlight == "preboomer", "Predicted Position on\nIndependentist Scale", "\n")
  # Add age labels
  break_start <- min(graph_data$year[graph_data$generation == gen_highlight])
  break_start <- round(break_start / 5) * 5
  break_years <- seq(from = break_start, to = 2020, by = 5)
  graph_data <- as.data.frame(graphsData2) |> 
    mutate(highlight = ifelse(generation == gen_highlight, "yes", "no"))
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

# 4. Patchwork it all together -------------------------------------------

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
    caption = "Prediction for a Francophone man from Canada, positioned in the second income quartile, characterized by high political sophistication, assuming all other factors remain constant.\nDiamonds indicate that the question about Quebec's Independance was asked on a likert scale ; circles on a dichotomous scale"
) &
  theme(plot.caption = element_text(family = "Roboto"))

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure1_evolution.png",
  width = 15, height = 8
)
