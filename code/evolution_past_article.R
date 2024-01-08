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
                   "2" = 2022,
                   "2" = 2023)

response_scales <- c(
  `1974` = "dichotomous",
  `1979` = "point4",
  `1984` = "dichotomous",
  `1988` = "point4",
  `1993` = "point4",
  `2004` = "point4",
  `2008` = "point4",
  `2015` = "point4",
  `2019` = "point4",
  `2021` = "point4",
  `2022` = "point4",
  `2023` = "point5"
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
    model <- lm(iss_souv ~ generation * ses_geoloc.1 +
                  ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                  ses_origin_from_canada.1 + int_pol,
                data = mdata)
  } else if (yeari == 1993){ ## exception for 1993
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv2, generation, int_pol,ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv2 ~ generation * ses_geoloc.1 +
                  ses_gender + ses_lang.1 + ses_family_income_centile_cat + int_pol,
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
    model <- lm(iss_souv2 ~ generation * ses_geoloc.1 +
                  ses_gender + ses_lang.1 + ses_family_income_centile_cat +
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
    model <- lm(iss_souv2 ~ generation * ses_geoloc.1 +
                  ses_gender + ses_lang.1 + ses_family_income_centile_cat +
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
  model <- lm(iss_souv ~ generation +
                ses_gender + ses_lang.1 + ses_family_income_centile_cat +
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
  "2022" = c("preboomer", "boomer", "x", "y", "z"),
  "2023" = c("preboomer", "boomer", "x", "y", "z")
)

available_generations[["1974"]]
available_generations[["1993"]]

### Faire une loop par année pour avoir le jeu de données du graphsique
for (i in 1:length(linear_models)){
  yeari <- names(linear_models)[i]
  newdata <- marginaleffects::datagrid(model = linear_models[[i]],
                                       generation = available_generations[[yeari]],
                                       ses_geoloc.1 = c("montreal", "quebec", "region", "suburbs"))
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

graphsData$response_scale <- response_scales[as.character(graphsData$year)]
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
for (i in c("montreal", "quebec", "region", "suburbs")){
  predsi <- preds19972000 %>% mutate(ses_geoloc.1 = i)
  if (i == "montreal"){
    preds19972000 <- predsi
  } else {
    preds19972000 <- rbind(preds19972000, predsi)
  }
}

graphsData2 <- bind_rows(graphsData, preds19972000)

# graphs -------------------------------------------------------------------

## squelette
graphsData2 %>%
  filter(generation == "boomer" &
           year != 2023) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_point() +
  facet_wrap(~ses_geoloc.1)

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

# Functions ---------------------------------------------------------------
old_gen_graphs_together <- function(data){
  plot1 <- ggplot(data = data, aes(x = year, y = estimate)) +
    geom_segment(x = 1997, xend = 1997, linetype = "dotted",
                 linewidth = 0.5, color = "grey80",
                 y = 0.3, yend = 0.7) +
    geom_segment(x = 2000, xend = 2000, linetype = "dotted",
                 linewidth = 0.6, color = "grey80",
                 y = 0.3, yend = 0.7) +
    geom_vline(data = events_df, aes(xintercept = year),
               linetype = "dashed", color = "grey40") +
    geom_text(data = events_df, aes(label = label, x = year + 0.5),
              y = 1.4, angle = 90, hjust = 1, vjust = 1,
              color = "grey40", lineheight = 0.7, size = 3.5) +
    geom_line(size = 0.5, aes(linetype = ses_geoloc.1, color = ses_geoloc.1)) +
    geom_point(size = 1, color = "black") +
    theme_minimal() +
    labs(
      x = "",
      y = "Predicted position on independentist scale",
      linetype = "Geoloc",
      color = "Geoloc"
    ) +
    scale_linetype_manual(
      values = c(
        "montreal" = "longdash", 
        "quebec" = "dashed", 
        "suburbs" = "solid", 
        "region" = "dotted",
        "all" = "solid"
      ),
      guide = guide_legend(title = "Region type"),
      labels = c("montreal"="Montreal", "quebec" = "Quebec City",
                 "region"="Regions", "suburbs"="Greater Montreal Area",
                 "all" = "All")
    ) +
    scale_color_manual(
      values = c(
        "montreal" = "grey", 
        "quebec" = "black", 
        "suburbs" = "black", 
        "region" = "black"
      ),
      guide = guide_legend(title = "Region type"),
      labels = c("montreal"="Montreal", "quebec" = "Quebec City", "region"="Regions", "suburbs"="Greater Montreal Area")
    ) +
    scale_x_continuous(breaks = seq(from = 1975, to = 2020, by = 5)) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.2, 0.55, 1.3),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    ylab("\nPredicted position\non independentist scale\n") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, hjust = 0.5)) 
  return(plot1)
}

old_gen_graphs_facet <- function(data){
  plot <- ggplot(data = data, aes(x = year, y = estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "gray") +
    geom_line(size = 1) +
    geom_point(aes(shape = response_scale), size = 2, color = "black") +
    geom_hline(yintercept = 0.5, linetype = "solid", color = "black", size = 0.5) +
    theme_minimal() +
    labs(
      x = "",
      y = "Predicted position on independentist scale"
    ) +
    facet_wrap(
      ~ ses_geoloc.1, 
      ncol = 4, 
      labeller = labeller(ses_geoloc.1 = c(
        "montreal" = "Montreal", 
        "quebec" = "Quebec", 
        "region" = "Regions", 
        "suburbs" = "Greater Montreal Area"
      ))
    ) +
    guides(shape = guide_legend(title = "Response scale")) +
    scale_x_continuous(breaks = seq(from=1975, to=2020, by = 5)) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.05, 0.5, 1.05),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    scale_shape_manual(values = c(16, 17),
                       labels = c("dichotomous" = "Dichotomous", "point4" = "Likert")) +
    ylab("\nPredicted position\non independentist scale\n") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.position = "bottom")
  return(plot)
}

## Boomer ------------------------------------------------------------------
calculate_age_range <- function(year, birth_start = 1947, birth_end = 1961) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste(age_start, "-", age_end))
}

## squelette graphs1
plot1 <- graphsData2 %>%
  filter(generation == "boomer" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_together(.)

# Add age labels
break_years <- seq(from = 1975, to = 2020, by = 5)
for (year in break_years) {
  age_label <- calculate_age_range(year)
  plot1 <- plot1 +
    annotate("text", x = year, y = 0, label = age_label, vjust = 7, hjust = 0.5, size = 2.5)
}


## squelette graphs2
plot2 <- graphsData %>%
  filter(generation == "boomer" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_facet(.)
print(plot2)

## combinaison des graphss
# Sous-layout pour plot1 avec des espaces de chaque côté
plot1_with_spacers <- plot_spacer() | plot1 | plot_spacer()

# Adjust the widths argument to change spacer size
plot1_with_spacers <- plot1_with_spacers + plot_layout(widths = c(1, 5, 1))  # Adjust the ratio as needed

# Layout combiné
combined_plot <- plot1_with_spacers / plot2 +
  plot_annotation(title = "Boomer",
                  caption = "Prediction for a boomer Francophone man from Canada, positioned in the second income quartile, characterized by high political sophistication, assuming all other factors remain constant.\nSince there is no available geographical data for the 1997 and 2000 CES, the prediction is the same for all regions.",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(heights = unit(c(7, 0.5), c('cm', 'null')))

ggsave(plot = combined_plot,
       filename = "SharedFolder_spsa_article_nationalisme/graphs/models/evolution/plot_boomer.png", 
       width = 12, height = 8)

## preboomer ------------------------------------------------------------------
calculate_age_rangePB <- function(year, birth_start = 1925, birth_end = 1947) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste(age_start, "-", age_end))
}

## squelette graphs1
plot1 <- graphsData2 %>%
  filter(generation == "preboomer" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_together(.)

# Add age labels
break_years <- seq(from = 1975, to = 2020, by = 5)
for (year in break_years) {
  age_label <- calculate_age_rangePB(year)
  plot1 <- plot1 +
    annotate("text", x = year, y = 0, label = age_label, vjust = 7, hjust = 0.5, size = 2.5)
}

plot1

## squelette graphs2
plot2 <- graphsData %>%
  filter(generation == "preboomer" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_facet(.)

print(plot2)

## combinaison des graphss
# Sous-layout pour plot1 avec des espaces de chaque côté
plot1_with_spacers <- plot_spacer() | plot1 | plot_spacer()

# Adjust the widths argument to change spacer size
plot1_with_spacers <- plot1_with_spacers + plot_layout(widths = c(1, 5, 1))  # Adjust the ratio as needed

# Layout combiné
combined_plot <- plot1_with_spacers / plot2 +
  plot_annotation(title = "Pre-boomer",
                  caption = "Prediction for a pre-boomer Francophone man from Canada, positioned in the second income quartile, characterized by high political sophistication, assuming all other factors remain constant.\nSince there is no available geographical data for the 1997 and 2000 CES, the prediction is the same for all regions.",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(heights = unit(c(7, 0.5), c('cm', 'null')))

ggsave(filename = "SharedFolder_spsa_article_nationalisme/graphs/models/evolution/plot_preboomer.png", 
       plot = combined_plot, width = 12, height = 8)

## x ------------------------------------------------------------------
calculate_age_rangeX <- function(year, birth_start = 1962, birth_end = 1976) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste(age_start, "-", age_end))
}

## squelette graphs1
break_years <- seq(from = 1985, to = 2020, by = 5)
plot1 <- graphsData2 %>%
  filter(generation == "x" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_together(.) +
  scale_x_continuous(limits = c(1984, 2022),
                     breaks = break_years)

# Add age labels
for (year in break_years) {
  age_label <- calculate_age_rangeX(year)
  plot1 <- plot1 +
    annotate("text", x = year, y = 0, label = age_label, vjust = 7, hjust = 0.5, size = 2.5)
}

print(plot1)


## squelette graphs2
plot2 <- graphsData %>%
  filter(generation == "x" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  old_gen_graphs_facet(.) +
  scale_x_continuous(limits = c(1984, 2022),
                     breaks = break_years)

print(plot2)

## combinaison des graphss
# Sous-layout pour plot1 avec des espaces de chaque côté
plot1_with_spacers <- plot_spacer() | plot1 | plot_spacer()

# Adjust the widths argument to change spacer size
plot1_with_spacers <- plot1_with_spacers + plot_layout(widths = c(1, 5, 1))  # Adjust the ratio as needed

# Layout combiné
combined_plot <- plot1_with_spacers / plot2 +
  plot_annotation(title = "X",
                  caption = "Prediction for a X Francophone man from Canada, positioned in the second income quartile, characterized by high political sophistication, assuming all other factors remain constant.\nSince there is no available geographical data for the 1997 and 2000 CES, the prediction is the same for all regions.",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(heights = unit(c(7, 0.5), c('cm', 'null')))

ggsave(filename = "SharedFolder_spsa_article_nationalisme/graphs/models/evolution/plot_x.png", 
       plot = combined_plot, width = 12, height = 8)

## y ------------------------------------------------------------------
calculate_age_rangeY <- function(year, birth_start = 1977, birth_end = 1991) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste(age_start, "-", age_end))
}

## squelette graphs1
plot1 <- graphsData %>%
  filter(generation == "y" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  ggplot(aes(x = year, y = estimate, linetype = ses_geoloc.1, color = ses_geoloc.1)) +
  geom_line(size = 0.5) +
  geom_point(size = 1, color = "black") +
  theme_minimal() +
  labs(
    x = "",
    y = "Predicted position on independentist scale",
    linetype = "Geoloc",
    color = "Geoloc"
  ) +
  scale_linetype_manual(
    values = c(
      "montreal" = "longdash", 
      "quebec" = "dashed", 
      "suburbs" = "solid", 
      "region" = "dotted"
    ),
    guide = guide_legend(title = "Region type"),
    labels = c("montreal"="Montreal", "quebec" = "Quebec City", "region"="Regions", "suburbs"="Greater Montreal Area")
  ) +
  scale_color_manual(
    values = c(
      "montreal" = "grey", 
      "quebec" = "black", 
      "suburbs" = "black", 
      "region" = "black"
    ),
    guide = guide_legend(title = "Region type"),
    labels = c("montreal"="Montreal", "quebec" = "Quebec City", "region"="Regions", "suburbs"="Greater Montreal Area")
  ) +
  scale_x_continuous(breaks = seq(from = 2005, to = 2020, by = 5)) +
  scale_y_continuous(limits = c(-0.3, 1.4),
                     breaks = c(-0.2, 0.55, 1.3),
                     labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
  ylab("\nPredicted position\non independentist scale\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5)) 

# Add age labels
break_years <- seq(from = 2005, to = 2020, by = 5)
for (year in break_years) {
  age_label <- calculate_age_rangeY(year)
  plot1 <- plot1 +
    annotate("text", x = year, y = 0, label = age_label, vjust = 7, hjust = 0.5, size = 2.5)
}

print(plot1)


## squelette graphs2
plot2 <- graphsData %>%
  filter(generation == "y" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "gray") +
  geom_line(size = 1) +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal() +
  labs(
    x = "",
    y = "Predicted position on independentist scale"
  ) +
  facet_wrap(
    ~ ses_geoloc.1, 
    ncol = 4, 
    labeller = labeller(ses_geoloc.1 = c(
      "montreal" = "Montreal", 
      "quebec" = "Quebec", 
      "region" = "Regions", 
      "suburbs" = "Greater Montreal Area"
    ))
  ) +
  scale_x_continuous(breaks = seq(from=2005, to=2020, by = 5)) +
  scale_y_continuous(limits = c(-0.3, 1.4),
                     breaks = c(-0.05, 0.5, 1.05),
                     labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
  ylab("\nPredicted position\non independentist scale\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5)) 

print(plot2)

## combinaison des graphss
# Sous-layout pour plot1 avec des espaces de chaque côté
plot1_with_spacers <- plot_spacer() | plot1 | plot_spacer()

# Adjust the widths argument to change spacer size
plot1_with_spacers <- plot1_with_spacers + plot_layout(widths = c(1, 5, 1))  # Adjust the ratio as needed

# Layout combiné
combined_plot <- plot1_with_spacers / plot2 +
  plot_annotation(title = "Y",
                  caption = "Prediction for a Y Francophone man from Canada, positioned in the second income quartile,\n characterized by high political sophistication, assuming all other factors remain constant.",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(heights = unit(c(7, 0.5), c('cm', 'null')))


ggsave(filename = "SharedFolder_spsa_article_nationalisme/graphs/models/evolution/plot_y.png", 
       plot = combined_plot, width = 12, height = 8)

## z ------------------------------------------------------------------
calculate_age_range_Z <- function(year, birth_start = 1992, birth_end = 2003) {
  age_start = year - birth_end
  age_end = year - birth_start
  return(paste(age_start, "-", age_end))
}


## squelette graphs1
plot1 <- graphsData %>%
  filter(generation == "z" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  ggplot(aes(x = year, y = estimate, linetype = ses_geoloc.1, color = ses_geoloc.1)) +
  geom_line(size = 0.5) +
  geom_point(size = 1, color = "black") +
  theme_minimal() +
  labs(
    x = "",
    y = "Predicted position on independentist scale",
    linetype = "Geoloc",
    color = "Geoloc"
  ) +
  scale_linetype_manual(
    values = c(
      "montreal" = "longdash", 
      "quebec" = "dashed", 
      "suburbs" = "solid", 
      "region" = "dotted"
    ),
    guide = guide_legend(title = "Region type"),
    labels = c("montreal"="Montreal", "quebec" = "Quebec City", "region"="Regions", "suburbs"="Greater Montreal Area")
  ) +
  scale_color_manual(
    values = c(
      "montreal" = "grey", 
      "quebec" = "black", 
      "suburbs" = "black", 
      "region" = "black"
    ),
    guide = guide_legend(title = "Region type"),
    labels = c("montreal"="Montreal", "quebec" = "Quebec City", "region"="Regions", "suburbs"="Greater Montreal Area")
  ) +
  scale_x_continuous(breaks = seq(from = 2015, to = 2020, by = 5)) +
  scale_y_continuous(limits = c(-0.3, 1.4),
                     breaks = c(-0.2, 0.55, 1.3),
                     labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
  ylab("\nPredicted position\non independentist scale\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5)) 

# Add age labels
break_years <- seq(from = 2015, to = 2020, by = 5)
for (year in break_years) {
  age_label <- calculate_age_range_Z(year)
  plot1 <- plot1 +
    annotate("text", x = year, y = 0, label = age_label, vjust = 7, hjust = 0.5, size = 2.5)
}

print(plot1)


## squelette graphs2
plot2 <- graphsData %>%
  filter(generation == "z" & year != 2023) %>%
  mutate(
    conf.low = ifelse(conf.low < 0, 0, conf.low),
    conf.high = ifelse(conf.high > 1, 1, conf.high)
  ) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "gray") +
  geom_line(size = 1) +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "solid", color = "black", size = 0.5) +
  theme_minimal() +
  labs(
    x = "",
    y = "Predicted position on independentist scale"
  ) +
  facet_wrap(
    ~ ses_geoloc.1, 
    ncol = 4, 
    labeller = labeller(ses_geoloc.1 = c(
      "montreal" = "Montreal", 
      "quebec" = "Quebec", 
      "region" = "Regions", 
      "suburbs" = "Greater Montreal Area"
    ))
  ) +
  scale_x_continuous(breaks = seq(from=2015, to=2022, by = 3)) +
  scale_y_continuous(limits = c(-0.3, 1.4),
                     breaks = c(-0.05, 0.5, 1.05),
                     labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
  ylab("\nPredicted position\non independentist scale\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 90, hjust = 0.5)) 

print(plot2)

## combinaison des graphss
# Sous-layout pour plot1 avec des espaces de chaque côté
plot1_with_spacers <- plot_spacer() | plot1 | plot_spacer()

# Adjust the widths argument to change spacer size
plot1_with_spacers <- plot1_with_spacers + plot_layout(widths = c(1, 5, 1))  # Adjust the ratio as needed

# Layout combiné
combined_plot <- plot1_with_spacers / plot2 +
  plot_annotation(title = "Z",
                  caption = "Prediction for a Z Francophone man from Canada, positioned in the second income quartile,\n characterized by high political sophistication, assuming all other factors remain constant.",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(heights = unit(c(7, 0.5), c('cm', 'null')))


ggsave(filename = "SharedFolder_spsa_article_nationalisme/graphs/models/evolution/plot_z.png", 
       plot = combined_plot, width = 12, height = 8)
