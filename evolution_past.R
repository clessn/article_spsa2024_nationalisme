# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  mutate(yob = year - ses_age,
         ses_geoloc.1 = as.character(ses_geoloc.1),
         ses_geoloc.1 = ifelse(ses_geoloc.1 == "quebec", "region", ses_geoloc.1),
         ses_geoloc.1 = factor(ses_geoloc.1),
         generation = case_when(
           yob <= 1945 ~ "preboomer",
           yob %in% 1946:1960 ~ "boomer",
           yob %in% 1961:1979 ~ "x",
           yob %in% 1980:1995 ~ "y",
           yob >= 1996 ~ "z"
         ),
         generation = factor(generation))

table(Data$generation)
table(Data$ses_geoloc.1)

## variables of interest: ses_age, int_pol, ses_geoloc.1

# 1. find survey years with 3 variables of interest  ----------------------

CheckMissing <- Data %>% 
  group_by(year) %>% 
  summarise(n = n(),
            nas_souv   = round(sum(is.na(iss_souv)) / n(), 2),
            nas_souv2  = round(sum(is.na(iss_souv2)) / n(), 2),
            nas_age    = round(sum(is.na(ses_age)) / n(), 2),
            nas_intpol = round(sum(is.na(int_pol)) / n(), 2),
            nas_geoloc = round(sum(is.na(ses_geoloc.1)) / n(), 2))

### check missing for potential control variables
CheckMissingCtrl <- Data %>% 
  group_by(year) %>% 
  summarise(n = n(),
            nas_gender   = round(sum(is.na(ses_gender)) / n(), 2),
            nas_lang  = round(sum(is.na(ses_lang.1)) / n(), 2),
            nas_educ   = round(sum(is.na(ses_educ)) / n(), 2),
            nas_income = round(sum(is.na(ses_family_income_centile_cat)) / n(), 2),
            nas_immigrant = round(sum(is.na(ses_origin_from_canada.1)) / n(), 2),
            nas_relig = round(sum(is.na(ses_religiosity)) / n(), 2))

## remove educ and relig


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



# 2. Train models ------------------------------------------------------------

## VD: binary -----------------------------------------------------

for (i in 1:length(years_to_keep)){
  yeari <- years_to_keep[i]
  if (yeari == 1993){
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv, generation, int_pol,ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_family_income_centile_cat)
    model <- glm(iss_souv ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat,
                 data = mdata,
                 family = binomial(link = "logit"))
  } else if (yeari == 2023){
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      ## use education as proxy for political interest
      mutate(int_pol = as.numeric(as.character(ses_educ))) %>% 
      tidyr::drop_na(iss_souv,
                     generation,
                     int_pol,
                     ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_origin_from_canada.1,
                     ses_family_income_centile_cat)
    model <- glm(iss_souv ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                   ses_origin_from_canada.1,
                 data = mdata,
                 family = binomial(link = "logit"))
  } else {
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
    model <- glm(iss_souv ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                   ses_origin_from_canada.1,
                 data = mdata,
                 family = binomial(link = "logit"))
  }
  if (i == 1){
    binary_models <- list()
    binary_models[[as.character(yeari)]] <- model
  } else {
    binary_models[[as.character(yeari)]] <- model
  }
  message(yeari)
}

## VD: likert and binary --------------------------------------------------------------

for (i in 1:length(years_to_keep)){
  yeari <- years_to_keep[i]
  if (names(years_to_keep[years_to_keep == yeari]) == "1"){
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
    model <- lm(iss_souv ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                   ses_origin_from_canada.1,
                 data = mdata)
  } else if (yeari == 1993){
    mdata <- Data %>% 
      filter(year == yeari) %>% 
      tidyr::drop_na(iss_souv2, generation, int_pol,ses_geoloc.1,
                     ses_gender,
                     ses_lang.1,
                     ses_family_income_centile_cat)
    model <- lm(iss_souv2 ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat,
                 data = mdata)
  } else if (yeari == 2023){
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
    model <- lm(iss_souv2 ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                   ses_origin_from_canada.1,
                 data = mdata)
  } else {
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
    model <- lm(iss_souv2 ~ generation * int_pol * ses_geoloc.1 +
                   ses_gender + ses_lang.1 + ses_family_income_centile_cat +
                   ses_origin_from_canada.1,
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

# Predict models with int pol ----------------------------------------------------------

available_generations <- list(
  "1974" = c("preboomer", "boomer"),
  "1979" = c("preboomer", "boomer"),
  "1984" = c("preboomer", "boomer", "x"),
  "1988" = c("preboomer", "boomer", "x"),
  "1993" = c("preboomer", "boomer", "x"),
  "2004" = c("preboomer", "boomer", "x", "y"),
  "2008" = c("preboomer", "boomer", "x", "y"),
  "2015" = c("preboomer", "boomer", "x", "y", "z"),
  "2019" = c("preboomer", "boomer", "x", "y", "z"),
  "2021" = c("preboomer", "boomer", "x", "y", "z"),
  "2022" = c("preboomer", "boomer", "x", "y", "z"),
  "2023" = c("preboomer", "boomer", "x", "y", "z")
)

## Binary ------------------------------------------------------------------

for (i in 1:length(binary_models)){
  yeari <- names(binary_models)[i]
  newdata <- marginaleffects::datagrid(model = binary_models[[i]],
                                       generation = available_generations[[yeari]],
                                       int_pol = 0:1,
                                       ses_geoloc.1 = c("montreal", "region", "suburbs")) %>% 
    mutate(int_pol = as.numeric(as.character(int_pol)),
           generation = as.character(generation),
           ses_geoloc.1 = as.character(ses_geoloc.1))
  predsi <- marginaleffects::predictions(binary_models[[i]],
                                        newdata = newdata,
                                        type = "response") %>% 
    mutate(year = yeari)
  if (i == 1){
    predsbinary <- predsi 
  } else {
    predsbinary <- bind_rows(predsbinary, predsi)
  }
  message(yeari)
}

## Linear ------------------------------------------------------------------

for (i in 1:length(linear_models)){
  yeari <- names(linear_models)[i]
  newdata <- marginaleffects::datagrid(model = linear_models[[i]],
                                       generation = available_generations[[yeari]],
                                       int_pol = 0:1,
                                       ses_geoloc.1 = c("montreal", "region", "suburbs")) %>% 
    mutate(int_pol = as.numeric(as.character(int_pol)))
  predsi <- marginaleffects::predictions(linear_models[[i]],
                                         newdata = newdata) %>% 
    mutate(year = yeari)
  if (i == 1){
    predslinear <- predsi 
  } else {
    predslinear <- bind_rows(predslinear, predsi)
  }
  message(yeari)
}

predslinear$year <- as.numeric(predslinear$year)
predsbinary$year <- as.numeric(predsbinary$year)

# Graph -------------------------------------------------------------------

## events df
events_dfa <- data.frame(
  year = c(1976, 1980, 1990, 1995, 2004, 2012)
) %>% mutate(ses_geoloc.1 = "montreal",
             label = c("PQ\nElection", "Referendum", "Meech\nFailure",
                       "Referendum", "Gomery\nCommission", "Student\nCrisis"))
events_dfb <- data.frame(
  year = c(1976, 1980, 1990, 1995, 2004, 2012)
) %>% mutate(ses_geoloc.1 = "region",
             label = "")
events_dfc <- data.frame(
  year = c(1976, 1980, 1990, 1995, 2004, 2012)
) %>% mutate(ses_geoloc.1 = "suburbs",
             label = "")
events_df <- rbind(events_dfa, events_dfb, events_dfc)

events_df <- data.frame(
  year = c(1976, 1980, 1990, 1995, 2004, 2012),
  label = c("PQ\nElection", "Referendum", "Meech\nFailure",
            "Referendum", "Gomery\nCommission", "Student\nCrisis")
)


## generations labels
generations <- c("boomer", "preboomer", "x", "y", "z")
generations_labels <- c("Boomer", "Pre-boomer", "X", "Y", "Z")
names(generations_labels) <- generations

## generations ages

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

## binary
for (i in generations){
  yearsi <- predsbinary %>% 
    filter(generation == i)
  minyear <- min(yearsi$year)
  maxyear <- max(yearsi$year)
  events_dfi <- events_df %>% filter(year >= minyear &
                                       year <= maxyear)
  breaksi <- breaks[breaks >= minyear &
                      breaks <= maxyear]
  ages <- paste0(breaksi - generations_yob[[i]][2],
                 "-",
                 breaksi - generations_yob[[i]][1])
  ages_df <- data.frame(ages, year = breaksi)
  plot <- predsbinary %>%
    filter(generation == i &
             year != 2023) %>%
    mutate(
      conf.low = ifelse(conf.low < 0, 0, conf.low),
      conf.high = ifelse(conf.high > 1, 1, conf.high)
    ) %>%
    ggplot(aes(
      x = year,
      y = estimate
    )) +
    lemon::facet_rep_wrap( ~ ses_geoloc.1, ncol = 1,
                           repeat.tick.labels = "x") +
    geom_vline(data = events_dfi,
               aes(xintercept = year),
               linetype = "dotted") +
    geom_text(data = events_dfi,
              y = 1.4, hjust = 1,
              vjust = 0, angle = 90,
              aes(label = label,
                  x = year - 0.5),
              size = 2) +
    geom_label(data = ages_df, size = 2,
               aes(y = -0.3, label = ages),
               fill = "white", color = "black",
               label.size = 0) +
    ggtitle(generations_labels[i]) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                    color = as.factor(int_pol),
                    fill = as.factor(int_pol),
                    group = int_pol),
                alpha = 0.125) +
    geom_point(aes(color = as.factor(int_pol),
                   fill = as.factor(int_pol),
                   group = int_pol)) +
    clessnverse::theme_clean_light() +
    scale_x_continuous(breaks = breaksi) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = seq(from = 0, to = 1, by = 0.25),
                       labels = seq(from = 0, to = 1, by = 0.25)*100) +
    scale_fill_manual(name = "Political interest/sophistication",
                      values = c("darkgrey", "black"),
                      labels = c("Low", "High")) +
    scale_color_manual(name = "Political interest/sophistication",
                       values = c("darkgrey", "black"),
                       labels = c("Low", "High")) +
    ylab("\nProbability of\nbeing independantist (%)\n") +
    xlab("") +
    guides(name = "Political interest/sophistication") +
    theme(axis.title.y = element_text(hjust = 0.5),
          legend.title = element_text())
  ggsave(plot, filename = paste0("SharedFolder_spsa_article_nationalisme/graph/models/evolution/int_pol/binomial_", i, ".png"),
         width = 7, height = 8)
}


## linear
for (i in generations){
  yearsi <- predslinear %>% 
    filter(generation == i)
  minyear <- min(yearsi$year)
  maxyear <- max(yearsi$year)
  events_dfi <- events_df %>% filter(year >= minyear &
                                       year <= maxyear)
  breaksi <- breaks[breaks >= minyear &
                      breaks <= maxyear]
  ages <- paste0(breaksi - generations_yob[[i]][2],
                 "-",
                 breaksi - generations_yob[[i]][1])
  ages_df <- data.frame(ages, year = breaksi)
  plot <- predslinear %>%
    filter(generation == i &
             year != 2023) %>%
    mutate(
      conf.low = ifelse(conf.low < 0, 0, conf.low),
      conf.high = ifelse(conf.high > 1, 1, conf.high)
    ) %>%
    ggplot(aes(
      x = year,
      y = estimate
    )) +
    lemon::facet_rep_wrap( ~ ses_geoloc.1, ncol = 1,
                           repeat.tick.labels = "x") +
    geom_vline(data = events_dfi,
               aes(xintercept = year),
               linetype = "dotted") +
    geom_text(data = events_dfi,
              y = 1.4, hjust = 1,
              vjust = 0, angle = 90,
              aes(label = label,
                  x = year - 0.5),
              size = 2) +
    geom_label(data = ages_df, size = 2,
               aes(y = -0.3, label = ages),
               fill = "white", color = "black",
               label.size = 0) +
    ggtitle(generations_labels[i]) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                    color = as.factor(int_pol),
                    fill = as.factor(int_pol),
                    group = int_pol),
                alpha = 0.125) +
    geom_point(aes(color = as.factor(int_pol),
                   fill = as.factor(int_pol),
                   group = int_pol)) +
    clessnverse::theme_clean_light() +
    scale_x_continuous(breaks = breaksi) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.05, 0.5, 1.05),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    scale_fill_manual(name = "Political interest/sophistication",
                      values = c("darkgrey", "black"),
                      labels = c("Low", "High")) +
    scale_color_manual(name = "Political interest/sophistication",
                       values = c("darkgrey", "black"),
                       labels = c("Low", "High")) +
    ylab("\nPredicted position\non independantist scale\n") +
    xlab("") +
    theme(axis.title.y = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                     size = 7),
          legend.title = element_text())
  ggsave(plot, filename = paste0("SharedFolder_spsa_article_nationalisme/graph/models/evolution/int_pol/linear_", i, ".png"),
         width = 7, height = 8)
}



# Predict models without int pol ----------------------------------------------------------

available_generations <- list(
  "1974" = c("preboomer", "boomer"),
  "1979" = c("preboomer", "boomer"),
  "1984" = c("preboomer", "boomer", "x"),
  "1988" = c("preboomer", "boomer", "x"),
  "1993" = c("preboomer", "boomer", "x"),
  "2004" = c("preboomer", "boomer", "x", "y"),
  "2008" = c("preboomer", "boomer", "x", "y"),
  "2015" = c("preboomer", "boomer", "x", "y", "z"),
  "2019" = c("preboomer", "boomer", "x", "y", "z"),
  "2021" = c("preboomer", "boomer", "x", "y", "z"),
  "2022" = c("preboomer", "boomer", "x", "y", "z"),
  "2023" = c("preboomer", "boomer", "x", "y", "z")
)

## Linear ------------------------------------------------------------------

for (i in 1:length(linear_models)){
  yeari <- names(linear_models)[i]
  newdata <- marginaleffects::datagrid(model = linear_models[[i]],
                                       generation = available_generations[[yeari]],
                                       ses_geoloc.1 = c("montreal", "region", "suburbs")) %>% 
    mutate(int_pol = as.numeric(as.character(int_pol)))
  predsi <- marginaleffects::predictions(linear_models[[i]],
                                         newdata = newdata) %>% 
    mutate(year = yeari)
  if (i == 1){
    predslinear <- predsi 
  } else {
    predslinear <- bind_rows(predslinear, predsi)
  }
  message(yeari)
}

predslinear$year <- as.numeric(predslinear$year)

# Graph -------------------------------------------------------------------

events_df <- data.frame(
  year = c(1976, 1980, 1990, 1995, 2004, 2012),
  label = c("PQ\nElection", "Referendum", "Meech\nFailure",
            "Referendum", "Gomery\nCommission", "Student\nCrisis")
)


## generations labels
generations <- c("boomer", "preboomer", "x", "y", "z")
generations_labels <- c("Boomer", "Pre-boomer", "X", "Y", "Z")
names(generations_labels) <- generations

## generations ages

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

## facet
for (i in generations){
  yearsi <- predslinear %>% 
    filter(generation == i)
  minyear <- min(yearsi$year)
  maxyear <- max(yearsi$year)
  events_dfi <- events_df %>% filter(year >= minyear &
                                       year <= maxyear)
  breaksi <- breaks[breaks >= minyear &
                      breaks <= maxyear]
  ages <- paste0(breaksi - generations_yob[[i]][2],
                 "-",
                 breaksi - generations_yob[[i]][1])
  ages_df <- data.frame(ages, year = breaksi)
  plot <- predslinear %>%
    filter(generation == i &
             year != 2023) %>%
    mutate(
      conf.low = ifelse(conf.low < 0, 0, conf.low),
      conf.high = ifelse(conf.high > 1, 1, conf.high)
    ) %>%
    ggplot(aes(
      x = year,
      y = estimate
    )) +
    lemon::facet_rep_wrap( ~ ses_geoloc.1, ncol = 1,
                           repeat.tick.labels = "x") +
    geom_vline(data = events_dfi,
               aes(xintercept = year),
               linetype = "dotted") +
    geom_text(data = events_dfi,
              y = 1.4, hjust = 1,
              vjust = 0, angle = 90,
              aes(label = label,
                  x = year - 0.5),
              size = 2) +
    geom_label(data = ages_df, size = 2,
               aes(y = -0.3, label = ages),
               fill = "white", color = "black",
               label.size = 0) +
    ggtitle(generations_labels[i]) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.125, color = "black", fill = "black") +
    geom_point(color = "black") +
    clessnverse::theme_clean_light() +
    scale_x_continuous(breaks = breaksi) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.05, 0.5, 1.05),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    ylab("\nPredicted position\non independantist scale\n") +
    xlab("") +
    theme(axis.title.y = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                     size = 7),
          legend.title = element_text())
  ggsave(plot, filename = paste0("SharedFolder_spsa_article_nationalisme/graph/models/evolution/facet_", i, ".png"),
         width = 7, height = 8)
}


## color
for (i in generations){
  yearsi <- predslinear %>% 
    filter(generation == i)
  minyear <- min(yearsi$year)
  maxyear <- max(yearsi$year)
  events_dfi <- events_df %>% filter(year >= minyear &
                                       year <= maxyear)
  breaksi <- breaks[breaks >= minyear &
                      breaks <= maxyear]
  ages <- paste0(breaksi - generations_yob[[i]][2],
                 "-",
                 breaksi - generations_yob[[i]][1])
  ages_df <- data.frame(ages, year = breaksi)
  plot <- predslinear %>%
    filter(generation == i &
             year != 2023) %>%
    mutate(
      conf.low = ifelse(conf.low < 0, 0, conf.low),
      conf.high = ifelse(conf.high > 1, 1, conf.high)
    ) %>%
    ggplot(aes(
      x = year,
      y = estimate
    )) +
    lemon::facet_rep_wrap( ~ ses_geoloc.1, ncol = 1,
                           repeat.tick.labels = "x") +
    geom_vline(data = events_dfi,
               aes(xintercept = year),
               linetype = "dotted") +
    geom_text(data = events_dfi,
              y = 1.4, hjust = 1,
              vjust = 0, angle = 90,
              aes(label = label,
                  x = year - 0.5),
              size = 2) +
    geom_label(data = ages_df, size = 2,
               aes(y = -0.3, label = ages),
               fill = "white", color = "black",
               label.size = 0) +
    ggtitle(generations_labels[i]) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.125, color = "black", fill = "black") +
    geom_point(color = "black") +
    clessnverse::theme_clean_light() +
    scale_x_continuous(breaks = breaksi) +
    scale_y_continuous(limits = c(-0.3, 1.4),
                       breaks = c(-0.05, 0.5, 1.05),
                       labels = c("More\nFederalist", "Neutral", "More\nSeparatist")) +
    ylab("\nPredicted position\non independantist scale\n") +
    xlab("") +
    theme(axis.title.y = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                     size = 7),
          legend.title = element_text())
  ggsave(plot, filename = paste0("SharedFolder_spsa_article_nationalisme/graph/models/evolution/facet_", i, ".png"),
         width = 7, height = 8)
}
