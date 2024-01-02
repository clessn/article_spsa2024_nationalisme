# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyverse)

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
summary(linear_models[[1974]])
summary(linear_models[[2022]])


# Predict models ------------------------------------------------------------------

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

### predslinear contains the main data for the graph!

# Graph -------------------------------------------------------------------

## 1. Generate auxiliary data ----------------------------------------------

### important events
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


## 1. start by only doing it for 1 genration (example: boomer)

## Boomer ------------------------------------------------------------------

