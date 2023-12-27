# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  mutate(yob = year - ses_age,
         generation = case_when(
           yob <= 1945 ~ "preboomer",
           yob %in% 1946:1960 ~ "boomer",
           yob %in% 1961:1979 ~ "x",
           yob %in% 1980:1995 ~ "y",
           yob >= 1996 ~ "z"
         ),
         generation = factor(generation))

table(Data$generation)

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
      mutate(int_pol = ses_educ) %>% 
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
      mutate(int_pol = ses_educ) %>% 
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

# Predict models ----------------------------------------------------------


