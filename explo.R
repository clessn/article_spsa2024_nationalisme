# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds")


# Regression --------------------------------------------------------------

model <- lm(iss_souv ~ year + ses_age + ses_gender + ses_lang.1 +
              ses_educ + ses_family_income_centile_cat + ses_origin_from_canada.1 +
              int_pol,
            data = Data)
summary(model)

survey_years <- unique(Data$year[!(Data$year %in% c(1965, 1968,
                                                    2008, 2014,
                                                    1993, 2015))])

model_data <- Data %>% 
  select(iss_souv, year, ses_age, ses_gender, ses_lang.1,
           ses_educ, ses_family_income_centile_cat, ses_origin_from_canada.1,
           int_pol)

for (i in 1:length(survey_years)){
  survey_yeari <- survey_years[i]
  modeli <- model_data %>%
    filter(year == survey_yeari) %>% 
    select(-year) %>%
    lm(iss_souv ~ .,
        data = .)
  marginal_meansi <- marginaleffects::marginal_means(modeli) %>% 
    mutate(year = survey_yeari)
  if (i == 1){
    graph <- marginal_meansi
  } else {
    graph <- rbind(graph, marginal_meansi)
  }
  print(survey_yeari)
}


for (i in 1:length(unique(graph$term))){
  print(graph %>% 
    filter(term == unique(graph$term)[i]) %>% 
    ggplot(aes(x = year, y = estimate,
               group = value,
               color = value,
               fill  = value)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
    geom_line(linewidth = 1) +
    facet_wrap(~value) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-1, 1)))
}
