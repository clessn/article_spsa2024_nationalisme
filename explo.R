# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds")


# Regression --------------------------------------------------------------

model <- lm(iss_souv ~ year + ses_age + ses_gender + ses_lang.1 +
              ses_educ + ses_family_income_centile_cat + ses_origin_from_canada.1 +
              int_pol,
            data = Data)
summary(model)


survey_years <- unique(Data$)

for (i in 1:length()){
  survey_yeari <- survey_years[i]
  modeli <- data %>%
    filter(survey_year == survey_yeari) %>% 
    select(-survey_year) %>% 
    glm(support_sovereignty ~ .,
        data = ., family = binomial())
  marginal_meansi <- marginalmeans(modeli) %>% 
    mutate(survey_year = survey_yeari)
  if (i == 1){
    graph <- marginal_meansi
  } else {
    graph <- rbind(graph, marginal_meansi)
  }
  print(survey_yeari)
}
