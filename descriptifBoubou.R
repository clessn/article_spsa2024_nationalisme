library(tidyverse)
library(clessnverse)

Data <- readRDS("/home/alexab/Dropbox/CLESSN/article_spsa2024_nationalisme/SharedFolder_spsa_article_nationalisme/data/merged_v1.rds")

### Variables selon year ###
DataYearGender <- Data %>% 
  select(ses_gender, year) %>% 
  drop_na() %>%
  group_by(year, ses_gender) %>%
  summarise(n = n())
  
## ses_gender ##
ggplot(DataYearGender, aes(x = year, y = n, fill = ses_gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition des genres selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantGenreYear.png",
       width = 12, height = 9)

## ses_lang.1 ##
DataYearLangue <- Data %>% 
  select(ses_lang.1, year) %>% 
  drop_na() %>%
  group_by(year, ses_lang.1) %>%
  summarise(n = n())

ggplot(DataYearLangue, aes(x = year, y = n, fill = ses_lang.1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de la langue selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantLangueYear.png",
       width = 12, height = 9)

## ses_educ ## 

DataYearEduc <- Data %>% 
  select(ses_educ, year) %>% 
  drop_na() %>%
  group_by(year, ses_educ) %>%
  summarise(n = n())

ggplot(DataYearEduc, aes(x = year, y = n, fill = ses_educ)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de l'education selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantEducYear.png",
       width = 12, height = 9)


## ses_age ## 

DataYearAge <- Data %>% 
  select(ses_age, year) %>% 
  drop_na() %>%
  group_by(year, ses_age) %>%
  summarise(n = n())

ggplot(DataYearAge, aes(x = year, y = n, fill = ses_age)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de l'age selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantAgeYear.png",
       width = 12, height = 9)

## ses_religiosity ## 

DataYearRel <- Data %>% 
  select(ses_religiosity, year) %>% 
  drop_na() %>%
  group_by(year, ses_religiosity) %>%
  summarise(n = n())

ggplot(DataYearRel, aes(x = year, y = n, fill = ses_religiosity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de la religiosite selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantRelYear.png",
       width = 12, height = 9)



## ses_family_income_centile_cat ## 

DataYearInc <- Data %>% 
  select(ses_family_income_centile_cat, year) %>% 
  drop_na() %>%
  group_by(year, ses_family_income_centile_cat) %>%
  summarise(n = n())

ggplot(DataYearInc, aes(x = year, y = n, fill = ses_family_income_centile_cat)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition du revenu selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantIncYear.png",
       width = 12, height = 9)

## ses_origin_from_canada.1 ## 

DataYearOriginCan <- Data %>% 
  select(ses_origin_from_canada.1, year) %>% 
  drop_na() %>%
  group_by(year, ses_origin_from_canada.1) %>%
  summarise(n = n())

ggplot(DataYearOriginCan, aes(x = year, y = n, fill = ses_origin_from_canada.1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de l'origine canadienne selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantOriginCanYear.png",
       width = 12, height = 9)


## ses_year_canada ## 

DataYearCan <- Data %>% 
  select(ses_year_canada, year) %>% 
  drop_na() %>%
  group_by(year, ses_year_canada) %>%
  summarise(n = n())

ggplot(DataYearCan, aes(x = year, y = n, fill = ses_year_canada)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de l'arrivée au Canada selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantArrCanYear.png",
       width = 12, height = 9)


## int_pol ## 

DataYearCan <- Data %>% 
  select(int_pol, year) %>% 
  drop_na() %>%
  group_by(year, int_pol) %>%
  summarise(n = n())

ggplot(DataYearCan, aes(x = year, y = n, fill = int_pol)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Année") +
  ylab("Nombre de répondants") +
  ggtitle("Répartition de l'arrivée au Canada selon l'année") +
  theme_clean_light()

ggsave("SharedFolder_spsa_article_nationalisme/graph/repondantArrCanYear.png",
       width = 12, height = 9)

#### Graph croise ####

DataInt <- Data %>% 
  select(iss_souv2, ses_age, year, int_pol, ses_geoloc.1)

DataSouv <- DataInt %>%
  select(iss_souv2, year) %>% 
  drop_na()# %>%
#  group_by(year, iss_souv2) %>%
 # summarise(n = n())

DataSouv$iss_souv2[DataSouv$iss_souv2 == 0.33] <- 0.25
DataSouv$iss_souv2[DataSouv$iss_souv2 == 0.66] <- 0.75

DataSouv2223 <- DataSouv %>% 
  filter(year == 2021 | year == 2022 | year == 2023)

hist(DataSouv2223$iss_souv2)

DataSouv2223_grouped <- DataSouv2223 %>% 
  group_by(year, iss_souv2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

### year-souv ###
colors <- c("0" = "red", "0.25" = "pink", "0.5" = "grey", "0.75" = "lightblue", "1" = "blue")

ggplot(DataSouv2223_grouped, aes(x = year, y = proportion, fill = as.factor(iss_souv2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2021, 2022, 2023), labels = c("2021", "2022", "2023")) +
  xlab("Année") +
  ylab("Proportion") +
  ggtitle("Répartition de l'appui à la souveraineté selon l'année") +
  theme_clean_dark()

ggsave("SharedFolder_spsa_article_nationalisme/graph/souv21-23.png",
       width = 12, height = 9)

### Regions ###
DataSouvRegion <- DataInt %>% 
  select(ses_geoloc.1, year, iss_souv2)
  
DataSouvRegion$iss_souv2[DataSouvRegion$iss_souv2 == 0.33] <- 0.25
DataSouvRegion$iss_souv2[DataSouvRegion$iss_souv2 == 0.66] <- 0.75

## montreal ##

DataMtl <- DataSouvRegion %>% 
  filter(year == 2021 | year == 2022 | year == 2023) %>% 
  filter(ses_geoloc.1 == "montreal") %>% 
  drop_na() %>% 
  group_by(year, iss_souv2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))


ggplot(DataMtl, aes(x = year, y = proportion, fill = as.factor(iss_souv2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2021, 2022, 2023), labels = c("2021", "2022", "2023")) +
  xlab("Année") +
  ylab("Proportion") +
  ggtitle("Répartition de l'appui à la souveraineté selon l'année \n (ville de Montréal)") +
  theme_clean_dark()

ggsave("SharedFolder_spsa_article_nationalisme/graph/souvMtl21-23.png",
       width = 12, height = 9)

## Qc ##

DataQc <- DataSouvRegion %>% 
  filter(year == 2021 | year == 2022 | year == 2023) %>% 
  filter(ses_geoloc.1 == "quebec") %>% 
  drop_na() %>% 
  group_by(year, iss_souv2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(DataQc, aes(x = year, y = proportion, fill = as.factor(iss_souv2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2021, 2022, 2023), labels = c("2021", "2022", "2023")) +
  xlab("Année") +
  ylab("Proportion") +
  ggtitle("Répartition de l'appui à la souveraineté selon l'année \n (ville de Québec)") +
  theme_clean_dark()

ggsave("SharedFolder_spsa_article_nationalisme/graph/souvQc21-23.png",
       width = 12, height = 9)

## suburbs ##

DataSubMtl <- DataSouvRegion %>% 
  filter(year == 2021 | year == 2022 | year == 2023) %>% 
  filter(ses_geoloc.1 == "suburbs") %>% 
  drop_na() %>% 
  group_by(year, iss_souv2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(DataSubMtl, aes(x = year, y = proportion, fill = as.factor(iss_souv2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2021, 2022, 2023), labels = c("2021", "2022", "2023")) +
  xlab("Année") +
  ylab("Proportion") +
  ggtitle("Répartition de l'appui à la souveraineté selon l'année \n (suburbs of Mtl)") +
  theme_clean_dark()

ggsave("SharedFolder_spsa_article_nationalisme/graph/souvSubMtl21-23.png",
       width = 12, height = 9)

## region ##

DataRegion <- DataSouvRegion %>% 
  filter(year == 2021 | year == 2022 | year == 2023) %>% 
  filter(ses_geoloc.1 == "region") %>% 
  drop_na() %>% 
  group_by(year, iss_souv2) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(DataRegion, aes(x = year, y = proportion, fill = as.factor(iss_souv2))) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = c(2021, 2022, 2023), labels = c("2021", "2022", "2023")) +
  xlab("Année") +
  ylab("Proportion") +
  ggtitle("Répartition de l'appui à la souveraineté selon l'année \n (Régions)") +
  theme_clean_dark()

ggsave("SharedFolder_spsa_article_nationalisme/graph/souvReg21-23.png",
       width = 12, height = 9)
