# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  filter(
    year >= 2021 &
    source_id %in% c("january", "february", "march", "april", "may", "june")
  ) |> 
  mutate(yob = year - ses_age,
         generation = case_when(
           yob %in% 1925:1946 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation))

data |> 
  tidyr::drop_na(generation, iss_idcan) |> 
  group_by(generation, iss_idcan) |> 
  summarise(
    n = n()
  ) |> 
  group_by(generation) |> 
  mutate(
    total = sum(n),
    prop = n / total) |> 
  filter(iss_idcan == 0)
