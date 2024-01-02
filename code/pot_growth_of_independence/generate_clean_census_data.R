# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
Raw <- readxl::read_excel("SharedFolder_spsa_article_nationalisme/data/census/statistiques-recensement-2021-CEP.xls",
                          sheet = "125 CEP 2022")

row1 <- unlist(Raw[1,])
row1[is.na(row1)] <- ""
### paste row 1 in column name
names(Raw) <- paste0(names(Raw), row1)

# Associate riding names to their codes -----------------------------------
raw_riding_names <- names(Raw)[-c(1:3)]

prov_ridings <- read.csv("SharedFolder_spsa_article_nationalisme/data/census/region_ridings.csv") %>% 
  filter(level == "prov2022")

clean_riding_names <- prov_ridings$riding_name[stringdist::amatch(x = raw_riding_names, table = prov_ridings$riding_name, maxDist = 2)]
df <- data.frame(raw_riding_names, clean_riding_names) ## check if worked
## worked, associate names in Raw to their riding_ids

riding_ids <- prov_ridings$riding_id[stringdist::amatch(x = raw_riding_names, table = prov_ridings$riding_name, maxDist = 2)]
names(riding_ids) <- raw_riding_names

# Wrangling ---------------------------------------------------------------

get_value <- function(riding_id = 648, characteristic){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  value <- Raw[[riding_column]][which(Raw$characteristic == characteristic)]
  return(as.numeric(value))
}


get_riding_df <- function(riding_id = 648){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  total_pop <- get_value(riding_id = riding_id,
                         characteristic = "Population totale 2021 (Données intégrales)")
  pop_014 <- get_value(riding_id = riding_id,
                       characteristic = "0 à 14 ans (nombre)")
  total_pop14p <- total_pop - pop_014
  age1834 <-  get_value(riding_id = riding_id,
                        characteristic = "15 à 29 ans  (nombre)")
  age3455 <-  get_value(riding_id = riding_id,
                        characteristic = "30 à 44 ans  (nombre)") +
              get_value(riding_id = riding_id,
                        characteristic = "45 à 59 ans  (nombre)")
  age55p <-  get_value(riding_id = riding_id,
                        characteristic = "60 à 74 ans  (nombre)") +
             get_value(riding_id = riding_id,
                       characteristic = "75 et plus  (nombre)")
  langue_df <- Raw[276:286,]
  french <- langue_df[[riding_column]][which(langue_df$characteristic == "Français (nombre)")]
  english <- langue_df[[riding_column]][which(langue_df$characteristic == "Anglais (nombre)")]
  other <- langue_df[[riding_column]][which(langue_df$characteristic == "Langues non officielles (nombre)")]
  df <- data.frame(
    riding_id = riding_id,
    level = "provqc2022",
    var = c(rep("gender", 2), rep("age", 3), rep("langue", 3)),
    category = c("men", "women", "1834", "3554", "55p", "french", "english", "other"),
    n = c(
      get_value(riding_id = riding_id, characteristic = "Hommes + (nombre)"), # men
      get_value(riding_id = riding_id, characteristic = "Femmes + (nombre)"), # women
      age1834,
      age3455,
      age55p,
      french,
      english,
      other
    ),
    total_pop = total_pop,
    total_pop14p = total_pop14p
  )
}

t <- get_riding_df()


for (i in 1:length(riding_ids)){
  riding_idi <- riding_ids[i]
  if (i == 1){
    Clean <- get_riding_df(riding_idi)
  } else {
    Clean <- rbind(Clean, get_riding_df(riding_idi))
  }
  print(i)
  print(riding_idi)
}

Clean$n <- as.numeric(Clean$n)

saveRDS(Clean, "_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/census.rds")

# Transform into synthetic poststrat table ------------------------------------------

SurveyData <- readRDS("_SharedFolder_article_pot-growth/data/warehouse/step3_agregate_rci/rcis_prov.rds") %>% 
  mutate(gender = ifelse(male == 1, "men", "women")) %>% 
  ## only select indepedent variables
  select(riding_id, gender, age, langue)
  

CensusWide <- Clean %>% 
  mutate(varname = paste0(var, "_", category)) %>% 
  tidyr::pivot_wider(., id_cols = c("riding_id", "total_pop", "total_pop14p"),
                     names_from = "varname",
                     values_from = "n")

for (i in 1:nrow(CensusWide)) {
  options(dplyr.summarise.inform = FALSE)
  options(dplyr.left_join.inform = FALSE)
  riding_idi <- CensusWide$riding_id[i]
  prop_age14p <- CensusWide$total_pop14p[i]
  censusGender <- c("men" = CensusWide$gender_men[i],
                         "women" = CensusWide$gender_women[i])
  censusPropsGender <- censusGender/sum(censusGender)
  censusLangue <- c("french" = CensusWide$langue_french[i],
                    "english" = CensusWide$langue_english[i],
                    "other"  = CensusWide$langue_other[i])
  censusPropsLangue <- censusLangue/sum(censusLangue)
  censusAge <- c("1834" = CensusWide$age_1834[i],
                 "3554" = CensusWide$age_3554[i],
                 "55p" = CensusWide$age_55p[i])
  censusPropsAge <- censusAge/sum(censusAge)
  FirstStrat <- SurveyData %>%
    select(gender, age) %>%
    na.omit() %>%
    group_by(gender, age) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n)) %>% 
    group_by(age) %>% 
    mutate(prct = sum(prct))
  FirstStrat$adjustCoef <- censusPropsAge/FirstStrat$prct
  FirstStrat$newFreq <- FirstStrat$n*FirstStrat$adjustCoef
  FirstStrat <- FirstStrat %>% 
    ungroup() %>% 
    select(gender, age, newFreq) %>%
    rename(n=newFreq) %>%
    mutate(prct=n / sum(n))
  
  LastStage <- FirstStrat
  
  Strat <- SurveyData %>%
    select(gender, age, langue) %>%
    na.omit() %>%
    group_by(gender, age, langue) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n))
  vars <- c("gender", "age", "langue")
  args <- paste0("unique(Strat$", vars, ")", collapse = ", ")
  
  AllCombs <-
    eval(parse(text = paste0("expand.grid(", args, ")")))
  names(AllCombs) <- vars
  
  Strat <- left_join(AllCombs, Strat) %>%
    replace(is.na(.), 0) %>%
    group_by(age, langue) %>%
    mutate(prct2 = sum(prct))
  
  Strat$adjustCoef <- censusPropsLangue[as.character(Strat$langue)]/Strat$prct2
  Strat$adjustCoef <-
    ifelse(Strat$adjustCoef %in% c(-Inf, Inf), 0, Strat$adjustCoef)
  Strat$newFreq <- Strat$n * Strat$adjustCoef
  
  Strat <- Strat %>%
    select(all_of(vars), newFreq) %>%
    rename(n = newFreq) %>%
    group_by(gender, age) %>%
    mutate(prct = (n / sum(n)))
  
  LastStagej <- LastStage %>%
    select(-n) %>%
    rename(prct_ls = prct)
  
  Strat2 <- left_join(Strat, LastStagej) %>%
    mutate(prct = prct * prct_ls)
  
  LastStage <- Strat2 %>%
    select(-prct_ls)
  
  if (i == 1) {
    StratTable <- LastStage %>%
      mutate(riding_id = riding_idi)
  }
  else {
    TempStrat <- LastStage %>%
      mutate(riding_id = riding_idi)
    StratTable <- rbind(StratTable, TempStrat)
  }
  print(paste0(round(i / nrow(CensusWide) * 100), "% - ", riding_idi))
}

### Check 
StratTable$n[is.nan(StratTable$n)] <- 0
StratTable$prct[is.nan(StratTable$prct)] <- 0

# Test
StratTable %>% 
  group_by(riding_id) %>% 
  summarise(sum = sum(prct)) %>% 
  arrange(-sum)
#### Every riding has a sum of 1? Good!!

saveRDS(StratTable, "_SharedFolder_article_pot-growth/data/warehouse/dimensions/census/provqc2022/poststrat.rds")
