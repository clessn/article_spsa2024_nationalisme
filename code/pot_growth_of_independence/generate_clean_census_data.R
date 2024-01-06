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


##### CHECK INCOME PERCENTILES
revenu <- c(
  "Sans revenu" = 177960,
  "Moins de 10 000 $" = 495035,
  "10 000 $ à 19 999 $" = 853250,
  "20 000 $ à 29 999 $" = 1316685,
  "30 000 $ à 39 999 $" = 1127905,
  "40 000 $ à 49 999 $" = 994235,
  "50 000 $ à 59 999 $" = 691015,
  "60 000 $ à 69 999 $" = 459665,
  "70 000 $ à 79 999 $" = 292135,
  "80 000 $ à 89 999 $" = 174905,
  "90 000 $ à 99 999 $" = 106165,
  "100 000 $ et plus" = 229765,
  "100 000 $ à 124 999 $" = 119125,
  "125 000 $ et plus" = 110635
)

df <- data.frame(revenu) %>% 
  mutate(cumsum = cumsum(revenu),
         c = cumsum/sum(revenu))


# Wrangling ---------------------------------------------------------------

get_value <- function(riding_id = 648, characteristic,
                      df = Raw){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  value <- df[[riding_column]][which(df$characteristic == characteristic)]
  return(as.numeric(value))
}

get_riding_df <- function(riding_id = 648){
  riding_column <- names(riding_ids)[riding_ids == riding_id]
  total_pop <- get_value(riding_id = riding_id,
                         characteristic = "Population totale 2021 (Données intégrales)")
  gen_alpha <- get_value(riding_id = riding_id,
                       characteristic = "0 à 14 ans (nombre)")
  total_pop14p <- total_pop - gen_alpha
  gen_z <-  get_value(riding_id = riding_id,
                        characteristic = "15 à 29 ans  (nombre)")
  gen_y <-  get_value(riding_id = riding_id,
                        characteristic = "30 à 44 ans  (nombre)")
  gen_x <- get_value(riding_id = riding_id,
                        characteristic = "45 à 59 ans  (nombre)")
  gen_boomer <-  get_value(riding_id = riding_id,
                        characteristic = "60 à 74 ans  (nombre)")
  gen_preboomer <- get_value(riding_id = riding_id,
                       characteristic = "75 et plus  (nombre)")
  langue_df <- Raw[276:286,]
  french <- langue_df[[riding_column]][which(langue_df$characteristic == "Français (nombre)")]
  english <- langue_df[[riding_column]][which(langue_df$characteristic == "Anglais (nombre)")]
  other <- langue_df[[riding_column]][which(langue_df$characteristic == "Langues non officielles (nombre)")]
  from_canada <- get_value(riding_id = riding_id,
                           characteristic = "Non-immigrants")
  not_from_canada <- get_value(riding_id = riding_id,
                           characteristic = "Immigrants (nombre)")
  incomedf <- Raw[1394:1415,]
  inc1_10 <- get_value(riding_id = riding_id,
                       characteristic = "Sans revenu",
                       df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "Moins de 10 000 $",
              df = incomedf)
  inc1_10 <- get_value(riding_id = riding_id,
                       characteristic = "Sans revenu",
                       df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "Moins de 10 000 $",
              df = incomedf)
  inc11_25 <- get_value(riding_id = riding_id,
                        characteristic = "10 000 $ à 19 999 $",
                        df = incomedf)
  inc26_50 <- get_value(riding_id = riding_id,
                        characteristic = "20 000 $ à 29 999 $",
                        df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "30 000 $ à 39 999 $",
              df = incomedf)
  inc51_75 <- get_value(riding_id = riding_id,
                        characteristic = "40 000 $ à 49 999 $",
                        df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "50 000 $ à 59 999 $",
              df = incomedf)
  inc76_90 <- get_value(riding_id = riding_id,
                        characteristic = "60 000 $ à 69 999 $",
                        df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "70 000 $ à 79 999 $",
              df = incomedf)
  inc91_100 <- get_value(riding_id = riding_id,
                        characteristic = "80 000 $ à 89 999 $",
                        df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "90 000 $ à 99 999 $",
              df = incomedf) +
    get_value(riding_id = riding_id,
              characteristic = "100 000 $ et plus (nombre)",
              df = incomedf)
  df <- data.frame(
    riding_id = riding_id,
    var = c(rep("gender", 2),
            rep("generation", 6),
            rep("langue", 3),
            rep("origin", 2),
            rep("income", 6)),
    category = c("men", "women",
                 "alpha", "z", "y", "x", "boomer", "preboomer",
                 "french", "english", "other",
                 "from_canada", "not_from_canada",
                 "1_10", "11_25", "26_50", "51_75", "76_90", "91_100"),
    n = c(
      get_value(riding_id = riding_id, characteristic = "Hommes + (nombre)"), # men
      get_value(riding_id = riding_id, characteristic = "Femmes + (nombre)"), # women
      gen_alpha,
      gen_z,
      gen_y,
      gen_x,
      gen_boomer,
      gen_preboomer,
      french,
      english,
      other,
      from_canada,
      not_from_canada,
      inc1_10,
      inc11_25,
      inc26_50,
      inc51_75,
      inc76_90,
      inc91_100
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

saveRDS(Clean, "SharedFolder_spsa_article_nationalisme/data/census/clean_census.rds")

# Transform into synthetic poststrat table ------------------------------------------

SurveyData <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  filter(year >= 2021) %>% 
  mutate(gender = ifelse(ses_gender == "male", "men", "women"),
         origin = ifelse(ses_origin_from_canada.1 == 1, "from_canada", "not_from_canada"),
         yob = year - ses_age,
         generation = case_when(
           yob <= 1947 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation)) %>% 
  ## only select indepedent variables
  select(ses_geoloc.1, gender, generation, langue = ses_lang.1,
         origin, income = ses_family_income_centile_cat)
  
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
  ## generation
  censusGen <- c("z" = CensusWide$generation_z[i],
                 "y" = CensusWide$generation_y[i],
                 "x" = CensusWide$generation_x[i],
                 "boomer" = CensusWide$generation_boomer[i],
                 "preboomer" = CensusWide$generation_preboomer[i])
  censusPropsGen <- censusGen/sum(censusGen)
  ## langue
  censusLangue <- c("french" = CensusWide$langue_french[i],
                    "english" = CensusWide$langue_english[i],
                    "other"  = CensusWide$langue_other[i])
  censusPropsLangue <- censusLangue/sum(censusLangue)
  ## gender
  censusGender <- c("men" = CensusWide$gender_men[i],
                         "women" = CensusWide$gender_women[i])
  censusPropsGender <- censusGender/sum(censusGender)
  ## origin
  censusOrigin <- c("from_canada" = CensusWide$origin_from_canada[i],
                    "not_from_canada" = CensusWide$origin_not_from_canada[i])
  censusPropsOrigin <- censusOrigin/sum(censusOrigin)
  ## income
  censusIncome <- c("1_10" = CensusWide$income_1_10[i],
                    "11_25" = CensusWide$income_11_25[i],
                    "26_50" = CensusWide$income_26_50[i],
                    "51_75" = CensusWide$income_51_75[i],
                    "76_90" = CensusWide$income_76_90[i],
                    "91_100" = CensusWide$income_91_100[i])
  censusPropsIncome <- censusIncome/sum(censusIncome)
  
  FirstStrat <- SurveyData %>%
    select(generation, langue) %>%
    na.omit() %>%
    group_by(generation, langue) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prct = n / sum(n)) %>% 
    group_by(langue) %>% 
    mutate(prct = sum(prct))
  FirstStrat$adjustCoef <- censusPropsLangue[as.character(FirstStrat$langue)]/FirstStrat$prct
  FirstStrat$newFreq <- FirstStrat$n * FirstStrat$adjustCoef
  FirstStrat <- FirstStrat %>% 
    ungroup() %>% 
    select(generation, langue, newFreq) %>%
    rename(n=newFreq) %>%
    mutate(prct=n / sum(n))
  
  LastStage <- FirstStrat
  
  next_vars <- c("gender", "origin", "income")
  for (j in 1:length(next_vars)){
    #### https://github.com/clessn/elxn-qc2022/blob/main/codeR/stratTableGenerator.R
    vars <- c("generation", "langue", next_vars[1:j])
    if (vars[2 + j]=="gender") {
      censusProps <- censusPropsGender
    } else if (vars[2 + j]=="origin") {
      censusProps <- censusPropsOrigin
    } else if (vars[2 + j]=="income") {
      censusProps <- censusPropsIncome
    }
    Stratj <- SurveyData %>%
      select(all_of(vars)) %>%
      na.omit() %>%
      group_by_at(vars) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      tidyr::complete(!!!syms(vars), fill = list(n = 0)) %>% 
      mutate(prct = n / sum(n)) %>%
      group_by_at(vars[-1]) %>%
      mutate(prct2 = sum(prct))
    Stratj$censusProp <- censusProps[as.character(Stratj[[vars[2+j]]])]
    Stratj$adjustCoef <- Stratj$censusProp / Stratj$prct2
    Stratj$adjustCoef <- ifelse(Stratj$adjustCoef %in% c(-Inf, Inf), 0, Stratj$adjustCoef)
    Stratj$newFreq <- Stratj$n * Stratj$adjustCoef
    Stratj2 <- Stratj %>%
      select(all_of(vars), newFreq) %>%
      rename(n = newFreq) %>%
      group_by_at(vars[-(j+2)]) %>%
      mutate(prct = (n / sum(n)))
    LastStagej <- LastStage %>%
      select(-n) %>%
      rename(prct_ls = prct)
    Strat2 <- left_join(Stratj2, LastStagej) %>%
      mutate(prct = prct * prct_ls)
    LastStage <- Strat2 %>%
      select(-prct_ls)
  }
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


# Aggregate by region -----------------------------------------------------

## add total pop by riding
total_pops <- CensusWide$total_pop
names(total_pops) <- CensusWide$riding_id
StratTable$total_pop <- total_pops[as.character(StratTable$riding_id)]
## calculate freq in riding
StratTable$riding_freq <- StratTable$prct * StratTable$total_pop

## associate each riding to its region
region_ridings <- read.csv("SharedFolder_spsa_article_nationalisme/data/census/region_ridings.csv")

large_regions <- region_ridings$large
names(large_regions) <- region_ridings$riding_id
StratTable$large_region <- large_regions[as.character(StratTable$riding_id)]

granular_regions <- region_ridings$granular
names(granular_regions) <- region_ridings$riding_id
StratTable$granular_region <- granular_regions[as.character(StratTable$riding_id)]

saveRDS(StratTable, "SharedFolder_spsa_article_nationalisme/data/census/poststrat.rds")

## aggregate the proportions by regions

### large

StratTableLarge <- StratTable %>% 
  group_by(large_region, generation, langue,
           gender, origin, income) %>%
  summarise(n = sum(riding_freq)) %>% 
  group_by(large_region) %>% 
  mutate(total_region = sum(n),
         prct_region = n / total_region) %>% 
  ungroup() %>% 
  mutate(prct_total = n / sum(n))

saveRDS(StratTableLarge, "SharedFolder_spsa_article_nationalisme/data/census/poststrat_large.rds")

### granular
saveRDS(StratTable, "SharedFolder_spsa_article_nationalisme/data/census/poststrat_granular.rds")
