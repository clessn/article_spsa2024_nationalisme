## generate_weights.R
## Applique le raking au dataset merged_v1.rds pour créer merged_v2.rds
##
## Stratégie:
## 1. Associer chaque observation à l'année de recensement précédente la plus proche
## 2. Convertir l'âge en groupes d'âge compatibles avec les marginals
## 3. Appliquer le raking avec survey::rake()
## 4. Gérer les valeurs manquantes avec des fonctions de raking par combinaisons
##
## Note: Pour 1996-2006, language n'est disponible qu'en "total" (pas par sexe)
##       On utilise alors un raking marginal pour la langue

library(dplyr)
library(tidyr)
library(survey)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Chemins
# Assumes script is running from code/census_marginals/
data_dir <- "../../SharedFolder_spsa_article_nationalisme/data" 
marginals_path <- "marginals_all.rds"

# Années de recensement disponibles
CENSUS_YEARS <- c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016, 2021)

# Paramètres du raking
RAKE_MAXIT <- 100      # Nombre max d'itérations
RAKE_EPSILON <- 1e-6   # Seuil de convergence

# =============================================================================
# FONCTIONS UTILITAIRES
# =============================================================================

#' Trouve l'année de recensement appropriée pour une année de sondage
#' Règle: prendre le recensement précédent le plus proche
get_census_year <- function(survey_year) {
  valid_years <- CENSUS_YEARS[CENSUS_YEARS <= survey_year]
  if (length(valid_years) == 0) {
    return(min(CENSUS_YEARS))  # Fallback au plus ancien
  }
  return(max(valid_years))
}

#' Convertit l'âge en groupe d'âge standardisé (15-24, 25-34, etc.)
#' Compatible avec les marginals de la plupart des recensements
age_to_group <- function(age) {
  case_when(
    age < 15 ~ NA_character_,
    age >= 15 & age <= 24 ~ "15-24",
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 44 ~ "35-44",
    age >= 45 & age <= 54 ~ "45-54",
    age >= 55 & age <= 64 ~ "55-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_
  )
}

#' Standardise les groupes d'âge des marginals vers notre format
#' Les marginals ont parfois des groupes plus fins (15-19, 20-24, etc.)
standardize_age_groups <- function(marginals_df) {
  marginals_df %>%
    filter(variable == "age_group") %>%
    mutate(
      age_group_std = case_when(
        category %in% c("15-19", "20-24") ~ "15-24",
        category == "15-24" ~ "15-24",
        category %in% c("25-29", "30-34") ~ "25-34",
        category == "25-34" ~ "25-34",
        category %in% c("35-39", "40-44") ~ "35-44",
        category == "35-44" ~ "35-44",
        category %in% c("45-49", "50-54") ~ "45-54",
        category == "45-54" ~ "45-54",
        category %in% c("55-59", "60-64") ~ "55-64",
        category == "55-64" ~ "55-64",
        category %in% c("65-69", "70-74", "75-79", "80-84", "85-89",
                        "85+", "90-94", "95+", "99+", "75+") ~ "65+",
        category == "65+" ~ "65+",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_group_std)) %>%
    group_by(census_year, gender, age_group_std) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    rename(category = age_group_std) %>%
    mutate(variable = "age_group") %>%
    group_by(census_year, variable) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
}

#' Prépare les marges pour une année de recensement donnée
#' Retourne une liste de data.frames pour survey::rake()
prepare_margins <- function(marginals_df, census_yr, has_lang_by_sex = TRUE) {

  # Filtrer pour cette année
  m <- marginals_df %>% filter(census_year == census_yr)

  # --- Age par gender ---
  age_margins <- m %>%
    filter(variable == "age_group") %>%
    select(gender, category, n) %>%
    pivot_wider(names_from = gender, values_from = n, values_fill = 0)

  # Créer les marges séparées pour male et female (pour l'interaction)
  age_male <- age_margins %>%
    filter(male > 0) %>%
    transmute(
      ses_gender = "male",
      age_group = category,
      Freq = male
    )

  age_female <- age_margins %>%
    filter(female > 0) %>%
    transmute(
      ses_gender = "female",
      age_group = category,
      Freq = female
    )

  age_gender <- bind_rows(age_male, age_female)

  # --- Language ---
  lang_m <- m %>% filter(variable == "language")
  
  lang_margins <- NULL
  if (nrow(lang_m) > 0) {
    if (has_lang_by_sex) {
      # Language par gender disponible
      lang_margins <- lang_m %>%
        filter(gender %in% c("male", "female")) %>%
        transmute(
          ses_gender = gender,
          ses_lang.1 = category,
          Freq = n
        )
    } else {
      # Language total seulement
      lang_margins <- lang_m %>%
        filter(gender == "total" | (is.na(gender) & !is.na(n))) %>%
        transmute(
          ses_lang.1 = category,
          Freq = n
        )
    }
  }

  # --- Education par gender ---
  edu_m <- m %>% filter(variable == "education")
  edu_margins <- NULL

  if (nrow(edu_m) > 0) {
      edu_margins <- edu_m %>%
        filter(gender %in% c("male", "female")) %>%
        transmute(
          ses_gender = gender,
          ses_educ = category,
          Freq = n
        )
  }

  list(
    age_gender = age_gender,
    lang = lang_margins,
    edu = edu_margins
  )
}

#' Applique le raking à un sous-ensemble de données
#' Retourne les poids calculés
apply_raking <- function(data_subset, margins, use_edu = TRUE, use_lang = TRUE) {

  # Créer un design de sondage initial (poids = 1)
  data_subset$init_weight <- 1
  
  # Ensure no NAs in IDs (survey package can be picky)
  if(any(is.na(data_subset$init_weight))) stop("NA weights")

  des <- svydesign(
    ids = ~1,
    weights = ~init_weight,
    data = data_subset
  )

  # Préparer les marges pour rake()
  # Toujours commencer par Age x Gender car c'est notre base solide
  sample_margins <- list(~ses_gender + age_group)
  pop_margins <- list(margins$age_gender)

  # Ajouter Language
  if (use_lang && !is.null(margins$lang) && nrow(margins$lang) > 0) {
    if ("ses_gender" %in% names(margins$lang)) {
        # Joint distribution
        sample_margins <- c(sample_margins, list(~ses_gender + ses_lang.1))
        pop_margins <- c(pop_margins, list(margins$lang))
    } else {
        # Marginal distribution
        sample_margins <- c(sample_margins, list(~ses_lang.1))
        pop_margins <- c(pop_margins, list(margins$lang))
    }
  }

  # Ajouter Education
  if (use_edu && !is.null(margins$edu) && nrow(margins$edu) > 0) {
     # Education is always by gender in our structure
    sample_margins <- c(sample_margins, list(~ses_gender + ses_educ))
    pop_margins <- c(pop_margins, list(margins$edu))
  }
  
  # --- ALIGNEMENT POPULATION / SAMPLE ---
  # Pour éviter l'erreur "Some strata absent from sample", on ne garde 
  # dans les marges de population que les catégories/combinaisons présentes dans l'échantillon.
  
  clean_pop_margins <- list()
  clean_sample_margins <- list()
  
  for (i in seq_along(sample_margins)) {
    form <- sample_margins[[i]]
    pop_df <- pop_margins[[i]]
    
    # Variables impliquées dans cette marge
    vars <- all.vars(form)
    
    # Vérifier quelles combinaisons existent dans le sample
    # On utilise distinct() pour avoir les combinaisons uniques présentes
    sample_combs <- data_subset %>%
      select(all_of(vars)) %>%
      distinct() %>%
      drop_na() # On ignore les NA car ils ne sont pas rakés par cette marge
      
    if (nrow(sample_combs) == 0) {
        # Si aucune observation n'a de valeurs valides pour cette marge, on la saute
        next
    }

    # Filtrer la population pour ne garder que ce qui est dans le sample
    # semi_join va garder les lignes de pop_df qui matchent sample_combs
    pop_filtered <- pop_df %>%
      semi_join(sample_combs, by = vars)
    
    if (nrow(pop_filtered) > 0) {
      clean_pop_margins[[length(clean_pop_margins) + 1]] <- pop_filtered
      clean_sample_margins[[length(clean_sample_margins) + 1]] <- form
    }
  }
  
  if (length(clean_pop_margins) == 0) {
      # Fallback si aucune marge ne matche
      return(rep(1, nrow(data_subset)))
  }

  # Appliquer le raking
  tryCatch({
    raked <- rake(
      design = des,
      sample.margins = clean_sample_margins,
      population.margins = clean_pop_margins,
      control = list(maxit = RAKE_MAXIT, epsilon = RAKE_EPSILON)
    )

    return(weights(raked))

  }, error = function(e) {
    message(paste("Raking failed, returning 1s. Error:", e$message))
    return(rep(1, nrow(data_subset)))
  })
}

# =============================================================================
# CHARGEMENT DES DONNÉES
# =============================================================================

message("Loading data...")

if (!file.exists(file.path(data_dir, "merged_v1.rds"))) {
    stop(paste("Could not find merged_v1.rds at", file.path(data_dir, "merged_v1.rds")))
}

# Dataset principal
data_raw <- readRDS(file.path(data_dir, "merged_v1.rds"))
message(sprintf("  - merged_v1.rds: %d observations", nrow(data_raw)))

if (!file.exists(marginals_path)) {
    stop(paste("Could not find marginals_all.rds at", marginals_path))
}

# Marginals
marginals_raw <- readRDS(marginals_path)
message(sprintf("  - marginals_all.rds: %d rows", nrow(marginals_raw)))

# =============================================================================
# PRÉPARATION DES DONNÉES
# =============================================================================

message("\nPreparing data...")

# Standardiser les groupes d'âge dans les marginals
marginals_std <- standardize_age_groups(marginals_raw)

# Ajouter les autres variables (language, education)
marginals_other <- marginals_raw %>%
  filter(variable %in% c("language", "education"))

marginals <- bind_rows(marginals_std, marginals_other)

# Identifier les années où language est par "total" seulement
lang_by_total_years <- marginals_raw %>%
  filter(variable == "language") %>%
  group_by(census_year) %>%
  summarise(has_gender = any(gender %in% c("male", "female")), .groups = "drop") %>%
  filter(!has_gender) %>%
  pull(census_year)

message(sprintf("  - Years with language by total only: %s",
                paste(lang_by_total_years, collapse = ", ")))

# Préparer le dataset
data <- data_raw %>%
  mutate(
    # Créer les groupes d'âge
    age_group = age_to_group(ses_age),

    # Assigner l'année de recensement
    census_year = sapply(year, get_census_year),

    # Standardiser gender
    ses_gender = case_when(
      ses_gender == "male" ~ "male",
      ses_gender == "female" ~ "female",
      TRUE ~ NA_character_
    ),

    # Standardiser language
    ses_lang.1 = case_when(
      ses_lang.1 == "french" ~ "french",
      ses_lang.1 == "english" ~ "english",
      ses_lang.1 == "other" ~ "other",
      TRUE ~ NA_character_
    ),

    # Standardiser education
    ses_educ = case_when(
      ses_educ == 0 ~ "0",
      ses_educ == 1 ~ "1",
      TRUE ~ NA_character_
    )
  )

# Résumé des données préparées
message("\nData summary after preparation:")
message(sprintf("  - Total observations: %d", nrow(data)))
message(sprintf("  - With valid age_group: %d", sum(!is.na(data$age_group))))
message(sprintf("  - With valid gender: %d", sum(!is.na(data$ses_gender))))
message(sprintf("  - With valid language: %d", sum(!is.na(data$ses_lang.1))))
message(sprintf("  - With valid education: %d", sum(!is.na(data$ses_educ))))

# =============================================================================
# APPLICATION DU RAKING
# =============================================================================

message("\nApplying raking by census year...")

# Initialiser les poids
data$weight <- NA_real_

# Traiter par année de recensement
census_years_in_data <- sort(unique(data$census_year))

for (cy in census_years_in_data) {

  message(sprintf("\n  Processing census year %d...", cy))

  # Subset pour cette année de recensement
  idx <- which(data$census_year == cy)
  subset_data <- data[idx, ]

  message(sprintf("    - %d observations", length(idx)))

  # Vérifier si language est par total pour cette année
  has_lang_by_sex <- !(cy %in% lang_by_total_years)

  # Préparer les marges
  margins <- prepare_margins(marginals, cy, has_lang_by_sex)

  # Identifier les observations complètes pour le raking
  
  # Niveau 1: Toutes les variables (age, gender, language, education)
  complete_all <- !is.na(subset_data$age_group) &
                  !is.na(subset_data$ses_gender) &
                  !is.na(subset_data$ses_lang.1) &
                  !is.na(subset_data$ses_educ)

  # Niveau 2: Sans education (age, gender, language)
  complete_no_edu <- !is.na(subset_data$age_group) &
                     !is.na(subset_data$ses_gender) &
                     !is.na(subset_data$ses_lang.1) &
                     is.na(subset_data$ses_educ)

  # Niveau 3: Sans language (age, gender, education)
  complete_no_lang <- !is.na(subset_data$age_group) &
                      !is.na(subset_data$ses_gender) &
                      is.na(subset_data$ses_lang.1) &
                      !is.na(subset_data$ses_educ)

  # Niveau 4: Seulement age et gender
  complete_basic <- !is.na(subset_data$age_group) &
                    !is.na(subset_data$ses_gender) &
                    is.na(subset_data$ses_lang.1) &
                    is.na(subset_data$ses_educ)
  
  # Incomplete
  incomplete <- is.na(subset_data$age_group) | is.na(subset_data$ses_gender)

  message(sprintf("    - Complete (all vars): %d", sum(complete_all)))
  message(sprintf("    - Complete (no edu): %d", sum(complete_no_edu)))
  message(sprintf("    - Complete (no lang): %d", sum(complete_no_lang)))
  message(sprintf("    - Basic (age+gender): %d", sum(complete_basic)))
  message(sprintf("    - Incomplete: %d", sum(incomplete)))

  weights_vec <- rep(1, nrow(subset_data))

  # 1. Raking complet
  if (sum(complete_all) > 20) {
    message("    - Applying full raking...")
    w <- apply_raking(subset_data[complete_all, ], margins, use_edu = TRUE, use_lang = TRUE)
    weights_vec[complete_all] <- w
  }

  # 2. Raking sans education
  if (sum(complete_no_edu) > 20) {
    message("    - Applying raking without education...")
    w <- apply_raking(subset_data[complete_no_edu, ], margins, use_edu = FALSE, use_lang = TRUE)
    weights_vec[complete_no_edu] <- w
  }

  # 3. Raking sans language
  if (sum(complete_no_lang) > 20) {
    message("    - Applying raking without language...")
    w <- apply_raking(subset_data[complete_no_lang, ], margins, use_edu = TRUE, use_lang = FALSE)
    weights_vec[complete_no_lang] <- w
  }

  # 4. Raking basique
  if (sum(complete_basic) > 20) {
    message("    - Applying basic raking (age, gender only)...")
    w <- apply_raking(subset_data[complete_basic, ], margins, use_edu = FALSE, use_lang = FALSE)
    weights_vec[complete_basic] <- w
  }

  data$weight[idx] <- weights_vec
}

# =============================================================================
# NORMALISATION ET VALIDATION
# =============================================================================

message("\n\nNormalizing weights...")

# Normaliser les poids pour que la somme = n (par année de sondage)
data <- data %>%
  group_by(year) %>%
  mutate(
    weight_sum = sum(weight, na.rm = TRUE),
    weight_normalized = weight / weight_sum * n()
  ) %>%
  ungroup() %>%
  select(-weight_sum) %>%
  rename(weight_raw = weight, weight = weight_normalized)

# Statistiques des poids
message("\nWeight statistics:")
summary_stats <- summary(data$weight)
print(summary_stats)

# Trimming des poids extrêmes
TRIM_LOWER <- 0.2
TRIM_UPPER <- 5.0

data <- data %>%
  mutate(
    weight_trimmed = case_when(
      weight < TRIM_LOWER ~ TRIM_LOWER,
      weight > TRIM_UPPER ~ TRIM_UPPER,
      TRUE ~ weight
    )
  )

message(sprintf("\nAfter trimming [%.1f, %.1f]:", TRIM_LOWER, TRIM_UPPER))
message(sprintf("  - Observations trimmed low: %d", sum(data$weight < TRIM_LOWER, na.rm = TRUE)))
message(sprintf("  - Observations trimmed high: %d", sum(data$weight > TRIM_UPPER, na.rm = TRUE)))

# =============================================================================
# SAUVEGARDE
# =============================================================================

message("\n\nSaving merged_v2.rds...")

# Sélectionner les colonnes finales
data_final <- data %>%
  select(
    # Colonnes originales
    id, source_id, respondent_id, year,
    ses_age, ses_gender, ses_lang.1, ses_educ,
    ses_family_income_centile_cat, ses_origin_from_canada.1,
    ses_year_canada, ses_religiosity, int_pol, iss_idcan,
    vote_intent_prov, ses_geoloc.1, party_id_prov, iss_souv, iss_souv2,
    # Nouvelles colonnes
    age_group, census_year, weight, weight_raw, weight_trimmed
  )

saveRDS(data_final, file.path(data_dir, "merged_v2.rds"))

message(sprintf("\nSaved: %s", file.path(data_dir, "merged_v2.rds")))

# =============================================================================
# RÉSUMÉ FINAL
# =============================================================================

message("\n========================================")
message("RAKING SUMMARY")
message("========================================")

# Par année de sondage
summary_by_year <- data_final %>%
  group_by(year) %>%
  summarise(
    n = n(),
    census_year = first(census_year),
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight = sd(weight, na.rm = TRUE),
    pct_complete = mean(!is.na(weight)) * 100,
    .groups = "drop"
  )

print(summary_by_year, n = 30)

message("\nDone!")
