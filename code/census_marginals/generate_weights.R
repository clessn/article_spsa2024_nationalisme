## generate_weights.R
## Applique le raking au dataset merged_v1.rds pour créer merged_v2.rds
##
## STRATÉGIE DE RAKING SÉQUENTIEL:
## 1. Associer chaque observation à l'année de recensement précédente la plus proche
## 2. Convertir l'âge en groupes d'âge compatibles avec les marginals
## 3. Appliquer le raking SÉQUENTIEL avec survey::rake()
##    - Étape 1: Age × Gender (distribution conjointe)
##    - Étape 2: Language (distribution marginale)
##    - Éducation: NON corrigée par weighting, sera contrôlée en régression
##
## Justification: Le raking simultané sur age×gender×language×education produit
## des poids instables (ESS < 45%) pour les années récentes (2015, 2021-2023).
## Le raking séquentiel maintient l'efficacité statistique (ESS > 65%) tout en
## corrigeant les biais clés. Voir doc/weighting.md pour détails.

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

# Trimming adaptatif selon l'année
TRIM_LOWER <- 0.2  # Valeur par défaut
TRIM_UPPER <- 5.0  # Valeur par défaut
TRIM_LOWER_AGG <- 0.5  # Plus agressif pour années problématiques
TRIM_UPPER_AGG <- 2.5  # Plus agressif pour années problématiques

# Années nécessitant trimming agressif (haute variance)
AGGRESSIVE_TRIM_YEARS <- c(2015, 2021, 2022, 2023)

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
  # CHANGEMENT: Toujours utiliser la distribution marginale (pas par genre)
  # pour le raking séquentiel
  lang_m <- m %>% filter(variable == "language")

  lang_margins <- NULL
  if (nrow(lang_m) > 0) {
    # Agréger la langue sur tous les genres
    lang_margins <- lang_m %>%
      group_by(category) %>%
      summarise(Freq = sum(n), .groups = "drop") %>%
      rename(ses_lang.1 = category)
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

#' Applique le raking SÉQUENTIEL à un sous-ensemble de données
#' Étape 1: Age × Gender (distribution conjointe)
#' Étape 2: Language (distribution marginale)
#' Retourne les poids calculés
apply_sequential_raking <- function(data_subset, margins) {

  # Vérifier qu'on a les données nécessaires
  if (is.null(margins$age_gender) || nrow(margins$age_gender) == 0) {
    message("    No age×gender margins available, returning uniform weights")
    return(rep(1, nrow(data_subset)))
  }

  # Filtrer les observations avec age et gender valides
  valid_obs <- !is.na(data_subset$age_group) & !is.na(data_subset$ses_gender)
  if (sum(valid_obs) < 20) {
    message("    Too few valid observations, returning uniform weights")
    return(rep(1, nrow(data_subset)))
  }

  data_valid <- data_subset[valid_obs, ]
  data_valid$init_weight <- 1

  tryCatch({
    # ÉTAPE 1: Raking Age × Gender
    des <- svydesign(ids = ~1, weights = ~init_weight, data = data_valid)

    # Filtrer les marges pour ne garder que les combinaisons présentes
    sample_combs <- data_valid %>%
      select(age_group, ses_gender) %>%
      distinct() %>%
      drop_na()

    pop_age_gender <- margins$age_gender %>%
      semi_join(sample_combs, by = c("age_group", "ses_gender"))

    if (nrow(pop_age_gender) == 0) {
      message("    No matching age×gender cells, returning uniform weights")
      return(rep(1, nrow(data_subset)))
    }

    raked1 <- rake(
      design = des,
      sample.margins = list(~age_group + ses_gender),
      population.margins = list(pop_age_gender),
      control = list(maxit = RAKE_MAXIT, epsilon = RAKE_EPSILON)
    )

    message("    Step 1 (Age×Gender): converged")

    # ÉTAPE 2: Raking Language (si disponible)
    if (!is.null(margins$lang) && nrow(margins$lang) > 0) {

      # Vérifier qu'on a des observations avec langue valide
      valid_lang_idx <- which(!is.na(data_valid$ses_lang.1))
      if (length(valid_lang_idx) >= 20) {

        # Filtrer les marges de langue
        sample_langs <- data_valid %>%
          filter(!is.na(ses_lang.1)) %>%
          distinct(ses_lang.1)

        pop_lang <- margins$lang %>%
          semi_join(sample_langs, by = "ses_lang.1")

        if (nrow(pop_lang) > 0) {
          # Créer un subset avec seulement les observations ayant une langue
          data_with_lang <- data_valid[valid_lang_idx, ]
          weights_after_step1 <- weights(raked1)[valid_lang_idx]

          # Créer nouveau design avec les poids de l'étape 1
          des_lang <- svydesign(
            ids = ~1,
            weights = ~weights_after_step1,
            data = data_with_lang
          )

          raked2 <- rake(
            design = des_lang,
            sample.margins = list(~ses_lang.1),
            population.margins = list(pop_lang),
            control = list(maxit = RAKE_MAXIT, epsilon = RAKE_EPSILON)
          )

          message("    Step 2 (Language): converged")

          # Extraire les poids finaux
          weights_out <- rep(1, nrow(data_subset))
          weights_final <- weights(raked1)  # Poids de l'étape 1 pour tous
          weights_final[valid_lang_idx] <- weights(raked2)  # Remplacer par étape 2 où disponible
          weights_out[valid_obs] <- weights_final
          return(weights_out)
        }
      }
    }

    # Si pas de language raking, retourner les poids après étape 1
    message("    Step 2 (Language): skipped")
    weights_out <- rep(1, nrow(data_subset))
    weights_out[valid_obs] <- weights(raked1)
    return(weights_out)

  }, error = function(e) {
    message(paste("    Sequential raking failed:", e$message))
    return(rep(1, nrow(data_subset)))
  })
}

#' Calcule l'Effective Sample Size
calculate_ess <- function(weights) {
  w <- weights[!is.na(weights)]
  if (length(w) == 0) return(NA)
  sum(w)^2 / sum(w^2)
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
# APPLICATION DU RAKING SÉQUENTIEL
# =============================================================================

message("\n")
message("========================================")
message("APPLYING SEQUENTIAL RAKING")
message("========================================")
message("\nStrategy: Age×Gender + Language")
message("Education will be controlled via regression\n")

# Initialiser les poids
data$weight <- NA_real_

# Traiter par année de recensement
census_years_in_data <- sort(unique(data$census_year))

for (cy in census_years_in_data) {

  message(sprintf("\n  Processing census year %d...", cy))

  # Subset pour cette année de recensement
  idx <- which(data$census_year == cy)
  subset_data <- data[idx, ]

  survey_years <- sort(unique(subset_data$year))
  message(sprintf("  Survey years: %s", paste(survey_years, collapse = ", ")))
  message(sprintf("  Total observations: %d", length(idx)))

  # Préparer les marges
  margins <- prepare_margins(marginals, cy)

  # Appliquer le raking séquentiel (Age×Gender + Language)
  weights_vec <- apply_sequential_raking(subset_data, margins)

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

# Trimming adaptatif des poids extrêmes
message("\nTrimming weights...")
message(sprintf("  Default years: [%.1f, %.1f]", TRIM_LOWER, TRIM_UPPER))
message(sprintf("  Aggressive years (%s): [%.1f, %.1f]",
                paste(AGGRESSIVE_TRIM_YEARS, collapse=", "),
                TRIM_LOWER_AGG, TRIM_UPPER_AGG))

data <- data %>%
  mutate(
    trim_lower = ifelse(year %in% AGGRESSIVE_TRIM_YEARS, TRIM_LOWER_AGG, TRIM_LOWER),
    trim_upper = ifelse(year %in% AGGRESSIVE_TRIM_YEARS, TRIM_UPPER_AGG, TRIM_UPPER),
    weight_trimmed = case_when(
      weight < trim_lower ~ trim_lower,
      weight > trim_upper ~ trim_upper,
      TRUE ~ weight
    )
  )

trim_stats <- data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    n_trimmed_low = sum(weight < first(trim_lower), na.rm = TRUE),
    n_trimmed_high = sum(weight > first(trim_upper), na.rm = TRUE),
    pct_trimmed = 100 * (n_trimmed_low + n_trimmed_high) / n,
    .groups = "drop"
  ) %>%
  filter(pct_trimmed > 0)

if (nrow(trim_stats) > 0) {
  message("\nTrimming summary:")
  print(trim_stats, n = 25)
} else {
  message("  No observations trimmed")
}

# RE-NORMALISER après trimming pour corriger le biais introduit
message("\nRe-normalizing after trimming...")
data <- data %>%
  group_by(year) %>%
  mutate(
    weight_trimmed = weight_trimmed * n() / sum(weight_trimmed, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-trim_lower, -trim_upper)

message("  Weights re-normalized to sum = n by year")

# =============================================================================
# CALCUL DE L'EFFECTIVE SAMPLE SIZE (ESS)
# =============================================================================

message("\n\nCalculating Effective Sample Size (ESS)...")

ess_summary <- data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    ess_raw = calculate_ess(weight_raw),
    ess_normalized = calculate_ess(weight),
    ess_trimmed = calculate_ess(weight_trimmed),
    efficiency_trimmed = ess_trimmed / n,
    .groups = "drop"
  )

message("\nESS by year:")
print(ess_summary, n = 25)

message("\nYears with efficiency < 60%:")
low_eff <- ess_summary %>%
  filter(efficiency_trimmed < 0.60) %>%
  arrange(efficiency_trimmed)

if (nrow(low_eff) > 0) {
  print(low_eff)
} else {
  message("  None (all years have good efficiency)")
}

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

# Ajouter les ESS au dataset
data_final <- data_final %>%
  left_join(
    ess_summary %>% select(year, ess_trimmed, efficiency_trimmed),
    by = "year"
  )

saveRDS(data_final, file.path(data_dir, "merged_v2.rds"))

message(sprintf("Saved: %s", file.path(data_dir, "merged_v2.rds")))

# =============================================================================
# RÉSUMÉ FINAL
# =============================================================================

message("\n")
message("========================================")
message("FINAL SUMMARY")
message("========================================\n")

final_summary <- data_final %>%
  group_by(year) %>%
  summarise(
    n = n(),
    census_year = first(census_year),
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight = sd(weight, na.rm = TRUE),
    mean_weight_trimmed = mean(weight_trimmed, na.rm = TRUE),
    sd_weight_trimmed = sd(weight_trimmed, na.rm = TRUE),
    ess = first(ess_trimmed),
    efficiency = first(efficiency_trimmed),
    .groups = "drop"
  )

print(final_summary, n = 30)

message("\n✓ Sequential raking complete!")
message("  Strategy: Age×Gender + Language")
message("  Education controlled via regression")
message("  See doc/weighting.md for methodological justification")
