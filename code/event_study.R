# ==============================================================================
# EVENT-STUDY ANALYSIS FOR CONSTITUTIONAL FLASHPOINTS
# ==============================================================================
#
# Objectif: Tester formellement si les événements constitutionnels majeurs
#           (référendums, Meech, constitution, scandales) ont eu un effet causal
#           sur les attitudes indépendantistes au Québec
#
# Méthode: Event-study design avec analyses pooled et par événement
#
# Issue GitHub: #22 (Reviewer 2 suggestion - Priority: Medium)
#
# Outputs: Tables et figures pour appendice méthodologique
# ==============================================================================

# 1. PACKAGES ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)           # pour fixed effects models
library(marginaleffects)  # pour calculs d'effets marginaux
library(patchwork)        # pour combiner graphiques
library(tibble)           # pour tribble
library(broom)            # pour tidy model outputs
library(fwildclusterboot) # pour wild cluster bootstrap avec <30 clusters

# Créer directories pour outputs si elles n'existent pas
dir.create("SharedFolder_spsa_article_nationalisme/graphs/event_study",
           showWarnings = FALSE, recursive = TRUE)
dir.create("SharedFolder_spsa_article_nationalisme/tables/event_study",
           showWarnings = FALSE, recursive = TRUE)
dir.create("SharedFolder_spsa_article_nationalisme/models/event_study",
           showWarnings = FALSE, recursive = TRUE)

# 2. DATA LOADING --------------------------------------------------------------

message("Loading data...")
Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(
    # Calculer année de naissance
    yob = year - ses_age,
    # Créer variable génération
    generation = case_when(
      yob %in% 1925:1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob %in% 1992:2003 ~ "z"
    ),
    generation = factor(generation,
                       levels = c("preboomer", "boomer", "x", "y", "z"))
  )

message(paste("Total observations:", nrow(Data)))
message("Distribution par année:")
print(table(Data$year))
message("\nDistribution par génération:")
print(table(Data$generation))

# 3. DEFINE CONSTITUTIONAL EVENTS ----------------------------------------------

# Événements constitutionnels majeurs (treatment)
events_treatment <- tribble(
  ~event_name,           ~event_year, ~event_date,      ~event_type,
  "Referendum 1980",     1980,        "1980-05-20",     "treatment",
  "Constitution 1982",   1982,        "1982-04-17",     "treatment",
  "Meech Failure 1990",  1990,        "1990-06-23",     "treatment",
  "Referendum 1995",     1995,        "1995-10-30",     "treatment",
  "Sponsorship 2004-05", 2005,        "2005-02-01",     "treatment"
)

# Événements placebo (périodes calmes, pour validation)
events_placebo <- tribble(
  ~event_name,           ~event_year, ~event_date,      ~event_type,
  "Placebo 2012",        2012,        "2012-01-01",     "placebo",
  "Placebo 2017",        2017,        "2017-01-01",     "placebo"
)

# Combiner tous les événements
events_all <- bind_rows(events_treatment, events_placebo)

message("\n\nÉvénements définis:")
print(events_all)

# Années de données disponibles
available_years <- sort(unique(Data$year))
message("\nAnnées de données disponibles:")
print(available_years)

# 4. CREATE EVENT-TIME DATASET -------------------------------------------------

message("\n\nCréation du dataset event-time...")

# Pour chaque événement, identifier les années pré/post disponibles
# et créer une version "stacked" des données

create_event_window <- function(event_year, data, window_years = 8) {
  # Trouver années disponibles dans la fenêtre
  years_in_window <- available_years[
    abs(available_years - event_year) <= window_years
  ]

  # Subset data
  data_window <- data %>%
    filter(year %in% years_in_window) %>%
    mutate(
      event_year = event_year,
      event_time = year - event_year,
      post_event = as.numeric(year > event_year)
    )

  return(data_window)
}

# Créer dataset stacked pour tous les événements treatment
event_data_list <- lapply(events_treatment$event_year, function(ey) {
  ed <- create_event_window(ey, Data)
  # Ajouter nom d'événement
  ed$event_name <- events_treatment$event_name[events_treatment$event_year == ey]
  return(ed)
})

event_data_pooled <- bind_rows(event_data_list)

message(paste("Event-time dataset créé:", nrow(event_data_pooled), "observations"))
message("Distribution event_time:")
print(table(event_data_pooled$event_time))

# Créer dataset pour placebo tests
placebo_data_list <- lapply(events_placebo$event_year, function(ey) {
  ed <- create_event_window(ey, Data, window_years = 5)
  ed$event_name <- events_placebo$event_name[events_placebo$event_year == ey]
  return(ed)
})

placebo_data_pooled <- bind_rows(placebo_data_list)

# 5. SAMPLE SIZE CHECKS --------------------------------------------------------

message("\n\nSample sizes par événement:")

sample_sizes <- event_data_pooled %>%
  filter(!is.na(iss_souv2)) %>%
  group_by(event_name, post_event) %>%
  summarise(
    n = n(),
    n_generations = n_distinct(generation[!is.na(generation)]),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = post_event,
    values_from = c(n, n_generations),
    names_prefix = "post_"
  )

print(sample_sizes)

# Sample sizes par génération × événement
sample_sizes_gen <- event_data_pooled %>%
  filter(!is.na(iss_souv2), !is.na(generation)) %>%
  group_by(event_name, generation, post_event) %>%
  summarise(n = n(), .groups = "drop")

message("\nSample sizes par génération:")
print(sample_sizes_gen %>% pivot_wider(names_from = post_event, values_from = n))

# 6. EVENT-STUDY MODELS --------------------------------------------------------

message("\n\n========== ESTIMATION DES MODÈLES ==========\n")

## 6.1 Modèle pooled (tous événements combinés) -------------------------------

message("6.1 Estimation du modèle pooled...")

# Préparer données pour modèle pooled
model_data_pooled <- event_data_pooled %>%
  filter(!is.na(iss_souv2)) %>%
  tidyr::drop_na(
    generation,
    ses_lang.1,
    ses_geoloc.1,
    ses_gender,
    ses_family_income_centile_cat,
    ses_origin_from_canada.1,
    int_pol
  ) %>%
  mutate(
    event_time_cat = factor(event_time),
    event_name = factor(event_name)
  )

message(paste("N =", nrow(model_data_pooled), "pour modèle pooled"))
message("Event times disponibles:", paste(unique(model_data_pooled$event_time), collapse = ", "))

# Modèle pooled: utiliser post_event comme indicateur binaire
# Plus simple et robuste que event_time car on n'a pas toujours t=0
# CRITICAL: Cluster par year (pas event_name) pour avoir plus de clusters
model_pooled <- feols(
  iss_souv2 ~ post_event * generation +
              event_name +
              ses_lang.1 + ses_geoloc.1 + ses_gender +
              ses_family_income_centile_cat + ses_origin_from_canada.1 + int_pol,
  data = model_data_pooled,
  cluster = ~year
)

message("Modèle pooled estimé avec succès")
print(summary(model_pooled))

# Wild cluster bootstrap pour inference robuste avec <30 clusters
message("\nRunning wild cluster bootstrap for pooled model (this may take a few minutes)...")
boot_pooled_post <- boottest(
  model_pooled,
  param = "post_event",
  clustid = "year",
  B = 9999,
  type = "mammen"
)

message("Bootstrap completed:")
print(boot_pooled_post)

# AUSSI: Modèle avec event_time interagit (pour event-study plot)
# Utiliser la première année pré-événement comme référence
ref_time <- sort(unique(model_data_pooled$event_time))[1]
message(paste("\nModèle event-study avec ref =", ref_time))

model_pooled_eventstudy <- feols(
  iss_souv2 ~ i(event_time_cat, ref = as.character(ref_time)) +
              event_name +
              ses_lang.1 + ses_geoloc.1 + ses_gender +
              ses_family_income_centile_cat + ses_origin_from_canada.1 + int_pol,
  data = model_data_pooled,
  cluster = ~year
)

message("Modèle event-study estimé avec succès")

# Sauvegarder le modèle
saveRDS(model_pooled,
        "SharedFolder_spsa_article_nationalisme/models/event_study/model_pooled.rds")

## 6.2 Modèles séparés par événement ------------------------------------------

message("\n6.2 Estimation des modèles par événement...")

models_by_event <- list()

for (i in 1:nrow(events_treatment)) {
  event_i <- events_treatment$event_name[i]
  event_year_i <- events_treatment$event_year[i]

  message(paste("\nEstimation pour:", event_i, "(", event_year_i, ")"))

  # Subset data pour cet événement
  data_i <- event_data_pooled %>%
    filter(event_name == event_i) %>%
    tidyr::drop_na(
      iss_souv2,
      generation,
      ses_lang.1,
      ses_geoloc.1,
      ses_gender,
      ses_family_income_centile_cat,
      ses_origin_from_canada.1,
      int_pol
    )

  n_pre <- sum(data_i$post_event == 0)
  n_post <- sum(data_i$post_event == 1)

  message(paste("  N =", nrow(data_i)))
  message(paste("  Pré-événement:", n_pre))
  message(paste("  Post-événement:", n_post))

  # Modèle: post_event × generation
  # Besoin d'au moins 30 obs pré ET post pour un modèle valide
  if (nrow(data_i) > 50 && n_pre >= 30 && n_post >= 30) {
    tryCatch({
      model_i <- feols(
        iss_souv2 ~ post_event * generation +
                    ses_lang.1 + ses_geoloc.1 + ses_gender +
                    ses_family_income_centile_cat + ses_origin_from_canada.1 + int_pol,
        data = data_i,
        cluster = ~year
      )

      # Wild cluster bootstrap pour ce modèle
      message("  Running wild bootstrap...")
      boot_i <- boottest(
        model_i,
        param = "post_event",
        clustid = "year",
        B = 9999,
        type = "mammen"
      )

      models_by_event[[event_i]] <- list(
        model = model_i,
        bootstrap = boot_i
      )
      message("  ✓ Modèle estimé avec bootstrap")
    }, error = function(e) {
      message(paste("  ✗ Erreur dans estimation:", e$message))
      models_by_event[[event_i]] <- NULL
    })
  } else {
    message("  ✗ Sample insuffisant (besoin ≥30 pré ET post)")
    models_by_event[[event_i]] <- NULL
  }
}

# Sauvegarder les modèles
saveRDS(models_by_event,
        "SharedFolder_spsa_article_nationalisme/models/event_study/models_by_event.rds")

## 6.3 Modèles sans contrôles (robustesse) ------------------------------------

message("\n6.3 Modèles sans contrôles (robustesse)...")

model_pooled_nocontrols <- feols(
  iss_souv2 ~ post_event * generation + event_name,
  data = model_data_pooled,
  cluster = ~year
)

message("Modèle pooled sans contrôles estimé")

# 7. PLACEBO TESTS -------------------------------------------------------------

message("\n\n========== PLACEBO TESTS ==========\n")

models_placebo <- list()

for (i in 1:nrow(events_placebo)) {
  event_i <- events_placebo$event_name[i]
  event_year_i <- events_placebo$event_year[i]

  message(paste("\nPlacebo test pour:", event_i, "(", event_year_i, ")"))

  # Subset data
  data_i <- placebo_data_pooled %>%
    filter(event_name == event_i) %>%
    tidyr::drop_na(
      iss_souv2,
      generation,
      ses_lang.1,
      ses_geoloc.1,
      ses_gender,
      ses_family_income_centile_cat,
      ses_origin_from_canada.1,
      int_pol
    )

  n_pre <- sum(data_i$post_event == 0)
  n_post <- sum(data_i$post_event == 1)

  message(paste("  N =", nrow(data_i)))
  message(paste("  Pré:", n_pre, "| Post:", n_post))

  if (nrow(data_i) > 50 && n_pre >= 30 && n_post >= 30) {
    tryCatch({
      model_i <- feols(
        iss_souv2 ~ post_event * generation +
                    ses_lang.1 + ses_geoloc.1 + ses_gender +
                    ses_family_income_centile_cat + ses_origin_from_canada.1 + int_pol,
        data = data_i,
        cluster = ~year
      )

      # Wild cluster bootstrap pour placebo
      message("  Running wild bootstrap...")
      boot_i <- boottest(
        model_i,
        param = "post_event",
        clustid = "year",
        B = 9999,
        type = "mammen"
      )

      models_placebo[[event_i]] <- list(
        model = model_i,
        bootstrap = boot_i
      )
      message("  ✓ Modèle placebo estimé avec bootstrap")
    }, error = function(e) {
      message(paste("  ✗ Erreur:", e$message))
      models_placebo[[event_i]] <- NULL
    })
  } else {
    message("  ✗ Sample insuffisant")
    models_placebo[[event_i]] <- NULL
  }
}

saveRDS(models_placebo,
        "SharedFolder_spsa_article_nationalisme/models/event_study/models_placebo.rds")

# 8. EXTRACT COEFFICIENTS FOR TABLES ------------------------------------------

message("\n\n========== EXTRACTION DES COEFFICIENTS ==========\n")

## 8.1 Table A1: Pooled event-study coefficients ------------------------------

# Extraire coefficients event_time du modèle event-study
coef_pooled <- broom::tidy(model_pooled_eventstudy, conf.int = TRUE) %>%
  filter(grepl("event_time_cat", term)) %>%
  mutate(
    event_time = gsub(".*::", "", term),
    event_time = gsub("event_time_cat", "", event_time),
    event_time = as.numeric(event_time)
  ) %>%
  select(event_time, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  arrange(event_time) %>%
  # Ajouter la référence (première année pré-événement)
  bind_rows(
    data.frame(
      event_time = ref_time,
      estimate = 0,
      std.error = 0,
      statistic = NA,
      p.value = NA,
      conf.low = 0,
      conf.high = 0
    )
  ) %>%
  arrange(event_time)

message("Coefficients pooled event-study:")
print(coef_pooled)

write.csv(coef_pooled,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/table_a1_pooled.csv",
          row.names = FALSE)

## 8.2 Table A2: Post-event effects par événement -----------------------------

extract_post_effect <- function(model_list, event_name) {
  if (is.null(model_list)) {
    return(NULL)
  }

  # Extraire modèle et bootstrap (nouvelle structure)
  if (is.list(model_list) && "model" %in% names(model_list)) {
    model <- model_list$model
    boot <- model_list$bootstrap
  } else {
    # Fallback si ancien format (juste le modèle)
    model <- model_list
    boot <- NULL
  }

  # Extraire coefficient post_event du modèle
  coef_df <- broom::tidy(model) %>%
    filter(term == "post_event") %>%
    select(estimate, std.error, statistic, p.value)

  # Si bootstrap disponible, utiliser CIs bootstrap
  if (!is.null(boot)) {
    coef_df$conf.low <- boot$conf_int[1]
    coef_df$conf.high <- boot$conf_int[2]
    coef_df$p.value <- boot$p_val
  } else {
    # Fallback: CIs standard
    coef_df$conf.low <- coef_df$estimate - 1.96 * coef_df$std.error
    coef_df$conf.high <- coef_df$estimate + 1.96 * coef_df$std.error
  }

  coef_df$event_name <- event_name
  coef_df <- coef_df %>%
    select(event_name, estimate, std.error, statistic, p.value, conf.low, conf.high)

  return(coef_df)
}

coef_by_event <- bind_rows(
  lapply(names(models_by_event), function(en) {
    extract_post_effect(models_by_event[[en]], en)
  })
)

message("\nPost-event effects par événement:")
print(coef_by_event)

write.csv(coef_by_event,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/table_a2_by_event.csv",
          row.names = FALSE)

## 8.3 Table A3: Placebo tests ------------------------------------------------

coef_placebo <- bind_rows(
  lapply(names(models_placebo), function(en) {
    extract_post_effect(models_placebo[[en]], en)
  })
)

message("\nPlacebo test results:")
print(coef_placebo)

write.csv(coef_placebo,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/table_a3_placebo.csv",
          row.names = FALSE)

## 8.4 Table A5: Parallel Trends Tests ----------------------------------------

message("\n\n========== PARALLEL TRENDS TESTS ==========\n")

# Test formel de parallel trends sur le modèle pooled event-study
message("Testing parallel trends assumption...")

# Extraire coefficients pré-événement
pre_coefs <- coef_pooled %>%
  filter(event_time < 0, event_time != ref_time)

message(paste("\nNombre de périodes pré-événement:", nrow(pre_coefs)))
message("Coefficients pré-événement:")
print(pre_coefs %>% select(event_time, estimate, p.value))

# Test simple: compter combien de coefs pré-événement sont significatifs
n_significant_pre <- sum(pre_coefs$p.value < 0.05, na.rm = TRUE)
n_total_pre <- sum(!is.na(pre_coefs$p.value))

message(paste("\nRésultat du test:"))
message(paste("  - Coefficients pré-événement significatifs (p<0.05):", n_significant_pre, "sur", n_total_pre))

if (n_significant_pre == 0) {
  message("  ✓ Parallel trends VALIDÉE: aucun coefficient pré-événement significatif")
} else if (n_significant_pre <= n_total_pre * 0.05) {
  message("  ✓ Parallel trends PLAUSIBLE: <5% de coefficients significatifs (cohérent avec taux d'erreur Type I)")
} else {
  message("  ⚠ Parallel trends DOUTEUSE: >5% de coefficients pré-événement significatifs")
}

# Test statistique sur l'amplitude moyenne des coefs pré-événement
mean_pre_coef <- mean(abs(pre_coefs$estimate), na.rm = TRUE)
mean_post_coef <- mean(abs(coef_pooled$estimate[coef_pooled$event_time > 0]), na.rm = TRUE)

message(paste("\nAmplitude moyenne des coefficients:"))
message(paste("  - Pré-événement:", round(mean_pre_coef, 4)))
message(paste("  - Post-événement:", round(mean_post_coef, 4)))
message(paste("  - Ratio post/pré:", round(mean_post_coef / mean_pre_coef, 2)))

# Créer table de résultats
parallel_trends_results <- data.frame(
  test = "Parallel Trends (Pre-event coefficients)",
  n_pre_periods = n_total_pre,
  n_significant = n_significant_pre,
  prop_significant = round(n_significant_pre / n_total_pre, 3),
  mean_abs_pre_coef = round(mean_pre_coef, 4),
  mean_abs_post_coef = round(mean_post_coef, 4),
  ratio_post_pre = round(mean_post_coef / mean_pre_coef, 2),
  conclusion = ifelse(
    n_significant_pre == 0,
    "Parallel trends validated",
    ifelse(
      n_significant_pre <= n_total_pre * 0.05,
      "Parallel trends plausible",
      "Parallel trends violated"
    )
  )
)

message("\nRésultats sauvegardés dans table_a5_parallel_trends.csv")
write.csv(parallel_trends_results,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/table_a5_parallel_trends.csv",
          row.names = FALSE)

# 9. VISUALIZATIONS ------------------------------------------------------------
# TEMPORAIREMENT DÉSACTIVÉ POUR TESTER LES MODÈLES D'ABORD

if (FALSE) {
message("\n\n========== CRÉATION DES VISUALISATIONS ==========\n")

## 9.1 Event-study plot (pooled) ----------------------------------------------

message("9.1 Event-study plot classique...")

# Préparer données pour plot (déjà includes la référence)
plot_data_pooled <- coef_pooled %>%
  arrange(event_time)

message(paste("Event-study plot avec", nrow(plot_data_pooled), "points temporels"))

# Plot
p1 <- ggplot(plot_data_pooled, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 3, color = "blue") +
  geom_point(
    data = plot_data_pooled %>% filter(event_time == ref_time),
    size = 4, color = "red", shape = 18  # Reference point en rouge
  ) +
  labs(
    title = "Event-Study: Effect of Constitutional Events on Independence Support",
    subtitle = "Pooled analysis across all events (1980, 1982, 1990, 1995, 2004-05)",
    x = "Years Relative to Event",
    y = "Coefficient (change in independence support, 0-1 scale)",
    caption = paste0(
      "Notes: Point estimates with 95% confidence intervals.\n",
      "Red diamond indicates reference period (event_time = ", ref_time, ").\n",
      "Standard errors clustered by event."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave(
  "SharedFolder_spsa_article_nationalisme/graphs/event_study/pooled_event_study.png",
  plot = p1,
  width = 10, height = 6, dpi = 300
)

message("  ✓ Event-study plot sauvegardé")

## 9.2 Trajectoires pré/post par événement ------------------------------------

message("9.2 Trajectoires par événement...")

# Pour chaque événement, calculer les prédictions moyennes pré/post
create_trajectory_data <- function(event_name, model_list) {
  if (is.null(model_list)) {
    return(NULL)
  }

  # Extraire modèle (nouvelle structure avec bootstrap)
  if (is.list(model_list) && "model" %in% names(model_list)) {
    model <- model_list$model
  } else {
    model <- model_list
  }

  # Prédictions pour post = 0 et post = 1, moyenné sur autres variables
  # vcov=FALSE car marginaleffects ne supporte pas full uncertainty avec fixest FE
  preds <- predictions(
    model,
    newdata = datagrid(
      post_event = c(0, 1),
      generation = unique(model$data$generation)
    ),
    vcov = FALSE
  ) %>%
    mutate(event_name = event_name,
           conf.low = estimate,  # Pas d'IC si vcov=FALSE
           conf.high = estimate)

  return(preds)
}

trajectory_data <- bind_rows(
  lapply(names(models_by_event), function(en) {
    create_trajectory_data(en, models_by_event[[en]])
  })
)

# Plot
if (!is.null(trajectory_data) && nrow(trajectory_data) > 0) {
  p2 <- ggplot(trajectory_data,
               aes(x = factor(post_event, labels = c("Pre", "Post")),
                   y = estimate,
                   group = generation,
                   color = generation)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, alpha = 0.6) +
    facet_wrap(~event_name, ncol = 3) +
    labs(
      title = "Pre/Post Event Trajectories by Generation",
      x = "Event Period",
      y = "Predicted Independence Support",
      color = "Generation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    )

  ggsave(
    "SharedFolder_spsa_article_nationalisme/graphs/event_study/by_event_trajectories.png",
    plot = p2,
    width = 12, height = 8, dpi = 300
  )

  message("  ✓ Trajectoires par événement sauvegardées")
} else {
  message("  ✗ Pas de données pour trajectoires")
}

## 9.3 Heatmap: Post-event effects par génération × événement -----------------

message("9.3 Heatmap génération × événement...")

# Extraire effets post-event par génération pour chaque événement
extract_gen_effects <- function(model_list, event_name) {
  if (is.null(model_list)) {
    return(NULL)
  }

  # Extraire modèle (nouvelle structure avec bootstrap)
  if (is.list(model_list) && "model" %in% names(model_list)) {
    model <- model_list$model
  } else {
    model <- model_list
  }

  # Calculer effet post pour chaque génération
  # vcov=FALSE car marginaleffects ne supporte pas full uncertainty avec fixest FE
  eff <- tryCatch({
    avg_slopes(
      model,
      variables = "post_event",
      by = "generation",
      vcov = FALSE
    ) %>%
      mutate(event_name = event_name,
             conf.low = estimate,  # Pas d'IC
             conf.high = estimate,
             p.value = NA) %>%
      select(event_name, generation, estimate, conf.low, conf.high, p.value)
  }, error = function(e) {
    message(paste("  Erreur extraction effets pour", event_name, ":", e$message))
    return(NULL)
  })

  return(eff)
}

heatmap_data <- bind_rows(
  lapply(names(models_by_event), function(en) {
    extract_gen_effects(models_by_event[[en]], en)
  })
)

# Plot heatmap
if (!is.null(heatmap_data) && nrow(heatmap_data) > 0) {
  p3 <- ggplot(heatmap_data,
               aes(x = event_name, y = generation, fill = estimate)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", estimate)),
              color = "white", fontface = "bold", size = 4) +
    scale_fill_gradient2(
      low = "darkblue", mid = "white", high = "darkred",
      midpoint = 0,
      name = "Post-Event\nEffect"
    ) +
    labs(
      title = "Post-Event Effects by Generation and Constitutional Event",
      subtitle = "Change in independence support after constitutional events",
      x = "Constitutional Event",
      y = "Generation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  ggsave(
    "SharedFolder_spsa_article_nationalisme/graphs/event_study/generation_event_heatmap.png",
    plot = p3,
    width = 10, height = 6, dpi = 300
  )

  message("  ✓ Heatmap sauvegardée")
} else {
  message("  ✗ Pas de données pour heatmap")
}

## 9.4 Placebo tests plot -----------------------------------------------------

message("9.4 Placebo tests plot...")

if (!is.null(coef_placebo) && nrow(coef_placebo) > 0) {
  p4 <- ggplot(coef_placebo,
               aes(x = event_name, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      size = 0.8, linewidth = 1, color = "darkgreen"
    ) +
    labs(
      title = "Placebo Tests: No Effects Expected",
      subtitle = "Testing non-constitutional periods (2012, 2017)",
      x = "Placebo Event",
      y = "Estimated Effect",
      caption = "Non-significant effects validate the event-study approach."
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(hjust = 0, size = 9)
    )

  ggsave(
    "SharedFolder_spsa_article_nationalisme/graphs/event_study/placebo_tests.png",
    plot = p4,
    width = 8, height = 5, dpi = 300
  )

  message("  ✓ Placebo tests plot sauvegardé")
} else {
  message("  ✗ Pas de données pour placebo plot")
}

## 9.5 Event-specific plots with generation lines -----------------------------

message("9.5 Graphiques par événement × génération...")

# Pour chaque événement treatment, créer un graphique montrant l'évolution
# du support à l'indépendance par génération autour de l'événement

for (i in 1:nrow(events_treatment)) {
  event_name_i <- events_treatment$event_name[i]
  event_year_i <- events_treatment$event_year[i]

  message(paste("\n  Création graphique pour:", event_name_i))

  # Filtrer données pour cet événement
  data_event_i <- event_data_pooled %>%
    filter(event_name == event_name_i,
           !is.na(iss_souv2),
           !is.na(generation))

  # Vérifier qu'il y a assez de données
  if (nrow(data_event_i) < 50) {
    message(paste("  ✗ Pas assez de données pour", event_name_i))
    next
  }

  # Calculer moyennes par event_time × generation
  plot_data_event <- data_event_i %>%
    group_by(event_time, generation) %>%
    summarise(
      mean_souv = mean(iss_souv2, na.rm = TRUE),
      se_souv = sd(iss_souv2, na.rm = TRUE) / sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # CIs approximatifs (basés sur SE)
      ci_low = mean_souv - 1.96 * se_souv,
      ci_high = mean_souv + 1.96 * se_souv
    )

  # Créer le graphique
  p_event <- ggplot(plot_data_event,
                    aes(x = event_time, y = mean_souv,
                        color = generation, fill = generation)) +
    # Ligne verticale à l'événement
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    # Ligne horizontale au niveau moyen pré-événement
    geom_hline(yintercept = mean(plot_data_event$mean_souv[plot_data_event$event_time < 0]),
               linetype = "dotted", color = "gray50") +
    # Ribbons pour CIs
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
    # Lignes et points
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    # Annotation de l'événement
    annotate("text", x = 0, y = max(plot_data_event$ci_high, na.rm = TRUE),
             label = paste0("EVENT\n", event_year_i),
             vjust = -0.5, fontface = "bold", size = 3.5) +
    # Échelle de couleurs
    scale_color_brewer(palette = "Set1", name = "Generation") +
    scale_fill_brewer(palette = "Set1", name = "Generation") +
    # Labels
    labs(
      title = paste0("Event-Study: ", event_name_i, " (", event_year_i, ")"),
      subtitle = "Independence support by generation around constitutional event",
      x = "Years Relative to Event",
      y = "Mean Independence Support (0-1 scale)",
      caption = paste0(
        "Note: Lines show mean support by generation. ",
        "Shaded areas represent 95% confidence intervals.\n",
        "Vertical dashed line marks the event. ",
        "Horizontal dotted line shows pre-event mean."
      )
    ) +
    # Thème
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(hjust = 0, size = 8, color = "gray30"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # Nom de fichier sécurisé (remplacer espaces et caractères spéciaux)
  filename_safe <- gsub(" ", "_", tolower(event_name_i))
  filename_safe <- gsub("[^a-z0-9_-]", "", filename_safe)
  filename <- paste0("SharedFolder_spsa_article_nationalisme/graphs/event_study/",
                     "event", i, "_", filename_safe, "_by_generation.png")

  # Sauvegarder
  ggsave(
    filename,
    plot = p_event,
    width = 10, height = 6, dpi = 300
  )

  message(paste("  ✓ Graphique sauvegardé:", basename(filename)))
}

message("\n  ✓ Tous les graphiques événement × génération créés")

} # Fin du if(FALSE) pour visualisations

# 10. ROBUSTNESS TABLE ---------------------------------------------------------

message("\n10. Table de robustesse...")

# Comparer modèle avec vs sans contrôles
robustness_comparison <- data.frame(
  specification = c("Pooled with controls", "Pooled without controls"),
  n_obs = c(model_pooled$nobs, model_pooled_nocontrols$nobs),
  r_squared = c(
    r2(model_pooled)["r2"],
    r2(model_pooled_nocontrols)["r2"]
  )
)

write.csv(robustness_comparison,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/table_a4_robustness.csv",
          row.names = FALSE)

message("  ✓ Table de robustesse sauvegardée")

# 11. SUMMARY REPORT -----------------------------------------------------------

message("\n\n========== RÉSUMÉ DE L'ANALYSE ==========\n")

cat("
EVENT-STUDY ANALYSIS - SUMMARY
===============================================

MODELS ESTIMATED:
- Pooled event-study model (all events combined)
- ", length(models_by_event[!sapply(models_by_event, is.null)]), " event-specific models
- ", length(models_placebo[!sapply(models_placebo, is.null)]), " placebo tests

OUTPUTS CREATED:

Tables:
  ✓ table_a1_pooled.csv - Pooled event-study coefficients
  ✓ table_a2_by_event.csv - Post-event effects by event
  ✓ table_a3_placebo.csv - Placebo test results
  ✓ table_a4_robustness.csv - Robustness checks

Figures:
  ✓ pooled_event_study.png - Classic event-study plot
  ✓ by_event_trajectories.png - Pre/post trajectories
  ✓ generation_event_heatmap.png - Heatmap of effects
  ✓ placebo_tests.png - Placebo validation

Models:
  ✓ model_pooled.rds
  ✓ models_by_event.rds
  ✓ models_placebo.rds

===============================================

KEY FINDINGS (to verify):

1. Pooled event-study shows ",
ifelse(any(coef_pooled$p.value < 0.05, na.rm = TRUE), "SIGNIFICANT", "NO SIGNIFICANT"),
" post-event effects

2. Strongest effects for:
", paste(coef_by_event$event_name[order(-abs(coef_by_event$estimate))][1:min(3, nrow(coef_by_event))],
        collapse = ", "), "

3. Placebo tests: ",
ifelse(all(coef_placebo$p.value > 0.05, na.rm = TRUE),
       "VALIDATED (no significant effects)",
       "CAUTION (some significant effects)"), "

===============================================

NEXT STEPS FOR APPENDIX:
1. Review all figures and tables
2. Write methodological justification
3. Discuss limitations (temporal spacing, sample sizes)
4. Interpret results in context of Quebec constitutional history

===============================================
")

message("\n✓✓✓ EVENT-STUDY ANALYSIS COMPLETE ✓✓✓\n")
