# ==============================================================================
# EVENT-STUDY: PARALLEL TRENDS TEST
# ==============================================================================
# Objectif: Tester l'hypothèse de parallel trends pour chaque événement
#           constitutionnel en estimant des coefficients par année relative
#
# Modèle: iss_souv ~ event_time_f + generation + ses_lang.1
#
# Output: Figure event-study pour 1995 + table des coefficients pré-événement
# ==============================================================================

library(dplyr)
library(ggplot2)
library(broom)

# 1. CHARGER DONNÉES -----------------------------------------------------------

Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(
    yob = year - ses_age,
    generation = case_when(
      yob <= 1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob >= 1992 ~ "z"
    ),
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z"))
  )

# 2. DÉFINIR ÉVÉNEMENTS --------------------------------------------------------

events <- list(
  # Treatment events
  ref_1980 = list(
    name = "Referendum 1980",
    year = 1980,
    pre_years = c(1974, 1979),
    post_years = c(1984, 1988),
    generations = c("preboomer", "boomer"),
    type = "treatment"
  ),
  meech_1990 = list(
    name = "Meech Lake 1990",
    year = 1990,
    pre_years = c(1984, 1988),
    post_years = c(1993, 1997),
    generations = c("preboomer", "boomer", "x"),
    type = "treatment"
  ),
  ref_1995 = list(
    name = "Referendum 1995",
    year = 1995,
    pre_years = c(1988, 1993),
    post_years = c(1997, 2000, 2004),
    generations = c("preboomer", "boomer", "x"),
    type = "treatment"
  ),
  sponsorship_2005 = list(
    name = "Sponsorship 2005",
    year = 2005,
    pre_years = c(2000, 2004),
    post_years = c(2006, 2008, 2011),
    generations = c("preboomer", "boomer", "x", "y"),
    type = "treatment"
  )
)

# 3. FONCTION EVENT-STUDY ------------------------------------------------------

run_event_study <- function(event, data) {

  # Filtrer données
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    mutate(
      event_time = year - event$year,
      event_time_f = factor(event_time)
    )

  # Debug: afficher années disponibles
  message(paste("  Years available:", paste(sort(unique(data_event$year)), collapse = ", ")))
  message(paste("  N =", nrow(data_event)))

  # Référence = dernière année pré-événement DISPONIBLE
  available_pre_years <- intersect(event$pre_years, unique(data_event$year))
  if (length(available_pre_years) == 0) {
    message("  WARNING: No pre-event data available!")
    return(NULL)
  }
  ref_year <- max(available_pre_years)
  ref_time <- ref_year - event$year
  message(paste("  Reference year:", ref_year, "(t =", ref_time, ")"))

  data_event$event_time_f <- relevel(data_event$event_time_f, ref = as.character(ref_time))

  # Estimer modèle (sans interaction, generation comme contrôle)
  model <- lm(iss_souv ~ event_time_f + generation + ses_lang.1, data = data_event)

  # Extraire coefficients
  coefs <- tidy(model, conf.int = TRUE) %>%
    filter(grepl("^event_time_f", term)) %>%
    filter(!grepl(":", term)) %>%  # Seulement effets principaux
    mutate(
      event_time = as.numeric(gsub("event_time_f", "", term)),
      ref_time = ref_time,
      event_name = event$name,
      event_type = event$type
    )

  # Ajouter point de référence
  coefs <- bind_rows(
    coefs,
    data.frame(
      term = paste0("event_time_f", ref_time),
      estimate = 0,
      std.error = 0,
      statistic = NA,
      p.value = NA,
      conf.low = 0,
      conf.high = 0,
      event_time = ref_time,
      ref_time = ref_time,
      event_name = event$name,
      event_type = event$type
    )
  ) %>%
    arrange(event_time)

  # Sample sizes
  n_info <- data_event %>%
    group_by(event_time) %>%
    summarise(n = n(), .groups = "drop")

  return(list(
    model = model,
    coefs = coefs,
    n_info = n_info,
    data = data_event
  ))
}

# 4. ESTIMER TOUS LES ÉVÉNEMENTS -----------------------------------------------

results <- lapply(events, function(e) {
  message(paste("\nRunning event-study for:", e$name))
  run_event_study(e, Data)
})

# Enlever les NULL
results <- results[!sapply(results, is.null)]

# 5. COMPILER RÉSULTATS PARALLEL TRENDS ----------------------------------------

# Extraire coefficients pré-événement pour tous les événements
# (exclure la référence qui a estimate = 0 par définition)
parallel_trends_test <- bind_rows(lapply(names(results), function(n) {
  coefs <- results[[n]]$coefs
  coefs %>%
    filter(event_time < 0, estimate != 0) %>%  # Pré-événement, exclure référence
    select(event_name, event_time, ref_time, estimate, std.error, p.value, conf.low, conf.high)
}))

message("\n=== PARALLEL TRENDS TEST ===")
message("Pre-event coefficients (should be ~0 if parallel trends holds):\n")
print(parallel_trends_test)

# Résumé: combien de coefs significatifs?
n_sig <- sum(parallel_trends_test$p.value < 0.05, na.rm = TRUE)
n_total <- sum(!is.na(parallel_trends_test$p.value))
message(paste("\nSignificant pre-event coefficients:", n_sig, "/", n_total))

# Sauvegarder
dir.create("SharedFolder_spsa_article_nationalisme/tables/event_study",
           showWarnings = FALSE, recursive = TRUE)
write.csv(parallel_trends_test,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/parallel_trends_test.csv",
          row.names = FALSE)

# 6. FIGURE EVENT-STUDY POUR 1995 ----------------------------------------------

message("\nCreating event-study figure for 1995 referendum...")

coefs_1995 <- results$ref_1995$coefs

p <- ggplot(coefs_1995, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_point(data = coefs_1995 %>% filter(estimate == 0),
             color = "red", size = 4, shape = 18) +
  labs(
    title = "Event-Study: 1995 Quebec Referendum",
    subtitle = "Change in independence support relative to 1993 (t=-2)",
    x = "Years relative to referendum",
    y = "Coefficient (change in support, 0-1 scale)",
    caption = "Note: Red diamond = reference period. Shaded area = 95% CI.\nPre-event coefficients near zero support parallel trends assumption."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40")
  )

dir.create("SharedFolder_spsa_article_nationalisme/graphs/event_study",
           showWarnings = FALSE, recursive = TRUE)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/event_study/parallel_trends_1995.png",
       plot = p, width = 9, height = 6, dpi = 300)

message("Figure saved: parallel_trends_1995.png")

# 7. RÉSUMÉ --------------------------------------------------------------------

message("\n=== SUMMARY ===")
message("Event-study models estimated for 4 treatment events")
message("Parallel trends test saved to: tables/event_study/parallel_trends_test.csv")
message("Event-study figure saved to: graphs/event_study/parallel_trends_1995.png")
