# ==============================================================================
# EVENT-STUDY: DIFFERENCE-IN-DIFFERENCES
# ==============================================================================
# Objectif: Estimer l'effet post-événement pour chaque événement constitutionnel
#           et valider avec des placebos
#
# Modèle: iss_souv ~ post_event * generation + ses_lang.1
#
# Output: Table des coefficients post_event (4 treatment + 2 placebo)
# ==============================================================================

library(dplyr)
library(broom)
library(marginaleffects)

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
  ),
  # Placebo events
  placebo_2012 = list(
    name = "Placebo 2012",
    year = 2012,
    pre_years = c(2008, 2011),
    post_years = c(2015, 2019),
    generations = c("preboomer", "boomer", "x", "y", "z"),
    type = "placebo"
  ),
  placebo_2020 = list(
    name = "Placebo 2020",
    year = 2020,
    pre_years = c(2015, 2019),
    post_years = c(2021, 2022, 2023),
    generations = c("preboomer", "boomer", "x", "y", "z"),
    type = "placebo"
  )
)

# 3. FONCTION DiD --------------------------------------------------------------

run_did <- function(event, data) {

  # Filtrer données
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    mutate(
      post_event = as.numeric(year > event$year)
    )

  # Info
  n_pre <- sum(data_event$post_event == 0)
  n_post <- sum(data_event$post_event == 1)
  message(paste("  N =", nrow(data_event), "(pre:", n_pre, "| post:", n_post, ")"))

  # Estimer modèle DiD (avec contrôle pour langue)
  model <- lm(iss_souv ~ post_event * generation + ses_lang.1, data = data_event)

  # Extraire effets par génération avec marginaleffects (inference correcte)
  coef_by_gen <- avg_slopes(
    model,
    variables = "post_event",
    by = "generation"
  ) %>%
    as.data.frame() %>%
    select(generation, estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(
      event_name = event$name,
      event_year = event$year,
      event_type = event$type,
      n_total = nrow(data_event),
      n_pre = n_pre,
      n_post = n_post
    )

  return(list(
    model = model,
    coef = coef_by_gen,
    data = data_event
  ))
}

# 4. ESTIMER TOUS LES ÉVÉNEMENTS -----------------------------------------------

results <- lapply(events, function(e) {
  message(paste("\nRunning DiD for:", e$name))
  run_did(e, Data)
})

# 5. COMPILER RÉSULTATS --------------------------------------------------------

did_results <- bind_rows(lapply(results, function(r) r$coef))

message("\n=== DIFFERENCE-IN-DIFFERENCES RESULTS BY GENERATION ===")
message("Post-event effects:\n")
print(did_results %>% select(event_name, event_type, generation, estimate, std.error, p.value))

# 6. SAUVEGARDER ---------------------------------------------------------------

dir.create("SharedFolder_spsa_article_nationalisme/tables/event_study",
           showWarnings = FALSE, recursive = TRUE)
write.csv(did_results,
          "SharedFolder_spsa_article_nationalisme/tables/event_study/did_results.csv",
          row.names = FALSE)

message("\n=== SUMMARY ===")
message("DiD models estimated for 6 events (4 treatment + 2 placebo)")
message("Results saved to: tables/event_study/did_results.csv")
