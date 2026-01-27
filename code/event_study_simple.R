# ==============================================================================
# EVENT-STUDY SIMPLE: RÉFÉRENDUM 1995
# ==============================================================================
# Question: Le référendum de 1995 a-t-il changé les attitudes par génération?
# Note: Gen Y et Z exclus (pas de données pré-1995)

library(dplyr)
library(ggplot2)
library(marginaleffects)

# 1. CHARGER DONNÉES -----------------------------------------------------------

Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(
    yob = year - ses_age,
    generation = case_when(
      yob %in% 1925:1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob %in% 1992:2003 ~ "z"
    ),
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z"))
  )

# 2. FILTRER POUR ANNÉES AUTOUR DE 1995 ---------------------------------------

# Pré: 1988, 1993
# Post: 1997, 2000, 2004
# Seulement preboomer, boomer, x (qui existent pré ET post)

data_1995 <- Data %>%
  filter(year %in% c(1988, 1993, 1997, 2000, 2004)) %>%
  filter(generation %in% c("preboomer", "boomer", "x")) %>%
  filter(!is.na(iss_souv2)) %>%
  mutate(
    event_time = year - 1995,
    event_time_f = factor(event_time)
  )

# 3. MODÈLE EVENT-STUDY --------------------------------------------------------

data_1995$event_time_f <- relevel(data_1995$event_time_f, ref = "-2")

# Effet par année × génération
model <- lm(iss_souv2 ~ event_time_f * generation, data = data_1995)

summary(model)

# 4. VISUALISATION -------------------------------------------------------------

# Comparaisons: changement depuis t=-2 pour chaque génération
comps <- avg_comparisons(
  model,
  variables = list(event_time_f = "reference"),
  by = "generation"
) %>%
  as.data.frame()

# Extraire event_time depuis le contraste
comps <- comps %>%
  mutate(event_time = as.numeric(gsub(" - .*", "", contrast)))

# Ajouter point de référence (t=-2 = 0)
comps <- bind_rows(
  comps,
  data.frame(
    generation = c("preboomer", "boomer", "x"),
    event_time = -2,
    estimate = 0,
    conf.low = 0,
    conf.high = 0
  )
)

ggplot(comps, aes(x = event_time, y = estimate, color = generation)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~generation) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5) +
  labs(
    title = "Event-Study: Référendum 1995",
    x = "Années relatives au référendum",
    y = "Changement vs t=-2",
    color = "Génération"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("SharedFolder_spsa_article_nationalisme/graphs/event_study/simple_1995.png",
       width = 8, height = 6, dpi = 300)


