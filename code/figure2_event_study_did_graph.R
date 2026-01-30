# ==============================================================================
# EVENT-STUDY: DiD GRAPH BY GENERATION
# ==============================================================================
# Objectif: Visualiser les effets DiD par génération pour tous les événements
# ==============================================================================

library(dplyr)
library(ggplot2)
library(clessnize)

# 1. CHARGER RÉSULTATS ---------------------------------------------------------

did_results <- read.csv("SharedFolder_spsa_article_nationalisme/tables/event_study/did_results.csv")

# 2. PRÉPARER DONNÉES POUR GRAPH -----------------------------------------------

plot_data <- did_results %>%
  mutate(
    # Calculer CI 95%
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    # Catégorie de significativité pour alpha
    sig_cat = case_when(
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10",
      TRUE ~ "p >= 0.10"
    ),
    sig_cat = factor(sig_cat, levels = c("p < 0.05", "p < 0.10", "p >= 0.10")),
    # Ordonner événements chronologiquement
    event_name = factor(event_name, levels = rev(c(
      "Referendum 1980", "Meech Lake 1990", "Referendum 1995",
      "Sponsorship 2005", "Placebo 2012", "Placebo 2020"
    ))),
    # Ordonner générations
    generation = factor(
      generation,
      levels = c("preboomer", "boomer", "x", "y", "z"),
      labels = c("Pre-boomer", "Boomer", "X", "Y", "Z"),
    ),
    # Marquer placebos
    is_placebo = event_type == "placebo"
  )

# 3. CRÉER GRAPH ---------------------------------------------------------------

ggplot(plot_data, aes(x = estimate, y = event_name, alpha = sig_cat)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
  geom_segment(
    aes(x = conf.low, xend = conf.high),
    linewidth = 1,
    color = "black"
  ) +
  geom_point(
    aes(shape = is_placebo),
    size = 2,
    color = "white", fill = "white",
    alpha = 1
  ) +
  geom_point(
    aes(shape = is_placebo),
    size = 2,
    color = "black", fill = "black"
  ) +
  facet_wrap(~generation, nrow = 1) +
  scale_alpha_manual(
    values = c("p < 0.05" = 1, "p < 0.10" = 0.6, "p >= 0.10" = 0.3),
    name = "Significance"
  ) +
  scale_shape_manual(
    values = c("FALSE" = 16, "TRUE" = 15),  # Rond vs triangle
    name = "Event type",
    labels = c("Treatment", "Placebo")
  ) +
  labs(
    y = NULL,
    x = "\nPost-event effect on independence support\n(on scale from 0 to 1)"
  ) +
  coord_cartesian(
    xlim = c(-0.15, 0.25)
  ) +
  theme_clean_light() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 6.5),
    strip.text = element_text(face = "plain"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key = element_rect(color = NA),
    panel.background = element_rect(fill = NA, color = "grey80")
  )

# 4. SAUVEGARDER ---------------------------------------------------------------

ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure2_did_by_generation.png",
  width = 8,
  height = 6,
  dpi = 300
)

message("Graph saved: did_by_generation.png")
