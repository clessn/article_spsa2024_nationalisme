# HAPC-CCREM Analysis: Age-Period-Cohort Model
# Issue #15: Disentangle cohort from period effects
# Reference: Yang & Land (2008)

# Packages ----------------------------------------------------------------
library(dplyr)
library(lme4)
library(ggplot2)

# Data --------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(
    # Year of birth
    yob = year - ses_age,
    # Generation (cohort)
    generation = case_when(
      yob %in% 1925:1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob %in% 1992:2003 ~ "z"
    ),
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z")),
    # Standardize age (z-score) for convergence
    age_scaled = scale(ses_age)[,1],
    # Year as factor for random effect
    year_factor = factor(year)
  ) %>%
  filter(!is.na(generation), !is.na(iss_souv), !is.na(ses_age))

# Check data structure
table(data$generation)
table(data$year)

# HAPC Model --------------------------------------------------------------
# DV: iss_souv (binary 0/1)
# Fixed effects: age (quadratic)
# Random effects: generation (cohort), year (period)

model_hapc <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) +
    (1 | generation) + (1 | year_factor),
  data = data,
  family = binomial
)

summary(model_hapc)

# Variance components -----------------------------------------------------
var_components <- as.data.frame(VarCorr(model_hapc))
print(var_components)

# Calculate ICC (proportion of variance)
# For logistic regression, level-1 variance is pi^2/3
var_cohort <- var_components$vcov[var_components$grp == "generation"]
var_period <- var_components$vcov[var_components$grp == "year_factor"]
var_residual <- pi^2 / 3  # Standard logistic distribution variance
var_total <- var_cohort + var_period + var_residual

cat("\n=== Variance Decomposition ===\n")
cat("Cohort (generation) variance:", round(var_cohort, 4),
    "(", round(var_cohort/var_total * 100, 1), "%)\n")
cat("Period (year) variance:", round(var_period, 4),
    "(", round(var_period/var_total * 100, 1), "%)\n")
cat("Residual variance (pi^2/3):", round(var_residual, 4),
    "(", round(var_residual/var_total * 100, 1), "%)\n")

# Extract random effects --------------------------------------------------
re_cohort <- ranef(model_hapc)$generation
re_period <- ranef(model_hapc)$year_factor

cat("\n=== Cohort (Generation) Random Effects ===\n")
print(re_cohort)

cat("\n=== Period (Year) Random Effects ===\n")
print(re_period)

# Model comparison: test significance of random effects -------------------

# Model without cohort effect
model_no_cohort <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) +
    (1 | year_factor),
  data = data,
  family = binomial
)

# Model without period effect
model_no_period <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) +
    (1 | generation),
  data = data,
  family = binomial
)

# Likelihood ratio tests
cat("\n=== Likelihood Ratio Tests ===\n")
cat("\nTest for cohort effect (comparing model with vs without generation):\n")
print(anova(model_no_cohort, model_hapc))

cat("\nTest for period effect (comparing model with vs without year):\n")
print(anova(model_no_period, model_hapc))

# Visualizations ----------------------------------------------------------

# 1. Cohort effects plot
cohort_effects <- data.frame(
  generation = rownames(re_cohort),
  effect = re_cohort$`(Intercept)`
) %>%
  mutate(generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z")))

p_cohort <- ggplot(cohort_effects, aes(x = generation, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_segment(aes(xend = generation, y = 0, yend = effect)) +
  labs(
    title = "Cohort (Generation) Random Effects",
    subtitle = "Deviation from overall mean, controlling for age and period",
    x = "Generation",
    y = "Random Effect (deviation)"
  ) +
  clessnize::theme_clean_light()

print(p_cohort)

ggsave("SharedFolder_spsa_article_nationalisme/graphs/hapc_cohort_effects.png",
       p_cohort, width = 8, height = 5)

# 2. Period effects plot
period_effects <- data.frame(
  year = as.numeric(as.character(rownames(re_period))),
  effect = re_period$`(Intercept)`
)

p_period <- ggplot(period_effects, aes(x = year, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Period (Year) Random Effects",
    subtitle = "Deviation from overall mean, controlling for age and cohort",
    x = "Survey Year",
    y = "Random Effect (deviation)"
  ) +
  clessnize::theme_clean_light()

print(p_period)

ggsave("SharedFolder_spsa_article_nationalisme/graphs/hapc_period_effects.png",
       p_period, width = 8, height = 5)

# 3. Age effect (fixed) - on probability scale
age_range <- seq(min(data$age_scaled, na.rm = TRUE),
                 max(data$age_scaled, na.rm = TRUE),
                 length.out = 100)
fixed_effects <- fixef(model_hapc)

# Calculate log-odds then convert to probability
log_odds <- fixed_effects["(Intercept)"] +
  fixed_effects["age_scaled"] * age_range +
  fixed_effects["I(age_scaled^2)"] * age_range^2

# Convert scaled age back to real age
age_mean <- mean(data$ses_age, na.rm = TRUE)
age_sd <- sd(data$ses_age, na.rm = TRUE)

age_effect <- data.frame(
  age_scaled = age_range,
  age_real = age_range * age_sd + age_mean,
  predicted = plogis(log_odds)  # Convert to probability
)

p_age <- ggplot(age_effect, aes(x = age_real, y = predicted)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Age Effect (Fixed)",
    subtitle = "Predicted probability of sovereignty support by age, at average cohort and period",
    x = "Age",
    y = "Probability of Independence Support"
  ) +
  clessnize::theme_clean_light()

print(p_age)

ggsave("SharedFolder_spsa_article_nationalisme/graphs/hapc_age_effect.png",
       p_age, width = 8, height = 5)

# Summary table -----------------------------------------------------------
cat("\n=== HAPC Model Summary ===\n")
cat("N observations:", nrow(model_hapc@frame), "\n")
cat("N cohorts (generations):", length(unique(data$generation)), "\n")
cat("N periods (years):", length(unique(data$year)), "\n")

message("\nGraphs saved to SharedFolder_spsa_article_nationalisme/graphs/")
