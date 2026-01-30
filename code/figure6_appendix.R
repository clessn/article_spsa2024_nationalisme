# Figure 5 Appendix: Regression tables with and without controls
# Responds to Reviewer 1 Issue #11
# Logistic regression predicting attitude strength with generation x identity interaction

# Packages ----------------------------------------------------------------
library(dplyr)
library(tibble)
library(modelsummary)
library(kableExtra)

# Use classic booktabs format instead of tabularray
options(modelsummary_factory_default = "kableExtra")
options(knitr.table.format = "latex")

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  filter(
    year >= 2021 &
    source_id %in% c("january", "february", "march", "april", "may", "june")
  ) |>
  mutate(yob = year - ses_age,
         generation = case_when(
           yob %in% 1925:1946 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation, levels = c("boomer", "preboomer", "x", "y", "z")))

# Create binary DV: 1 = strong attitude (0 or 1), 0 = weak attitude (middle values)
data$attitude_strength <- NA
data$attitude_strength[data$iss_souv2 %in% c(0, 1)] <- 1
data$attitude_strength[data$iss_souv2 %in% c(0.25, 0.33, 0.5, 0.66, 0.75)] <- 0

# Model data - separate for with/without controls to maximize N
model_data_no_controls <- data |>
  select(attitude_strength, generation, iss_idcan) |>
  tidyr::drop_na()

model_data_with_controls <- data |>
  select(
    attitude_strength, generation, iss_idcan,
    ses_lang.1, ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ
  ) |>
  tidyr::drop_na()

# Models ------------------------------------------------------------------

# Model 1: Without controls (logistic)
model_no_controls <- glm(
  attitude_strength ~ generation * iss_idcan,
  data = model_data_no_controls,
  family = binomial()
)

# Model 2: With controls (logistic)
model_with_controls <- glm(
  attitude_strength ~ generation * iss_idcan + ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ,
  data = model_data_with_controls,
  family = binomial()
)

# Create comparison table -------------------------------------------------

models <- list(
  "Without controls" = model_no_controls,
  "With controls" = model_with_controls
)

# Coefficient mapping for cleaner labels
coef_map <- c(
  "(Intercept)" = "Intercept",
  # Generation
  "generationpreboomer" = "Generation: Preboomer",
  "generationx" = "Generation: X",
  "generationy" = "Generation: Y",
  "generationz" = "Generation: Z",
  # Identity
  "iss_idcan" = "Identity: Canadian first",
  # Interactions
  "generationpreboomer:iss_idcan" = "Preboomer x Canadian first",
  "generationx:iss_idcan" = "X x Canadian first",
  "generationy:iss_idcan" = "Y x Canadian first",
  "generationz:iss_idcan" = "Z x Canadian first",
  # Controls
  "ses_lang.1french" = "Language: French",
  "ses_lang.1other" = "Language: Other",
  "ses_gendermale" = "Gender: Male",
  "ses_family_income_centile_cat11_25" = "Income: 11-25th percentile",
  "ses_family_income_centile_cat26_50" = "Income: 26-50th percentile",
  "ses_family_income_centile_cat51_75" = "Income: 51-75th percentile",
  "ses_family_income_centile_cat76_90" = "Income: 76-90th percentile",
  "ses_family_income_centile_cat91_100" = "Income: 91-100th percentile",
  "ses_origin_from_canada.1" = "Origin: From Canada",
  "ses_educ1" = "Education: Higher"
)

# Goodness of fit statistics
gof_map <- tribble(
  ~raw,           ~clean,       ~fmt,
  "nobs",         "N",          0,
  "AIC",          "AIC",        1,
  "BIC",          "BIC",        1
)

# Save outputs ------------------------------------------------------------

# Generate table data
df_raw <- modelsummary(
  models,
  coef_map = coef_map,
  gof_map = gof_map,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  output = "dataframe"
)

# Process estimates (combine coef and se on same line)
df_estimates <- df_raw |>
  filter(part == "estimates") |>
  select(-part) |>
  group_by(term) |>
  summarise(
    across(c(`Without controls`, `With controls`),
           ~ {
             est <- .x[statistic == "estimate"]
             se <- gsub("[\\(\\)]", "", .x[statistic == "std.error"])
             if (is.na(est) || est == "") {
               ""
             } else {
               paste0(est, " (", se, ")")
             }
           })
  ) |>
  ungroup()

# Maintain order from coef_map
df_estimates <- df_estimates |>
  mutate(term = factor(term, levels = coef_map)) |>
  arrange(term) |>
  mutate(term = as.character(term))

# Process GOF stats (keep as is)
df_gof <- df_raw |>
  filter(part == "gof") |>
  select(term, `Without controls`, `With controls`)

# Combine
df <- bind_rows(df_estimates, df_gof)

# Create LaTeX table
tab_latex <- kableExtra::kbl(
  df,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "Without controls", "With controls"),
  align = "lcc"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position"))

writeLines(as.character(tab_latex),
           "SharedFolder_spsa_article_nationalisme/tables/appendix/figure5_regression_table.tex")

message("Table saved to SharedFolder_spsa_article_nationalisme/tables/appendix/figure5_regression_table.tex")
