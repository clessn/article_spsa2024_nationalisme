# Figure 3 Appendix: Regression tables with and without controls
# Responds to Reviewer 1 Issue #11
# Logistic regression predicting attitude strength (strong vs weak)

# Packages ----------------------------------------------------------------
library(dplyr)
library(tibble)
library(modelsummary)
library(kableExtra)
library(survey)  # for svyglm with proper weighting and clustering

# Use classic booktabs format instead of tabularray
options(modelsummary_factory_default = "kableExtra")
options(knitr.table.format = "latex")

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds") %>%
  filter(year >= 2021) |>
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
# Note: year included in both for clustered SEs
model_data_no_controls <- data |>
  select(attitude_strength, generation, ses_lang.1, year, weight_trimmed) |>
  tidyr::drop_na()

model_data_with_controls <- data |>
  select(
    attitude_strength, generation, ses_lang.1,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ,
    year,
    weight_trimmed
  ) |>
  tidyr::drop_na()

# Models ------------------------------------------------------------------

# Survey design with clustering by year
des_no_controls <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_no_controls
)

des_with_controls <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_with_controls
)

# Model 1: Without controls (logistic)
model_no_controls <- svyglm(
  attitude_strength ~ generation * ses_lang.1,
  design = des_no_controls,
  family = quasibinomial()
)

# Model 2: With controls + year FE (logistic)
model_with_controls <- svyglm(
  attitude_strength ~ generation * ses_lang.1 +
    ses_gender + ses_family_income_centile_cat +
    ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des_with_controls,
  family = quasibinomial()
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
  # Language
  "ses_lang.1french" = "Language: French",
  "ses_lang.1other" = "Language: Other",
  # Interactions
  "generationpreboomer:ses_lang.1french" = "Preboomer x French",
  "generationx:ses_lang.1french" = "X x French",
  "generationy:ses_lang.1french" = "Y x French",
  "generationz:ses_lang.1french" = "Z x French",
  "generationpreboomer:ses_lang.1other" = "Preboomer x Other",
  "generationx:ses_lang.1other" = "X x Other",
  "generationy:ses_lang.1other" = "Y x Other",
  "generationz:ses_lang.1other" = "Z x Other",
  # Controls
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

# Generate table data (svyglm already has proper SEs)
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

# Add FE indicators
df_fe <- tibble(
  term = c("Year FE"),
  `Without controls` = c("No"),
  `With controls` = c("Yes")
)

# Combine
df <- bind_rows(df_estimates, df_fe, df_gof)

# Create LaTeX table
tab_latex <- kableExtra::kbl(
  df,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "Without controls", "With controls"),
  align = "lcc"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position")) |>
  kableExtra::footnote(
    general = "Survey-weighted logistic regression with standard errors clustered by survey year.",
    general_title = "Note: ",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE
  )

writeLines(as.character(tab_latex),
           "SharedFolder_spsa_article_nationalisme/tables/appendix/figure4_regression_table.tex")

message("Table saved to SharedFolder_spsa_article_nationalisme/tables/appendix/figure4_regression_table.tex")
