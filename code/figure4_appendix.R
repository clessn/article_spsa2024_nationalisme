# Figure 4 Appendix: Regression tables with and without controls
# Responds to Reviewer 1 Issue #11 (with/without controls)
# Also responds to Issue #16 (Gen Z definition robustness check)
# Logistic regression predicting attitude strength (strong vs weak)

# Packages ----------------------------------------------------------------
library(dplyr)
library(tibble)
library(modelsummary)
library(survey)
library(kableExtra)

# Use classic booktabs format instead of tabularray
options(modelsummary_factory_default = "kableExtra")
options(knitr.table.format = "latex")

# Data -------------------------------------------------------------------
data_raw <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds") %>%
  filter(year >= 2021) |>
  mutate(yob = year - ses_age)

# Function to create generation variable with different Gen Z cutoffs
create_generation_var <- function(data, genz_start = 1992) {
  geny_end <- genz_start - 1
  data |>
    mutate(
      generation = case_when(
        yob %in% 1925:1946 ~ "preboomer",
        yob %in% 1947:1961 ~ "boomer",
        yob %in% 1962:1976 ~ "x",
        yob %in% 1977:geny_end ~ "y",
        yob >= genz_start ~ "z"
      ),
      generation = factor(generation, levels = c("boomer", "preboomer", "x", "y", "z"))
    )
}

# Create datasets with different Gen Z definitions
data_1992 <- create_generation_var(data_raw, genz_start = 1992)
data_1995 <- create_generation_var(data_raw, genz_start = 1995)
data_1997 <- create_generation_var(data_raw, genz_start = 1997)

# Create binary DV: 1 = strong attitude (0 or 1), 0 = weak attitude (middle values)
prepare_dv <- function(data) {
  data$attitude_strength <- NA
  data$attitude_strength[data$iss_souv2 %in% c(0, 1)] <- 1
  data$attitude_strength[data$iss_souv2 %in% c(0.25, 0.33, 0.5, 0.66, 0.75)] <- 0
  return(data)
}

data_1992 <- prepare_dv(data_1992)
data_1995 <- prepare_dv(data_1995)
data_1997 <- prepare_dv(data_1997)

# Model data - separate for with/without controls to maximize N
model_data_no_controls_1992 <- data_1992 |>
  select(attitude_strength, generation, ses_lang.1, year, weight_trimmed) |>
  tidyr::drop_na()

model_data_with_controls_1992 <- data_1992 |>
  select(
    attitude_strength, generation, ses_lang.1,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ,
    year, weight_trimmed
  ) |>
  tidyr::drop_na()

model_data_with_controls_1995 <- data_1995 |>
  select(
    attitude_strength, generation, ses_lang.1,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ,
    year, weight_trimmed
  ) |>
  tidyr::drop_na()

model_data_with_controls_1997 <- data_1997 |>
  select(
    attitude_strength, generation, ses_lang.1,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ,
    year, weight_trimmed
  ) |>
  tidyr::drop_na()

# Models ------------------------------------------------------------------

# Survey designs with clustering by year
des_no_controls_1992 <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_no_controls_1992
)

des_with_controls_1992 <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_with_controls_1992
)

des_with_controls_1995 <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_with_controls_1995
)

des_with_controls_1997 <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data_with_controls_1997
)

# Model 1: Without controls (Gen Z = 1992+)
model_no_controls <- svyglm(
  attitude_strength ~ generation * ses_lang.1,
  design = des_no_controls_1992,
  family = quasibinomial()
)

# Model 2: With controls (Gen Z = 1992+)
model_with_controls_1992 <- svyglm(
  attitude_strength ~ generation * ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des_with_controls_1992,
  family = quasibinomial()
)

# Model 3: With controls (Gen Z = 1995+) - Robustness check
model_with_controls_1995 <- svyglm(
  attitude_strength ~ generation * ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des_with_controls_1995,
  family = quasibinomial()
)

# Model 4: With controls (Gen Z = 1997+) - Robustness check (Pew definition)
model_with_controls_1997 <- svyglm(
  attitude_strength ~ generation * ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des_with_controls_1997,
  family = quasibinomial()
)

# Create comparison table -------------------------------------------------

models <- list(
  "1992+ (no controls)" = model_no_controls,
  "1992+" = model_with_controls_1992,
  "1995+" = model_with_controls_1995,
  "1997+" = model_with_controls_1997
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
  "nobs",         "N",          0
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
    across(c(`1992+ (no controls)`, `1992+`, `1995+`, `1997+`),
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
  select(term, `1992+ (no controls)`, `1992+`, `1995+`, `1997+`)

# Add FE indicators
df_fe <- tibble(
  term = c("Year FE"),
  `1992+ (no controls)` = c("No"),
  `1992+` = c("Yes"),
  `1995+` = c("Yes"),
  `1997+` = c("Yes")
)

# Combine
df <- bind_rows(df_estimates, df_fe, df_gof)

# Create LaTeX table
tab_latex <- kableExtra::kbl(
  df,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "1992+ (no controls)", "1992+", "1995+", "1997+"),
  align = "lcccc"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::add_header_above(c(" " = 1, " " = 1, "With controls" = 3)) |>
  kableExtra::footnote(
    general = "Logistic regression (quasibinomial). Survey-weighted with standard errors clustered by survey year.",
    general_title = "Note: ",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE
  )

writeLines(as.character(tab_latex),
           "SharedFolder_spsa_article_nationalisme/tables/appendix/figure4_regression_table.tex")

message("Table saved to SharedFolder_spsa_article_nationalisme/tables/appendix/figure4_regression_table.tex")
