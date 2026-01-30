# Figure 2 Appendix: Regression tables with and without controls
# Responds to Reviewer 1 Issue #11

# Packages ----------------------------------------------------------------
library(dplyr)
library(tibble)
library(modelsummary)

# Use classic booktabs format instead of tabularray
options(modelsummary_factory_default = "kableExtra")
options(knitr.table.format = "latex")

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
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

# Prepare DV (using 5-point scale version)
data$vd <- data$iss_souv2
data$vd[data$vd == 0.33] <- 0.25
data$vd[data$vd == 0.66] <- 0.75

# Model data - separate for with/without controls to maximize N
model_data_no_controls <- data |>
  select(vd, generation, ses_lang.1) |>
  tidyr::drop_na()

model_data_with_controls <- data |>
  select(
    vd, generation, ses_lang.1,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ
  ) |>
  tidyr::drop_na()

# Models ------------------------------------------------------------------

# Model 1: Without controls
model_no_controls <- lm(
  vd ~ generation * ses_lang.1,
  data = model_data_no_controls
)

# Model 2: With controls
model_with_controls <- lm(
  vd ~ generation * ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ,
  data = model_data_with_controls
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
  "r.squared",    "R²",         3,
  "adj.r.squared", "Adj. R²",   3
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
term_order <- names(coef_map)
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
           "SharedFolder_spsa_article_nationalisme/tables/appendix/figure2_regression_table.tex")

message("Table saved to SharedFolder_spsa_article_nationalisme/tables/appendix/figure2_regression_table.tex")
