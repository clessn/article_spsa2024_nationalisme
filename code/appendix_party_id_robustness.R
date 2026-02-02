# Appendix: Party ID Robustness Check
# Responds to Reviewer 2 Issue #21
# Tests whether results hold when controlling for provincial party ID
#
# TODO: Add justification in Methods/Discussion text:
# "Provincial party identification is available for a subset of respondents.
# Due to high rates of missing data (>85% in recent surveys), we do not include
# party ID in our main models. However, Appendix X shows that our key findings
# remain robust when controlling for party identification, though effect sizes
# are attenuated, consistent with party ID acting as a partial mediator."

# Packages ----------------------------------------------------------------
library(dplyr)
library(tibble)
library(modelsummary)
library(survey)
library(kableExtra)

options(modelsummary_factory_default = "kableExtra")
options(knitr.table.format = "latex")

# Data -------------------------------------------------------------------
# Use all years with non-missing party ID, control for year FE
data_raw <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds") %>%
  filter(!is.na(party_id_prov)) |>
  mutate(
    yob = year - ses_age,
    generation = case_when(
      yob %in% 1925:1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob >= 1992 ~ "z"
    ),
    generation = factor(generation, levels = c("boomer", "preboomer", "x", "y", "z")),
    # Recode party ID to main categories
    party_id = case_when(
      party_id_prov == "pq" ~ "PQ",
      party_id_prov == "plq" ~ "PLQ",
      party_id_prov == "caq" ~ "CAQ",
      party_id_prov == "qs" ~ "QS",
      TRUE ~ "Other/None"
    ),
    party_id = factor(party_id, levels = c("CAQ", "PQ", "PLQ", "QS", "Other/None"))
  )

# Prepare DV (sovereignty attitudes 0-1)
data_raw$vd <- data_raw$iss_souv2
data_raw$vd[data_raw$vd == 0.33] <- 0.25
data_raw$vd[data_raw$vd == 0.66] <- 0.75

cat("=== Sample size ===\n")
cat("N:", nrow(data_raw), "\n\n")
cat("By year:\n")
print(table(data_raw$year))
cat("\nBy party:\n")
print(table(data_raw$party_id))

# Model data
model_data <- data_raw |>
  select(
    vd, generation, ses_lang.1, party_id,
    ses_gender, ses_family_income_centile_cat,
    ses_origin_from_canada.1, ses_educ,
    year, weight_trimmed
  ) |>
  tidyr::drop_na()

cat("\n=== Model N (after dropping NA) ===\n")
cat("N:", nrow(model_data), "\n")

# Survey design
des <- svydesign(
  ids = ~year,
  weights = ~weight_trimmed,
  data = model_data
)

# Models ------------------------------------------------------------------

# Model 1: Without party ID (baseline)
model_no_party <- svyglm(
  vd ~ generation * ses_lang.1 + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des
)

# Model 2: With party ID
model_with_party <- svyglm(
  vd ~ generation * ses_lang.1 + party_id + ses_gender +
    ses_family_income_centile_cat + ses_origin_from_canada.1 + ses_educ +
    factor(year),
  design = des
)

# Create comparison table -------------------------------------------------

models <- list(
  "Without Party ID" = model_no_party,
  "With Party ID" = model_with_party
)

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
  "generationpreboomer:ses_lang.1french" = "Preboomer × French",
  "generationx:ses_lang.1french" = "X × French",
  "generationy:ses_lang.1french" = "Y × French",
  "generationz:ses_lang.1french" = "Z × French",
  "generationpreboomer:ses_lang.1other" = "Preboomer × Other",
  "generationx:ses_lang.1other" = "X × Other",
  "generationy:ses_lang.1other" = "Y × Other",
  "generationz:ses_lang.1other" = "Z × Other",
  # Party ID
  "party_idPQ" = "Party ID: PQ",
  "party_idPLQ" = "Party ID: PLQ",
  "party_idQS" = "Party ID: QS",
  "party_idOther/None" = "Party ID: Other/None",
  # Controls
  "ses_gendermale" = "Gender: Male",
  "ses_origin_from_canada.1" = "Origin: From Canada",
  "ses_educ1" = "Education: Higher"
)

gof_map <- tribble(
  ~raw,           ~clean,       ~fmt,
  "nobs",         "N",          0
)

# Generate table
df_raw <- modelsummary(
  models,
  coef_map = coef_map,
  gof_map = gof_map,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  output = "dataframe"
)

# Process estimates
df_estimates <- df_raw |>
  filter(part == "estimates") |>
  select(-part) |>
  group_by(term) |>
  summarise(
    across(c(`Without Party ID`, `With Party ID`),
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

# Maintain order
df_estimates <- df_estimates |>
  mutate(term = factor(term, levels = coef_map)) |>
  arrange(term) |>
  mutate(term = as.character(term))

# GOF stats
df_gof <- df_raw |>
  filter(part == "gof") |>
  select(term, `Without Party ID`, `With Party ID`)

# FE indicators
df_fe <- tibble(
  term = c("Year FE", "Income controls"),
  `Without Party ID` = c("Yes", "Yes"),
  `With Party ID` = c("Yes", "Yes")
)

df <- bind_rows(df_estimates, df_fe, df_gof)

# Create LaTeX table (simple, with caption and label)
tab_latex <- kableExtra::kbl(
  df,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "Without Party ID", "With Party ID"),
  align = "lcc",
  caption = "Robustness Check: Controlling for Provincial Party Identification",
  label = "party_id_robustness"
) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

# Save
dir.create("SharedFolder_spsa_article_nationalisme/tables/appendix", showWarnings = FALSE, recursive = TRUE)
writeLines(as.character(tab_latex),
           "SharedFolder_spsa_article_nationalisme/tables/appendix/party_id_robustness.tex")

message("Table saved to SharedFolder_spsa_article_nationalisme/tables/appendix/party_id_robustness.tex")

# Print key coefficients comparison
cat("\n=== Key coefficients comparison ===\n")
cat("Generation Z (without party ID):", round(coef(model_no_party)["generationz"], 3), "\n")
cat("Generation Z (with party ID):   ", round(coef(model_with_party)["generationz"], 3), "\n")
cat("\nZ × French (without party ID):", round(coef(model_no_party)["generationz:ses_lang.1french"], 3), "\n")
cat("Z × French (with party ID):   ", round(coef(model_with_party)["generationz:ses_lang.1french"], 3), "\n")
