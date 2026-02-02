## Structure 1981 census data into marginals
## Reads .sav files extracted by fetch_1981.R and creates marginals_1981.rds
## Variables: age × gender, language × gender (no education in this dataset)

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1981"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read AGE file
message("Reading AGE .sav file...")
data_age <- read_sav(file.path(raw_dir, "AGE_cnd_ead81a10.sav"))

# Read LANG file
message("Reading LANG .sav file...")
data_lang <- read_sav(file.path(raw_dir, "LANG_cnd_eac81a10.sav"))

# Filter for Quebec (RP == 24)
message("Filtering for Quebec (RP == 24)...")
data_age_qc <- data_age %>% filter(as.numeric(RP) == 24)
data_lang_qc <- data_lang %>% filter(as.numeric(RP) == 24)
message("Quebec rows - AGE: ", nrow(data_age_qc), ", LANG: ", nrow(data_lang_qc))

# Variable structure from codebooks:
# AGE (15+ by 5-year groups, need to aggregate to 10-year):
#   15-19: F75 (Male), F76 (Female)
#   20-24: F93 (Male), F94 (Female)
#   25-29: F111 (Male), F112 (Female)
#   30-34: F129 (Male), F130 (Female)
#   35-39: F147 (Male), F148 (Female)
#   40-44: F165 (Male), F166 (Female)
#   45-49: F183 (Male), F184 (Female)
#   50-54: F201 (Male), F202 (Female)
#   55-59: F219 (Male), F220 (Female)
#   60-64: F237 (Male), F238 (Female)
#   65-69: F240 (Male), F241 (Female)
#   70-74: F243 (Male), F244 (Female)
#   75+: F246 (Male), F247 (Female)
#
# LANG (mother tongue, total population):
#   English: F3 (Male), F4 (Female)
#   French: F6 (Male), F7 (Female)
#   Other: German (F9/F10) + Indian/Inuktituk (F12/F13) + Italian (F15/F16) +
#          Netherlandic (F18/F19) + Polish (F21/F22) + Ukrainian (F24/F25) + Others (F27/F28)

# Age marginals (15+ only, aggregated to 10-year groups)
message("Creating age marginals...")
age_male <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "male",
  n = c(
    sum(data_age_qc$F75, data_age_qc$F93, na.rm = TRUE),
    sum(data_age_qc$F111, data_age_qc$F129, na.rm = TRUE),
    sum(data_age_qc$F147, data_age_qc$F165, na.rm = TRUE),
    sum(data_age_qc$F183, data_age_qc$F201, na.rm = TRUE),
    sum(data_age_qc$F219, data_age_qc$F237, na.rm = TRUE),
    sum(data_age_qc$F240, data_age_qc$F243, data_age_qc$F246, na.rm = TRUE)
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    sum(data_age_qc$F76, data_age_qc$F94, na.rm = TRUE),
    sum(data_age_qc$F112, data_age_qc$F130, na.rm = TRUE),
    sum(data_age_qc$F148, data_age_qc$F166, na.rm = TRUE),
    sum(data_age_qc$F184, data_age_qc$F202, na.rm = TRUE),
    sum(data_age_qc$F220, data_age_qc$F238, na.rm = TRUE),
    sum(data_age_qc$F241, data_age_qc$F244, data_age_qc$F247, na.rm = TRUE)
  )
)

# Language marginals (mother tongue)
message("Creating language marginals...")
lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    sum(data_lang_qc$F3, na.rm = TRUE),
    sum(data_lang_qc$F6, na.rm = TRUE),
    sum(data_lang_qc$F9, data_lang_qc$F12, data_lang_qc$F15, data_lang_qc$F18,
        data_lang_qc$F21, data_lang_qc$F24, data_lang_qc$F27, na.rm = TRUE)
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    sum(data_lang_qc$F4, na.rm = TRUE),
    sum(data_lang_qc$F7, na.rm = TRUE),
    sum(data_lang_qc$F10, data_lang_qc$F13, data_lang_qc$F16, data_lang_qc$F19,
        data_lang_qc$F22, data_lang_qc$F25, data_lang_qc$F28, na.rm = TRUE)
  )
)

# Combine all marginals
marginals <- bind_rows(age_male, age_female, lang_male, lang_female) %>%
  mutate(census_year = 1981L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

# Summary
message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

message("\nNote: education not available in this 1981 dataset")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1981.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1981.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
