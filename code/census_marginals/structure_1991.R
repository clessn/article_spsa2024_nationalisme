## Structure 1991 census data into marginals
## Reads .sav files extracted by fetch_1991.R and creates marginals_1991.rds
## Variables: age × gender, language (no gender breakdown for language)
##
## Uses e9101ea.sav (age by single year with sex) instead of e9102ea.sav (truncated)

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1991"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read AGE file (e9101ea - age by single year with sex breakdown)
message("Reading AGE file (e9101ea)...")
age_data <- read_sav(file.path(raw_dir, "AGE_e9101ea.sav"))

# Filter for Quebec total (RP == 24, FED87 == 0)
message("Filtering for Quebec (RP == 24)...")
age_qc <- age_data %>% filter(RP == 24, FED87 == 0)

# Age structure for 1991 (e9101ea):
# Each 5-year group has Total, Male, Female columns
# Male columns: V60(15-19), V78(20-24), V96(25-29), V114(30-34), V132(35-39),
#               V150(40-44), V168(45-49), V186(50-54), V204(55-59), V222(60-64),
#               V240(65-69), V258(70-74), V276(75-79), V294(80-84), V312(85-89), V330(90+)
# Female columns: V61, V79, V97, V115, V133, V151, V169, V187, V205, V223, V241, V259, V277, V295, V313, V331

message("Creating age marginals...")

# Male age groups
age_male <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "male",
  n = c(
    age_qc$V60[1], age_qc$V78[1], age_qc$V96[1], age_qc$V114[1],
    age_qc$V132[1], age_qc$V150[1], age_qc$V168[1], age_qc$V186[1],
    age_qc$V204[1], age_qc$V222[1], age_qc$V240[1], age_qc$V258[1],
    # 75+ = 75-79 + 80-84 + 85-89 + 90+
    age_qc$V276[1] + age_qc$V294[1] + age_qc$V312[1] + age_qc$V330[1]
  )
)

# Female age groups (now complete!)
age_female <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "female",
  n = c(
    age_qc$V61[1], age_qc$V79[1], age_qc$V97[1], age_qc$V115[1],
    age_qc$V133[1], age_qc$V151[1], age_qc$V169[1], age_qc$V187[1],
    age_qc$V205[1], age_qc$V223[1], age_qc$V241[1], age_qc$V259[1],
    # 75+ = 75-79 + 80-84 + 85-89 + 90+
    age_qc$V277[1] + age_qc$V295[1] + age_qc$V313[1] + age_qc$V331[1]
  )
)

message("  Male 15+ total: ", sum(age_male$n))
message("  Female 15+ total: ", sum(age_female$n))

# Read LANG file
message("Reading LANG file...")
lang_data <- read_sav(file.path(raw_dir, "LANG_l9101ea.sav"))
lang_qc <- lang_data %>% filter(RP == 24, FED == 0)

# Language structure for 1991 (no sex breakdown available):
# v28: English total, v41: French total, v54: Non-official languages total
# Note: These are total population, not 15+ only

message("Creating language marginals...")
message("  Note: Language data has no sex breakdown - using overall sex ratio")

# Get totals
english_total <- lang_qc$v28[1]
french_total <- lang_qc$v41[1]
other_total <- lang_qc$v54[1]

# Use sex ratio from age data (15+) to split language
male_15_plus <- sum(age_male$n)
female_15_plus <- sum(age_female$n)
male_ratio <- male_15_plus / (male_15_plus + female_15_plus)
female_ratio <- 1 - male_ratio

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = round(c(english_total, french_total, other_total) * male_ratio)
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = round(c(english_total, french_total, other_total) * female_ratio)
)

# Combine all marginals
marginals <- bind_rows(age_male, age_female, lang_male, lang_female) %>%
  mutate(census_year = 1991L) %>%
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

message("\nNote: Language split by sex estimated from overall sex ratio (15+)")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1991.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1991.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
