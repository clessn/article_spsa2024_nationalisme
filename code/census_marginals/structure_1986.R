## Structure 1986 census data into marginals
## Reads .sav files extracted by fetch_1986.R and creates marginals_1986.rds
## Variables: age × gender, language × gender (no education available)

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1986"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read AGE file
message("Reading AGE file...")
age_data <- read_sav(file.path(raw_dir, "AGE_eadm86a01.sav"))

# Filter for Quebec total (rp == 24, fed == 0)
message("Filtering for Quebec (rp == 24)...")
age_qc <- age_data %>% filter(rp == 24, fed == 0)
message("Quebec total population: ", age_qc$total[1])

# Age groups structure for 1986 (5-year groups):
# r1: 0-4, r2: 5-9, r3: 10-14, r4: 15-19, r5: 20-24, r6: 25-29,
# r7: 30-34, r8: 35-39, r9: 40-44, r10: 45-49, r11: 50-54,
# r12: 55-59, r13: 60-64, r14: 65-69, r15: 70-74, r16: 75+
#
# For 18+ population, we use r4 onwards (15-19 includes some <18, but best approximation)
# c1 = male, c2 = female

message("Creating age marginals (15+)...")
age_male <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "male",
  n = c(
    age_qc$r4c1[1], age_qc$r5c1[1], age_qc$r6c1[1], age_qc$r7c1[1],
    age_qc$r8c1[1], age_qc$r9c1[1], age_qc$r10c1[1], age_qc$r11c1[1],
    age_qc$r12c1[1], age_qc$r13c1[1], age_qc$r14c1[1], age_qc$r15c1[1],
    age_qc$r16c1[1]
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "female",
  n = c(
    age_qc$r4c2[1], age_qc$r5c2[1], age_qc$r6c2[1], age_qc$r7c2[1],
    age_qc$r8c2[1], age_qc$r9c2[1], age_qc$r10c2[1], age_qc$r11c2[1],
    age_qc$r12c2[1], age_qc$r13c2[1], age_qc$r14c2[1], age_qc$r15c2[1],
    age_qc$r16c2[1]
  )
)

# Read LANG file
message("Reading LANG file...")
lang_data <- read_sav(file.path(raw_dir, "LANG_eamt86a01.sav"))
lang_qc <- lang_data %>% filter(rp == 24, fed == 0)

# Language structure for 1986 (mother tongue):
# r1: English, r2: French, r3: English+French subtotal
# r4-r22: Other specific languages
# r23-r25: Multiple responses / totals
#
# For our purposes: english = r1, french = r2, other = sum(r4:r22)

message("Creating language marginals...")

# Calculate "other" as sum of r4 to r22
other_male <- sum(
  lang_qc$r4c1[1], lang_qc$r5c1[1], lang_qc$r6c1[1], lang_qc$r7c1[1],
  lang_qc$r8c1[1], lang_qc$r9c1[1], lang_qc$r10c1[1], lang_qc$r11c1[1],
  lang_qc$r12c1[1], lang_qc$r13c1[1], lang_qc$r14c1[1], lang_qc$r15c1[1],
  lang_qc$r16c1[1], lang_qc$r17c1[1], lang_qc$r18c1[1], lang_qc$r19c1[1],
  lang_qc$r20c1[1], lang_qc$r21c1[1], lang_qc$r22c1[1],
  na.rm = TRUE
)

other_female <- sum(
  lang_qc$r4c2[1], lang_qc$r5c2[1], lang_qc$r6c2[1], lang_qc$r7c2[1],
  lang_qc$r8c2[1], lang_qc$r9c2[1], lang_qc$r10c2[1], lang_qc$r11c2[1],
  lang_qc$r12c2[1], lang_qc$r13c2[1], lang_qc$r14c2[1], lang_qc$r15c2[1],
  lang_qc$r16c2[1], lang_qc$r17c2[1], lang_qc$r18c2[1], lang_qc$r19c2[1],
  lang_qc$r20c2[1], lang_qc$r21c2[1], lang_qc$r22c2[1],
  na.rm = TRUE
)

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(lang_qc$r1c1[1], lang_qc$r2c1[1], other_male)
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(lang_qc$r1c2[1], lang_qc$r2c2[1], other_female)
)

# Combine all marginals
marginals <- bind_rows(age_male, age_female, lang_male, lang_female) %>%
  mutate(census_year = 1986L) %>%
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

message("\nNote: education not available for 1986")
message("Note: language data is for total population, not 18+ only")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1986.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1986.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
