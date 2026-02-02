## Structure 1976 census data into marginals
## Reads .sav files extracted by fetch_1976.R and creates marginals_1976.rds
## Variables: age × gender, language × gender

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1976"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read AGE file
message("Reading AGE file...")
age_data <- read_sav(file.path(raw_dir, "AGE_cnd_eadema10.sav"))

# Filter for Quebec total (PROV == 24, FED == 0)
message("Filtering for Quebec (PROV == 24)...")
age_qc <- age_data %>% filter(PROV == 24, FED == 0)
message("Quebec total population: ", age_qc$V1[1])

# Age structure for 1976:
# Male: V17(15-19), V20(20-24), V23(25-29), V26(30-34), V29(35-39),
#       V32(40-44), V35(45-49), V38(50-54), V41(55-59), V44(60-64),
#       V47(65-69), V50(70-74), V53(75-79), V56(80-84), V59(85-89),
#       V62(90-94), V65(95-99), V68(100+)
# Female: V18, V21, V24, V27, V30, V33, V36, V39, V42, V45, V48, V51, V54, V57, V60, V63, V66, V69

message("Creating age marginals...")

# Male age groups
age_male <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "male",
  n = c(
    age_qc$V17[1], age_qc$V20[1], age_qc$V23[1], age_qc$V26[1],
    age_qc$V29[1], age_qc$V32[1], age_qc$V35[1], age_qc$V38[1],
    age_qc$V41[1], age_qc$V44[1], age_qc$V47[1], age_qc$V50[1],
    # 75+ = 75-79 + 80-84 + 85-89 + 90-94 + 95-99 + 100+
    age_qc$V53[1] + age_qc$V56[1] + age_qc$V59[1] + age_qc$V62[1] + age_qc$V65[1] + age_qc$V68[1]
  )
)

# Female age groups
age_female <- tibble(
  variable = "age_group",
  category = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
               "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"),
  gender = "female",
  n = c(
    age_qc$V18[1], age_qc$V21[1], age_qc$V24[1], age_qc$V27[1],
    age_qc$V30[1], age_qc$V33[1], age_qc$V36[1], age_qc$V39[1],
    age_qc$V42[1], age_qc$V45[1], age_qc$V48[1], age_qc$V51[1],
    # 75+ = 75-79 + 80-84 + 85-89 + 90-94 + 95-99 + 100+
    age_qc$V54[1] + age_qc$V57[1] + age_qc$V60[1] + age_qc$V63[1] + age_qc$V66[1] + age_qc$V69[1]
  )
)

message("  Male 15+ total: ", sum(age_male$n))
message("  Female 15+ total: ", sum(age_female$n))

# Read LANG file
message("Reading LANG file...")
lang_data <- read_sav(file.path(raw_dir, "LANG_eadema20.sav"))
lang_qc <- lang_data %>% filter(PROV == 24, FED == 0)

# Language structure for 1976:
# English: V5 (male), V6 (female)
# French: V8 (male), V9 (female)
# Other = all other languages combined

message("Creating language marginals...")

# Calculate "other" by summing all non-E/F languages
# V11/12 (Chinese+Japanese), V14/15 (German), V17/18 (Greek), V20/21 (Italian),
# V23/24 (Native Indian), V26/27 (Netherlandic), V29/30 (Polish), V32/33 (Ukrainian),
# V35/36 (Not stated), V38/39 (Other)
other_male <- lang_qc$V11[1] + lang_qc$V14[1] + lang_qc$V17[1] + lang_qc$V20[1] +
              lang_qc$V23[1] + lang_qc$V26[1] + lang_qc$V29[1] + lang_qc$V32[1] +
              lang_qc$V35[1] + lang_qc$V38[1]

other_female <- lang_qc$V12[1] + lang_qc$V15[1] + lang_qc$V18[1] + lang_qc$V21[1] +
                lang_qc$V24[1] + lang_qc$V27[1] + lang_qc$V30[1] + lang_qc$V33[1] +
                lang_qc$V36[1] + lang_qc$V39[1]

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(lang_qc$V5[1], lang_qc$V8[1], other_male)
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(lang_qc$V6[1], lang_qc$V9[1], other_female)
)

# Combine all marginals
marginals <- bind_rows(age_male, age_female, lang_male, lang_female) %>%
  mutate(census_year = 1976L) %>%
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

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1976.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1976.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
