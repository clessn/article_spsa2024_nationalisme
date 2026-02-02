## Structure 1971 census data into marginals
## Reads .sav file extracted by fetch_1971.R and creates marginals_1971.rds
## Variables: age × gender, language × gender (no education, no birthplace)

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1971"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read .sav file
message("Reading .sav file...")
data <- read_sav(file.path(raw_dir, "a1dem001.sav"))

# Filter for Quebec (PROV == 24 in 1971)
message("Filtering for Quebec (PROV == 24)...")
data_qc <- data %>% filter(as.numeric(PROV) == 24)
message("Quebec rows: ", nrow(data_qc))

# Variable structure from .sps file:
# Age (15+ only):
#   V6: Males 15-24, V7: Males 25-34, V8: Males 35-44, V9: Males 45-54, V10: Males 55-64, V11: Males 65+
#   V14: Females 15-24, V15: Females 25-34, V16: Females 35-44, V17: Females 45-54, V18: Females 55-64, V19: Females 65+
# Mother tongue:
#   V30: Males English, V31: Males French, V32-V38: Males other languages
#   V39: Females English, V40: Females French, V41-V47: Females other languages

# Age marginals (15+ only)
message("Creating age marginals...")
age_male <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "male",
  n = c(
    sum(data_qc$V6, na.rm = TRUE),
    sum(data_qc$V7, na.rm = TRUE),
    sum(data_qc$V8, na.rm = TRUE),
    sum(data_qc$V9, na.rm = TRUE),
    sum(data_qc$V10, na.rm = TRUE),
    sum(data_qc$V11, na.rm = TRUE)
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    sum(data_qc$V14, na.rm = TRUE),
    sum(data_qc$V15, na.rm = TRUE),
    sum(data_qc$V16, na.rm = TRUE),
    sum(data_qc$V17, na.rm = TRUE),
    sum(data_qc$V18, na.rm = TRUE),
    sum(data_qc$V19, na.rm = TRUE)
  )
)

# Language marginals (mother tongue)
message("Creating language marginals...")
lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    sum(data_qc$V30, na.rm = TRUE),
    sum(data_qc$V31, na.rm = TRUE),
    sum(data_qc$V32, data_qc$V33, data_qc$V34, data_qc$V35,
        data_qc$V36, data_qc$V37, data_qc$V38, na.rm = TRUE)
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    sum(data_qc$V39, na.rm = TRUE),
    sum(data_qc$V40, na.rm = TRUE),
    sum(data_qc$V41, data_qc$V42, data_qc$V43, data_qc$V44,
        data_qc$V45, data_qc$V46, data_qc$V47, na.rm = TRUE)
  )
)

# Combine all marginals
marginals <- bind_rows(age_male, age_female, lang_male, lang_female) %>%
  mutate(census_year = 1971L) %>%
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

message("\nNote: education and birthplace not available for 1971")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1971.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1971.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
