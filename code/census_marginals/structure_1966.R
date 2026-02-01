## Structure 1966 census data into marginals
## Reads .sav file extracted by fetch_1966.R and creates marginals_1966.rds
## Note: 1966 only has age × gender data (no education, no language available digitally)

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1966"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read .sav file
message("Reading .sav file...")
data <- read_sav(file.path(raw_dir, "pop66s.sav"))

# Filter for Quebec (PROV == "4")
message("Filtering for Quebec (PROV == '4')...")
data_qc <- data %>% filter(PROV == "4")
message("Quebec rows: ", nrow(data_qc))

# Age variables structure:
# MNFAGE0-100: Male Non-Farm by single year of age
# MFAGE0-100: Male Farm by single year of age
# FNFAGE0-100: Female Non-Farm by single year of age
# FFAGE0-100: Female Farm by single year of age

# Define age groups (matching 1961 structure, 15+ only)
age_groups <- list(
  "15-19" = 15:19,
  "20-24" = 20:24,
  "25-29" = 25:29,
  "30-34" = 30:34,
  "35-39" = 35:39,
  "40-44" = 40:44,
  "45-49" = 45:49,
  "50-54" = 50:54,
  "55-59" = 55:59,
  "60-64" = 60:64,
  "65-69" = 65:69,
  "70-74" = 70:74,
  "75-79" = 75:79,
  "80-84" = 80:84,
  "85-89" = 85:89,
  "90-94" = 90:94,
  "95+" = 95:100
)

# Function to sum age columns for a gender
sum_age_group <- function(data, prefix_nf, prefix_f, ages) {
  total <- 0
  for (age in ages) {
    # Non-farm column
    col_nf <- paste0(prefix_nf, age)
    if (col_nf %in% names(data)) {
      total <- total + sum(data[[col_nf]], na.rm = TRUE)
    }
    # Farm column
    col_f <- paste0(prefix_f, age)
    if (col_f %in% names(data)) {
      total <- total + sum(data[[col_f]], na.rm = TRUE)
    }
  }
  total
}

# Create marginals for males
message("Creating male age marginals...")
male_marginals <- tibble(
  variable = "age_group",
  category = names(age_groups),
  gender = "male",
  n = sapply(age_groups, function(ages) {
    sum_age_group(data_qc, "MNFAGE", "MFAGE", ages)
  })
)

# Create marginals for females
message("Creating female age marginals...")
female_marginals <- tibble(
  variable = "age_group",
  category = names(age_groups),
  gender = "female",
  n = sapply(age_groups, function(ages) {
    sum_age_group(data_qc, "FNFAGE", "FFAGE", ages)
  })
)

# Combine
marginals <- bind_rows(male_marginals, female_marginals) %>%
  mutate(census_year = 1966L) %>%
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

message("\nNote: education and language not available for 1966 (print-only)")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1966.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1966.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
