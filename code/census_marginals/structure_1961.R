## Structure 1961 census data into marginals
## Reads .sav files extracted by fetch_1961.R and creates marginals_1961.rds

library(dplyr)
library(tidyr)
library(haven)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961"
out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# Read .sav files
message("Reading .sav files...")
male <- read_sav(file.path(raw_dir, "M_pop61-m.sav"))
female <- read_sav(file.path(raw_dir, "F_pop61-f.sav"))

# Filter for Quebec (PROV == 4)
message("Filtering for Quebec (PROV == 4)...")
male_qc <- male %>% filter(PROV == "4")
female_qc <- female %>% filter(PROV == "4")

message("Quebec rows: Male = ", nrow(male_qc), ", Female = ", nrow(female_qc))

# Function to sum columns and create marginal tibble
create_marginals <- function(data, gender_label) {

  # Age groups (15+ only: AGE4 onwards)
  # Note: AGE11 is mislabeled "45-49" in codebook but should be "70-74" (fills gap)
  # AGE4=15-19, AGE5=20-24, AGE6=25-29, AGE7=30-34, AGE8=35-39,
  # AGE9=40-44, AGE10=45-49, AGE11=70-74, AGE12=50-54, AGE13=55-59,
  # AGE14=60-64, AGE15=65-69, AGE16=75-79, AGE17=80-84, AGE18=85-89,
  # AGE19=90-94, AGE20=95+, AGE21=99+
  age_vars <- c("AGE4", "AGE5", "AGE6", "AGE7", "AGE8", "AGE9", "AGE10",
                "AGE11", "AGE12", "AGE13", "AGE14", "AGE15", "AGE16",
                "AGE17", "AGE18", "AGE19", "AGE20", "AGE21")
  age_labels <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
                  "45-49", "70-74", "50-54", "55-59", "60-64", "65-69",
                  "75-79", "80-84", "85-89", "90-94", "95+", "99+")

  age_marginals <- tibble(
    variable = "age_group",
    category = age_labels,
    n = sapply(age_vars, function(v) sum(data[[v]], na.rm = TRUE))
  )

  # Mother tongue: english, french, other
  lang_marginals <- tibble(
    variable = "language",
    category = c("english", "french", "other"),
    n = c(
      sum(data$MTONG1, na.rm = TRUE),
      sum(data$MTONG2, na.rm = TRUE),
      sum(data$MTONG3, data$MTONG4, data$MTONG5, data$MTONG6, data$MTONG7,
          data$MTONG8, data$MTONG9, data$MTONG10, data$MTONG11, data$MTONG12,
          data$MTONG13, data$MTONG14, data$MTONG15, data$MTONG16, data$MTONG17,
          data$MTONG18, data$MTONG19, data$MTONG20, data$MTONG21, data$MTONG22,
          data$MTONG23, data$MTONG24, data$MTONG25, data$MTONG26, data$MTONG27,
          data$MTONG28, data$MTONG29, data$MTONG30, na.rm = TRUE)
    )
  )

  # Education: 0 = no post-secondary (EDU1-7), 1 = post-secondary+ (EDU8-10)
  edu_marginals <- tibble(
    variable = "education",
    category = c("0", "1"),
    n = c(
      sum(data$EDU1, data$EDU2, data$EDU3, data$EDU4, data$EDU5,
          data$EDU6, data$EDU7, na.rm = TRUE),
      sum(data$EDU8, data$EDU9, data$EDU10, na.rm = TRUE)
    )
  )

  # Combine and add gender
  bind_rows(age_marginals, lang_marginals, edu_marginals) %>%
    mutate(gender = gender_label)
}

# Create marginals for each gender
message("Creating marginals...")
marginals_male <- create_marginals(male_qc, "male")
marginals_female <- create_marginals(female_qc, "female")

# Combine
marginals <- bind_rows(marginals_male, marginals_female) %>%
  mutate(census_year = 1961L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

# Summary
message("\n=== Summary ===")
marginals %>%
  group_by(variable) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1961.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1961.rds"))

# Preview
message("\n=== Preview ===")
print(marginals, n = 30)
