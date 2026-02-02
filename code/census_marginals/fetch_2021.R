## cancensus 2021
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2021.rds
##
## Note: 2021 has language AND education by sex available!

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 2021 census data for Quebec...")

vectors_to_fetch <- c(
  # Age by sex (15+)
  # Male
  "v_CA21_15",   # 15-19
  "v_CA21_33",   # 20-24
  "v_CA21_36",   # 25-29
  "v_CA21_39",   # 30-34
  "v_CA21_42",   # 35-39
  "v_CA21_45",   # 40-44
  "v_CA21_48",   # 45-49
  "v_CA21_51",   # 50-54
  "v_CA21_54",   # 55-59
  "v_CA21_57",   # 60-64
  "v_CA21_60",   # 65-69
  "v_CA21_63",   # 70-74
  "v_CA21_66",   # 75-79
  "v_CA21_69",   # 80-84
  "v_CA21_72",   # 85+
  # Female
  "v_CA21_16",   # 15-19
  "v_CA21_34",   # 20-24
  "v_CA21_37",   # 25-29
  "v_CA21_40",   # 30-34
  "v_CA21_43",   # 35-39
  "v_CA21_46",   # 40-44
  "v_CA21_49",   # 45-49
  "v_CA21_52",   # 50-54
  "v_CA21_55",   # 55-59
  "v_CA21_58",   # 60-64
  "v_CA21_61",   # 65-69
  "v_CA21_64",   # 70-74
  "v_CA21_67",   # 75-79
  "v_CA21_70",   # 80-84
  "v_CA21_73",   # 85+

  # Language - mother tongue by sex (single responses)
  "v_CA21_1138",  # English Male
  "v_CA21_1139",  # English Female
  "v_CA21_1141",  # French Male
  "v_CA21_1142",  # French Female
  "v_CA21_1144",  # Non-official Male
  "v_CA21_1145",  # Non-official Female

  # Education by sex (15+)
  "v_CA21_5820",  # Total Male 15+ (for denominator)
  "v_CA21_5821",  # Total Female 15+ (for denominator)
  "v_CA21_5850",  # Postsecondary Male
  "v_CA21_5851"   # Postsecondary Female
)

qc_data <- get_census(
  dataset = "CA21",
  regions = list(PR = "24"),
  vectors = vectors_to_fetch,
  level = "PR"
)

# =============================================================================
# CREATE MARGINALS
# =============================================================================

get_val <- function(vec) {
  col <- names(qc_data)[grepl(paste0("^", vec, ":"), names(qc_data))]
  if (length(col) == 1) return(qc_data[[col]])
  return(NA)
}

# --- AGE MARGINALS ---
message("Creating age marginals...")

age_male <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "male",
  n = c(
    get_val("v_CA21_15") + get_val("v_CA21_33"),
    get_val("v_CA21_36") + get_val("v_CA21_39"),
    get_val("v_CA21_42") + get_val("v_CA21_45"),
    get_val("v_CA21_48") + get_val("v_CA21_51"),
    get_val("v_CA21_54") + get_val("v_CA21_57"),
    get_val("v_CA21_60") + get_val("v_CA21_63") + get_val("v_CA21_66") +
      get_val("v_CA21_69") + get_val("v_CA21_72")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA21_16") + get_val("v_CA21_34"),
    get_val("v_CA21_37") + get_val("v_CA21_40"),
    get_val("v_CA21_43") + get_val("v_CA21_46"),
    get_val("v_CA21_49") + get_val("v_CA21_52"),
    get_val("v_CA21_55") + get_val("v_CA21_58"),
    get_val("v_CA21_61") + get_val("v_CA21_64") + get_val("v_CA21_67") +
      get_val("v_CA21_70") + get_val("v_CA21_73")
  )
)

# --- LANGUAGE MARGINALS (by sex!) ---
message("Creating language marginals (by sex)...")

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    get_val("v_CA21_1138"),
    get_val("v_CA21_1141"),
    get_val("v_CA21_1144")
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    get_val("v_CA21_1139"),
    get_val("v_CA21_1142"),
    get_val("v_CA21_1145")
  )
)

# --- EDUCATION MARGINALS (by sex) ---
message("Creating education marginals...")

male_total <- get_val("v_CA21_5820")
female_total <- get_val("v_CA21_5821")
male_postsec <- get_val("v_CA21_5850")
female_postsec <- get_val("v_CA21_5851")

edu_male <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "male",
  n = c(
    male_total - male_postsec,
    male_postsec
  )
)

edu_female <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "female",
  n = c(
    female_total - female_postsec,
    female_postsec
  )
)

# =============================================================================
# COMBINE AND SAVE
# =============================================================================

marginals <- bind_rows(age_male, age_female, lang_male, lang_female, edu_male, edu_female) %>%
  mutate(census_year = 2021L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

saveRDS(marginals, file.path(out_dir, "marginals_2021.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_2021.rds"))

message("\n=== Preview ===")
print(marginals)
