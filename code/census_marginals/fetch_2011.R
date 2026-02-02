## cancensus 2011
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2011.rds
##
## Note: 2011 has language AND education by sex available!

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 2011 census data for Quebec...")

vectors_to_fetch <- c(
  # Age by sex (15+)
  # Male
  "v_CA11F_18",  # 15-19
  "v_CA11F_36",  # 20-24
  "v_CA11F_39",  # 25-29
  "v_CA11F_42",  # 30-34
  "v_CA11F_45",  # 35-39
  "v_CA11F_48",  # 40-44
  "v_CA11F_51",  # 45-49
  "v_CA11F_54",  # 50-54
  "v_CA11F_57",  # 55-59
  "v_CA11F_60",  # 60-64
  "v_CA11F_63",  # 65-69
  "v_CA11F_66",  # 70-74
  "v_CA11F_69",  # 75-79
  "v_CA11F_72",  # 80-84
  "v_CA11F_75",  # 85+
  # Female
  "v_CA11F_19",  # 15-19
  "v_CA11F_37",  # 20-24
  "v_CA11F_40",  # 25-29
  "v_CA11F_43",  # 30-34
  "v_CA11F_46",  # 35-39
  "v_CA11F_49",  # 40-44
  "v_CA11F_52",  # 45-49
  "v_CA11F_55",  # 50-54
  "v_CA11F_58",  # 55-59
  "v_CA11F_61",  # 60-64
  "v_CA11F_64",  # 65-69
  "v_CA11F_67",  # 70-74
  "v_CA11F_70",  # 75-79
  "v_CA11F_73",  # 80-84
  "v_CA11F_76",  # 85+

  # Language - mother tongue by sex (single responses)
  "v_CA11F_225",  # English Male
  "v_CA11F_226",  # English Female
  "v_CA11F_228",  # French Male
  "v_CA11F_229",  # French Female
  "v_CA11F_231",  # Non-official Male
  "v_CA11F_232",  # Non-official Female

  # Education by sex
  "v_CA11N_1772",  # Total Male 15+ (for denominator)
  "v_CA11N_1773",  # Total Female 15+ (for denominator)
  "v_CA11N_1781",  # Postsecondary Male
  "v_CA11N_1782"   # Postsecondary Female
)

qc_data <- get_census(
  dataset = "CA11",
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
    get_val("v_CA11F_18") + get_val("v_CA11F_36"),
    get_val("v_CA11F_39") + get_val("v_CA11F_42"),
    get_val("v_CA11F_45") + get_val("v_CA11F_48"),
    get_val("v_CA11F_51") + get_val("v_CA11F_54"),
    get_val("v_CA11F_57") + get_val("v_CA11F_60"),
    get_val("v_CA11F_63") + get_val("v_CA11F_66") + get_val("v_CA11F_69") +
      get_val("v_CA11F_72") + get_val("v_CA11F_75")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA11F_19") + get_val("v_CA11F_37"),
    get_val("v_CA11F_40") + get_val("v_CA11F_43"),
    get_val("v_CA11F_46") + get_val("v_CA11F_49"),
    get_val("v_CA11F_52") + get_val("v_CA11F_55"),
    get_val("v_CA11F_58") + get_val("v_CA11F_61"),
    get_val("v_CA11F_64") + get_val("v_CA11F_67") + get_val("v_CA11F_70") +
      get_val("v_CA11F_73") + get_val("v_CA11F_76")
  )
)

# --- LANGUAGE MARGINALS (by sex!) ---
message("Creating language marginals (by sex)...")

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    get_val("v_CA11F_225"),
    get_val("v_CA11F_228"),
    get_val("v_CA11F_231")
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    get_val("v_CA11F_226"),
    get_val("v_CA11F_229"),
    get_val("v_CA11F_232")
  )
)

# --- EDUCATION MARGINALS (by sex) ---
message("Creating education marginals...")

male_total <- get_val("v_CA11N_1772")
female_total <- get_val("v_CA11N_1773")
male_postsec <- get_val("v_CA11N_1781")
female_postsec <- get_val("v_CA11N_1782")

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
  mutate(census_year = 2011L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

saveRDS(marginals, file.path(out_dir, "marginals_2011.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_2011.rds"))

message("\n=== Preview ===")
print(marginals)
