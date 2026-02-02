## cancensus 2006
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2006.rds
##
## Note: Language by sex NOT available in cancensus for 2006 - using gender = "total"
## Note: Education by sex is for 25-64 years (not 15+)

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 2006 census data for Quebec...")

vectors_to_fetch <- c(
  # Age by sex (15+)
  # Male (children of v_CA06_3)
  "v_CA06_7",   # 15-19
  "v_CA06_8",   # 20-24
  "v_CA06_9",   # 25-29
  "v_CA06_10",  # 30-34
  "v_CA06_11",  # 35-39
  "v_CA06_12",  # 40-44
  "v_CA06_13",  # 45-49
  "v_CA06_14",  # 50-54
  "v_CA06_15",  # 55-59
  "v_CA06_16",  # 60-64
  "v_CA06_17",  # 65-69
  "v_CA06_18",  # 70-74
  "v_CA06_19",  # 75-79
  "v_CA06_20",  # 80-84
  "v_CA06_21",  # 85+
  # Female (children of v_CA06_22)
  "v_CA06_26",  # 15-19
  "v_CA06_27",  # 20-24
  "v_CA06_28",  # 25-29
  "v_CA06_29",  # 30-34
  "v_CA06_30",  # 35-39
  "v_CA06_31",  # 40-44
  "v_CA06_32",  # 45-49
  "v_CA06_33",  # 50-54
  "v_CA06_34",  # 55-59
  "v_CA06_35",  # 60-64
  "v_CA06_36",  # 65-69
  "v_CA06_37",  # 70-74
  "v_CA06_38",  # 75-79
  "v_CA06_39",  # 80-84
  "v_CA06_40",  # 85+

  # Language - mother tongue (total only)
  "v_CA06_142",  # English
  "v_CA06_143",  # French
  "v_CA06_144",  # Non-official languages

  # Education by sex (postsecondary, 25-64 years)
  "v_CA06_1208",  # Males 25-64 with postsecondary
  "v_CA06_1221"   # Females 25-64 with postsecondary
)

qc_data <- get_census(
  dataset = "CA06",
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
    get_val("v_CA06_7") + get_val("v_CA06_8"),
    get_val("v_CA06_9") + get_val("v_CA06_10"),
    get_val("v_CA06_11") + get_val("v_CA06_12"),
    get_val("v_CA06_13") + get_val("v_CA06_14"),
    get_val("v_CA06_15") + get_val("v_CA06_16"),
    get_val("v_CA06_17") + get_val("v_CA06_18") + get_val("v_CA06_19") +
      get_val("v_CA06_20") + get_val("v_CA06_21")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA06_26") + get_val("v_CA06_27"),
    get_val("v_CA06_28") + get_val("v_CA06_29"),
    get_val("v_CA06_30") + get_val("v_CA06_31"),
    get_val("v_CA06_32") + get_val("v_CA06_33"),
    get_val("v_CA06_34") + get_val("v_CA06_35"),
    get_val("v_CA06_36") + get_val("v_CA06_37") + get_val("v_CA06_38") +
      get_val("v_CA06_39") + get_val("v_CA06_40")
  )
)

# --- LANGUAGE MARGINALS ---
message("Creating language marginals (total only)...")

lang_total <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "total",
  n = c(
    get_val("v_CA06_142"),
    get_val("v_CA06_143"),
    get_val("v_CA06_144")
  )
)

# --- EDUCATION MARGINALS ---
message("Creating education marginals (25-64 years)...")

# For education, use 25-64 population from age groups
male_25_64 <- get_val("v_CA06_9") + get_val("v_CA06_10") + get_val("v_CA06_11") +
              get_val("v_CA06_12") + get_val("v_CA06_13") + get_val("v_CA06_14") +
              get_val("v_CA06_15") + get_val("v_CA06_16")
female_25_64 <- get_val("v_CA06_28") + get_val("v_CA06_29") + get_val("v_CA06_30") +
                get_val("v_CA06_31") + get_val("v_CA06_32") + get_val("v_CA06_33") +
                get_val("v_CA06_34") + get_val("v_CA06_35")

male_postsec <- get_val("v_CA06_1208")
female_postsec <- get_val("v_CA06_1221")

edu_male <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "male",
  n = c(
    male_25_64 - male_postsec,
    male_postsec
  )
)

edu_female <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "female",
  n = c(
    female_25_64 - female_postsec,
    female_postsec
  )
)

# =============================================================================
# COMBINE AND SAVE
# =============================================================================

marginals <- bind_rows(age_male, age_female, lang_total, edu_male, edu_female) %>%
  mutate(census_year = 2006L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

message("\nNote: language has gender='total'; education is for 25-64 years")

saveRDS(marginals, file.path(out_dir, "marginals_2006.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_2006.rds"))

message("\n=== Preview ===")
print(marginals)
