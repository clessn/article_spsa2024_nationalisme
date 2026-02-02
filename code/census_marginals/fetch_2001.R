## cancensus 2001
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2001.rds
##
## Note: Language by sex NOT available in cancensus for 2001 - using gender = "total"

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 2001 census data for Quebec...")

vectors_to_fetch <- c(
  # Age by sex (15+)
  # Male (children of v_CA01_6)
  "v_CA01_10",  # 15-19
  "v_CA01_11",  # 20-24
  "v_CA01_12",  # 25-29
  "v_CA01_13",  # 30-34
  "v_CA01_14",  # 35-39
  "v_CA01_15",  # 40-44
  "v_CA01_16",  # 45-49
  "v_CA01_17",  # 50-54
  "v_CA01_18",  # 55-59
  "v_CA01_19",  # 60-64
  "v_CA01_20",  # 65-69
  "v_CA01_21",  # 70-74
  "v_CA01_22",  # 75-79
  "v_CA01_23",  # 80-84
  "v_CA01_24",  # 85+
  # Female (children of v_CA01_25)
  "v_CA01_29",  # 15-19
  "v_CA01_30",  # 20-24
  "v_CA01_31",  # 25-29
  "v_CA01_32",  # 30-34
  "v_CA01_33",  # 35-39
  "v_CA01_34",  # 40-44
  "v_CA01_35",  # 45-49
  "v_CA01_36",  # 50-54
  "v_CA01_37",  # 55-59
  "v_CA01_38",  # 60-64
  "v_CA01_39",  # 65-69
  "v_CA01_40",  # 70-74
  "v_CA01_41",  # 75-79
  "v_CA01_42",  # 80-84
  "v_CA01_43",  # 85+

  # Language - mother tongue (total only)
  "v_CA01_135",  # English
  "v_CA01_136",  # French
  "v_CA01_137",  # Non-official languages

  # Education by sex (postsecondary)
  "v_CA01_1360",  # Males with postsecondary
  "v_CA01_1372"   # Females with postsecondary
)

qc_data <- get_census(
  dataset = "CA01",
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
    get_val("v_CA01_10") + get_val("v_CA01_11"),
    get_val("v_CA01_12") + get_val("v_CA01_13"),
    get_val("v_CA01_14") + get_val("v_CA01_15"),
    get_val("v_CA01_16") + get_val("v_CA01_17"),
    get_val("v_CA01_18") + get_val("v_CA01_19"),
    get_val("v_CA01_20") + get_val("v_CA01_21") + get_val("v_CA01_22") +
      get_val("v_CA01_23") + get_val("v_CA01_24")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA01_29") + get_val("v_CA01_30"),
    get_val("v_CA01_31") + get_val("v_CA01_32"),
    get_val("v_CA01_33") + get_val("v_CA01_34"),
    get_val("v_CA01_35") + get_val("v_CA01_36"),
    get_val("v_CA01_37") + get_val("v_CA01_38"),
    get_val("v_CA01_39") + get_val("v_CA01_40") + get_val("v_CA01_41") +
      get_val("v_CA01_42") + get_val("v_CA01_43")
  )
)

# --- LANGUAGE MARGINALS ---
message("Creating language marginals (total only)...")

lang_total <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "total",
  n = c(
    get_val("v_CA01_135"),
    get_val("v_CA01_136"),
    get_val("v_CA01_137")
  )
)

# --- EDUCATION MARGINALS ---
message("Creating education marginals...")

male_15plus <- sum(age_male$n)
female_15plus <- sum(age_female$n)
male_postsec <- get_val("v_CA01_1360")
female_postsec <- get_val("v_CA01_1372")

edu_male <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "male",
  n = c(
    male_15plus - male_postsec,
    male_postsec
  )
)

edu_female <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "female",
  n = c(
    female_15plus - female_postsec,
    female_postsec
  )
)

# =============================================================================
# COMBINE AND SAVE
# =============================================================================

marginals <- bind_rows(age_male, age_female, lang_total, edu_male, edu_female) %>%
  mutate(census_year = 2001L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

message("\nNote: language has gender='total' (no sex breakdown in cancensus for 2001)")

saveRDS(marginals, file.path(out_dir, "marginals_2001.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_2001.rds"))

message("\n=== Preview ===")
print(marginals)
