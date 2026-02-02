## cancensus 1996
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_1996.rds
##
## Note: Language by sex NOT available in cancensus for 1996 - using gender = "total"

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 1996 census data for Quebec...")

# Define vectors to fetch
vectors_to_fetch <- c(

# Age by sex (15+)
  # Male
  "v_CA1996_15",  # 15-19
"v_CA1996_16",  # 20-24
  "v_CA1996_17",  # 25-29
  "v_CA1996_18",  # 30-34
  "v_CA1996_19",  # 35-39
  "v_CA1996_20",  # 40-44
  "v_CA1996_21",  # 45-49
  "v_CA1996_22",  # 50-54
  "v_CA1996_23",  # 55-59
  "v_CA1996_24",  # 60-64
  "v_CA1996_25",  # 65-69
  "v_CA1996_26",  # 70-74
  "v_CA1996_27",  # 75-79
  "v_CA1996_28",  # 80-84
  "v_CA1996_29",  # 85+
  # Female
  "v_CA1996_39",  # 15-19
  "v_CA1996_40",  # 20-24
  "v_CA1996_41",  # 25-29
  "v_CA1996_42",  # 30-34
  "v_CA1996_43",  # 35-39
  "v_CA1996_44",  # 40-44
  "v_CA1996_45",  # 45-49
  "v_CA1996_46",  # 50-54
  "v_CA1996_47",  # 55-59
  "v_CA1996_48",  # 60-64
  "v_CA1996_49",  # 65-69
  "v_CA1996_50",  # 70-74
  "v_CA1996_51",  # 75-79
  "v_CA1996_52",  # 80-84
  "v_CA1996_53",  # 85+

  # Language - mother tongue (total only, no sex breakdown available)
  "v_CA1996_235",  # English
  "v_CA1996_236",  # French
  "v_CA1996_237",  # Non-official languages

  # Education by sex (postsecondary)
  "v_CA1996_1361",  # Males with postsecondary
  "v_CA1996_1373"   # Females with postsecondary
)

qc_data <- get_census(
  dataset = "CA1996",
  regions = list(PR = "24"),
  vectors = vectors_to_fetch,
  level = "PR"
)

# =============================================================================
# CREATE MARGINALS
# =============================================================================

# Helper to extract value
get_val <- function(vec) {
  col <- names(qc_data)[grepl(paste0("^", vec, ":"), names(qc_data))]
  if (length(col) == 1) return(qc_data[[col]])
  return(NA)
}

# --- AGE MARGINALS (15+, aggregated to 10-year groups) ---
message("Creating age marginals...")

age_male <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "male",
  n = c(
    get_val("v_CA1996_15") + get_val("v_CA1996_16"),
    get_val("v_CA1996_17") + get_val("v_CA1996_18"),
    get_val("v_CA1996_19") + get_val("v_CA1996_20"),
    get_val("v_CA1996_21") + get_val("v_CA1996_22"),
    get_val("v_CA1996_23") + get_val("v_CA1996_24"),
    get_val("v_CA1996_25") + get_val("v_CA1996_26") + get_val("v_CA1996_27") +
      get_val("v_CA1996_28") + get_val("v_CA1996_29")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA1996_39") + get_val("v_CA1996_40"),
    get_val("v_CA1996_41") + get_val("v_CA1996_42"),
    get_val("v_CA1996_43") + get_val("v_CA1996_44"),
    get_val("v_CA1996_45") + get_val("v_CA1996_46"),
    get_val("v_CA1996_47") + get_val("v_CA1996_48"),
    get_val("v_CA1996_49") + get_val("v_CA1996_50") + get_val("v_CA1996_51") +
      get_val("v_CA1996_52") + get_val("v_CA1996_53")
  )
)

# --- LANGUAGE MARGINALS (total only - no sex breakdown in cancensus) ---
message("Creating language marginals (total only - no sex breakdown available)...")

lang_total <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "total",
  n = c(
    get_val("v_CA1996_235"),
    get_val("v_CA1996_236"),
    get_val("v_CA1996_237")
  )
)

# --- EDUCATION MARGINALS (by sex) ---
message("Creating education marginals...")

# Calculate totals
male_15plus <- sum(age_male$n)
female_15plus <- sum(age_female$n)
male_postsec <- get_val("v_CA1996_1361")
female_postsec <- get_val("v_CA1996_1373")

edu_male <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "male",
  n = c(
    male_15plus - male_postsec,  # Without postsecondary
    male_postsec                  # With postsecondary
  )
)

edu_female <- tibble(
  variable = "education",
  category = c("0", "1"),
  gender = "female",
  n = c(
    female_15plus - female_postsec,  # Without postsecondary
    female_postsec                    # With postsecondary
  )
)

# =============================================================================
# COMBINE AND SAVE
# =============================================================================

marginals <- bind_rows(age_male, age_female, lang_total, edu_male, edu_female) %>%
  mutate(census_year = 1996L) %>%
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

message("\nNote: language has gender='total' (no sex breakdown in cancensus for 1996)")

# Save
saveRDS(marginals, file.path(out_dir, "marginals_1996.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_1996.rds"))

# Preview
message("\n=== Preview ===")
print(marginals)
