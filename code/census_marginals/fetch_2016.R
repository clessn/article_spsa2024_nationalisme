## cancensus 2016
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2016.rds

library(cancensus)
library(dplyr)
library(tidyr)

out_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# FETCH DATA FOR QUEBEC
# =============================================================================

message("Fetching 2016 census data for Quebec...")

vectors_to_fetch <- c(
  # Age by sex (15+)
  # Male 15-64
  "v_CA16_65",   # 15-19
  "v_CA16_83",   # 20-24
  "v_CA16_101",  # 25-29
  "v_CA16_119",  # 30-34
  "v_CA16_137",  # 35-39
  "v_CA16_155",  # 40-44
  "v_CA16_173",  # 45-49
  "v_CA16_191",  # 50-54
  "v_CA16_209",  # 55-59
  "v_CA16_227",  # 60-64
  # Male 65+
  "v_CA16_248",  # 65-69
  "v_CA16_266",  # 70-74
  "v_CA16_284",  # 75-79
  "v_CA16_302",  # 80-84
  "v_CA16_320",  # 85+
  # Female 15-64
  "v_CA16_66",   # 15-19
  "v_CA16_84",   # 20-24
  "v_CA16_102",  # 25-29
  "v_CA16_120",  # 30-34
  "v_CA16_138",  # 35-39
  "v_CA16_156",  # 40-44
  "v_CA16_174",  # 45-49
  "v_CA16_192",  # 50-54
  "v_CA16_210",  # 55-59
  "v_CA16_228",  # 60-64
  # Female 65+
  "v_CA16_249",  # 65-69
  "v_CA16_267",  # 70-74
  "v_CA16_285",  # 75-79
  "v_CA16_303",  # 80-84
  "v_CA16_321",  # 85+

  # Language - mother tongue by sex
  "v_CA16_531",  # English Male
  "v_CA16_532",  # English Female
  "v_CA16_534",  # French Male
  "v_CA16_535",  # French Female
  "v_CA16_564",  # Non-official Male
  "v_CA16_565",  # Non-official Female

  # Education by sex
  "v_CA16_5052",  # Total Male 15+
  "v_CA16_5053",  # Total Female 15+
  "v_CA16_5061",  # Postsecondary Male
  "v_CA16_5062"   # Postsecondary Female
)

qc_data <- get_census(
  dataset = "CA16",
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
    get_val("v_CA16_65") + get_val("v_CA16_83"),
    get_val("v_CA16_101") + get_val("v_CA16_119"),
    get_val("v_CA16_137") + get_val("v_CA16_155"),
    get_val("v_CA16_173") + get_val("v_CA16_191"),
    get_val("v_CA16_209") + get_val("v_CA16_227"),
    get_val("v_CA16_248") + get_val("v_CA16_266") + get_val("v_CA16_284") +
      get_val("v_CA16_302") + get_val("v_CA16_320")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA16_66") + get_val("v_CA16_84"),
    get_val("v_CA16_102") + get_val("v_CA16_120"),
    get_val("v_CA16_138") + get_val("v_CA16_156"),
    get_val("v_CA16_174") + get_val("v_CA16_192"),
    get_val("v_CA16_210") + get_val("v_CA16_228"),
    get_val("v_CA16_249") + get_val("v_CA16_267") + get_val("v_CA16_285") +
      get_val("v_CA16_303") + get_val("v_CA16_321")
  )
)

# --- LANGUAGE MARGINALS (by sex) ---
message("Creating language marginals...")

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    get_val("v_CA16_531"),
    get_val("v_CA16_534"),
    get_val("v_CA16_564")
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    get_val("v_CA16_532"),
    get_val("v_CA16_535"),
    get_val("v_CA16_565")
  )
)

# --- EDUCATION MARGINALS (by sex) ---
message("Creating education marginals...")

male_total <- get_val("v_CA16_5052")
female_total <- get_val("v_CA16_5053")
male_postsec <- get_val("v_CA16_5061")
female_postsec <- get_val("v_CA16_5062")

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
  mutate(census_year = 2016L) %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(census_year, variable, category, gender, n, prop)

message("\n=== Summary ===")
marginals %>%
  group_by(variable, gender) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  print()

saveRDS(marginals, file.path(out_dir, "marginals_2016.rds"))
message("\nSaved to: ", file.path(out_dir, "marginals_2016.rds"))

message("\n=== Preview ===")
print(marginals)
