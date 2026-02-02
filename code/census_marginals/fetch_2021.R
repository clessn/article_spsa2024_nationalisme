## cancensus 2021
## Fetch census marginals for Quebec using cancensus package
## Output: marginals_2021.rds

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
  "v_CA21_72",   # 15-19
  "v_CA21_90",   # 20-24
  "v_CA21_108",  # 25-29
  "v_CA21_126",  # 30-34
  "v_CA21_144",  # 35-39
  "v_CA21_162",  # 40-44
  "v_CA21_180",  # 45-49
  "v_CA21_198",  # 50-54
  "v_CA21_216",  # 55-59
  "v_CA21_234",  # 60-64
  "v_CA21_255",  # 65-69
  "v_CA21_273",  # 70-74
  "v_CA21_291",  # 75-79
  "v_CA21_309",  # 80-84
  "v_CA21_327",  # 85+
  # Female
  "v_CA21_73",   # 15-19
  "v_CA21_91",   # 20-24
  "v_CA21_109",  # 25-29
  "v_CA21_127",  # 30-34
  "v_CA21_145",  # 35-39
  "v_CA21_163",  # 40-44
  "v_CA21_181",  # 45-49
  "v_CA21_199",  # 50-54
  "v_CA21_217",  # 55-59
  "v_CA21_235",  # 60-64
  "v_CA21_256",  # 65-69
  "v_CA21_274",  # 70-74
  "v_CA21_292",  # 75-79
  "v_CA21_310",  # 80-84
  "v_CA21_328",  # 85+

  # Language - mother tongue by sex
  "v_CA21_1184",  # English Male
  "v_CA21_1185",  # English Female
  "v_CA21_1187",  # French Male
  "v_CA21_1188",  # French Female
  "v_CA21_1190",  # Non-official Male
  "v_CA21_1191",  # Non-official Female

  # Education by sex
  "v_CA21_5818",  # Total Male 15+
  "v_CA21_5819",  # Total Female 15+
  "v_CA21_5827",  # Postsecondary Male
  "v_CA21_5828"   # Postsecondary Female
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
    get_val("v_CA21_72") + get_val("v_CA21_90"),
    get_val("v_CA21_108") + get_val("v_CA21_126"),
    get_val("v_CA21_144") + get_val("v_CA21_162"),
    get_val("v_CA21_180") + get_val("v_CA21_198"),
    get_val("v_CA21_216") + get_val("v_CA21_234"),
    get_val("v_CA21_255") + get_val("v_CA21_273") + get_val("v_CA21_291") +
      get_val("v_CA21_309") + get_val("v_CA21_327")
  )
)

age_female <- tibble(
  variable = "age_group",
  category = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  gender = "female",
  n = c(
    get_val("v_CA21_73") + get_val("v_CA21_91"),
    get_val("v_CA21_109") + get_val("v_CA21_127"),
    get_val("v_CA21_145") + get_val("v_CA21_163"),
    get_val("v_CA21_181") + get_val("v_CA21_199"),
    get_val("v_CA21_217") + get_val("v_CA21_235"),
    get_val("v_CA21_256") + get_val("v_CA21_274") + get_val("v_CA21_292") +
      get_val("v_CA21_310") + get_val("v_CA21_328")
  )
)

# --- LANGUAGE MARGINALS (by sex) ---
message("Creating language marginals...")

lang_male <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "male",
  n = c(
    get_val("v_CA21_1184"),
    get_val("v_CA21_1187"),
    get_val("v_CA21_1190")
  )
)

lang_female <- tibble(
  variable = "language",
  category = c("english", "french", "other"),
  gender = "female",
  n = c(
    get_val("v_CA21_1185"),
    get_val("v_CA21_1188"),
    get_val("v_CA21_1191")
  )
)

# --- EDUCATION MARGINALS (by sex) ---
message("Creating education marginals...")

male_total <- get_val("v_CA21_5818")
female_total <- get_val("v_CA21_5819")
male_postsec <- get_val("v_CA21_5827")
female_postsec <- get_val("v_CA21_5828")

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
