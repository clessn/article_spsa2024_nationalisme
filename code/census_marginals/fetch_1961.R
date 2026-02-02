## On va fetch 1961 ici
## Source: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1961/statistics

## Lien du zip pour les male: https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_male_spss.zip
## Lien du zip pour les female: https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_female_spss.zip

# Packages
library(dplyr)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961"

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

# URLs
url_male <- "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_male_spss.zip"
url_female <- "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_female_spss.zip"

# Download and extract male file
zip_male <- file.path(raw_dir, "population_file_male_spss.zip")
if (!file.exists(zip_male)) {
  message("Downloading male population file...")
  download.file(url_male, zip_male, mode = "wb")
}

# Extract to temp dir, then rename with M_ prefix
temp_dir_m <- file.path(raw_dir, "temp_male")
dir.create(temp_dir_m, showWarnings = FALSE)
unzip(zip_male, exdir = temp_dir_m)
male_files <- list.files(temp_dir_m, full.names = TRUE)
for (f in male_files) {
  new_name <- file.path(raw_dir, paste0("M_", basename(f)))
  file.rename(f, new_name)
}
unlink(temp_dir_m, recursive = TRUE)

# Download and extract female file
zip_female <- file.path(raw_dir, "population_file_female_spss.zip")
if (!file.exists(zip_female)) {
  message("Downloading female population file...")
  download.file(url_female, zip_female, mode = "wb")
}

# Extract to temp dir, then rename with F_ prefix
temp_dir_f <- file.path(raw_dir, "temp_female")
dir.create(temp_dir_f, showWarnings = FALSE)
unzip(zip_female, exdir = temp_dir_f)
female_files <- list.files(temp_dir_f, full.names = TRUE)
for (f in female_files) {
  new_name <- file.path(raw_dir, paste0("F_", basename(f)))
  file.rename(f, new_name)
}
unlink(temp_dir_f, recursive = TRUE)

message("Files extracted to: ", raw_dir)
message("Contents:")
print(list.files(raw_dir, pattern = "\\.(sav|SAV)$"))

message("Codebook creation for each dataset")

codebook_m <- sondr::sav_to_codebook(haven::read_sav("SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961/M_pop61-m.sav"))
saveRDS(codebook_m, "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961/M_codebook.rds")

codebook_f <- sondr::sav_to_codebook(haven::read_sav("SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961/F_pop61-f.sav"))
saveRDS(codebook_f, "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1961/F_codebook.rds")
