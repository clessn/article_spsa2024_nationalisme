## Fetch 1966 census data
## Source: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1966
## Note: Single ZIP file (not separated by gender like 1961)

# lien: https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1966/population_z10_spss.zip

library(dplyr)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1966"

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

# URL
url <- "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1966/population_z10_spss.zip"

# Download
zip_file <- file.path(raw_dir, "population_z10_spss.zip")
if (!file.exists(zip_file)) {
  message("Downloading 1966 census file...")
  download.file(url, zip_file, mode = "wb")
}

# Extract
unzip(zip_file, exdir = raw_dir)

message("Files extracted to: ", raw_dir)
message("Contents:")
print(list.files(raw_dir, recursive = TRUE))

# Create codebook from .sav files
data <- haven::read_sav("SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1966/pop66s.sav")
cb <- sondr::sav_to_codebook(data)
saveRDS(cb, "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1966/codebook.rds")
