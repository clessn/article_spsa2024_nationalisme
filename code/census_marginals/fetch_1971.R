## Fetch 1971 census data
## Source: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1971/statistics/user-summary-tapes
## Using User Summary Tapes - Short Form Demographics (A1DEM001)
## Variables: age, sex, language, birthplace (no education in 1971)

library(dplyr)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1971"

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

# URL
url <- "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1971/statistics/ust/a1dem001_spss.zip"

# Download
zip_file <- file.path(raw_dir, "a1dem001_spss.zip")
if (!file.exists(zip_file)) {
  message("Downloading a1dem001_spss.zip...")
  download.file(url, zip_file, mode = "wb")
}

# Extract
unzip(zip_file, exdir = raw_dir)

message("Files extracted to: ", raw_dir)
message("Contents:")
print(list.files(raw_dir, recursive = TRUE))

# Create codebook from .sav files
