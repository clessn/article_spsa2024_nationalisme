## Fetch 1976 census data
## Source: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1976/statistics
## age by sex: http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadema10_spss.zip
## mother tongue by sex: http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadema20_spss.zip

library(dplyr)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1976"

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

# Files to download with prefixes
files <- list(
  AGE = "http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadema10_spss.zip",
  LANG = "http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadema20_spss.zip"
)

# Download and extract each file with prefix
for (prefix in names(files)) {
  url <- files[[prefix]]
  zip_name <- basename(url)
  zip_file <- file.path(raw_dir, zip_name)

  if (!file.exists(zip_file)) {
    message("Downloading ", prefix, ": ", zip_name, "...")
    download.file(url, zip_file, mode = "wb")
  }

  # Extract to temp dir, rename with prefix
  temp_dir <- file.path(raw_dir, paste0("temp_", prefix))
  dir.create(temp_dir, showWarnings = FALSE)
  unzip(zip_file, exdir = temp_dir)

  # Rename extracted files with prefix
  extracted_files <- list.files(temp_dir, full.names = TRUE)
  for (f in extracted_files) {
    new_name <- file.path(raw_dir, paste0(prefix, "_", basename(f)))
    file.rename(f, new_name)
  }
  unlink(temp_dir, recursive = TRUE)
}

message("\nFiles extracted to: ", raw_dir)
message("Contents:")
print(list.files(raw_dir, pattern = "\\.(sav|SAV)$"))

# Create codebooks
sav_files <- list.files(raw_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)
if (length(sav_files) > 0) {
  message("\nCreating codebooks...")
  for (f in sav_files) {
    tryCatch({
      cb <- sondr::sav_to_codebook(f)
      cb_name <- paste0(tools::file_path_sans_ext(basename(f)), "_codebook.rds")
      saveRDS(cb, file.path(raw_dir, cb_name))
      message("  Created: ", cb_name)
    }, error = function(e) {
      message("  Error creating codebook for ", basename(f), ": ", e$message)
    })
  }
}