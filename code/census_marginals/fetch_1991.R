## Fetch 1991 census data
## Source: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1991/statistics
## age sex: https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1991/statistics/bst/e9102ea.zip
## mother tongue: https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1991/statistics/bst/l9101ea.zip

library(dplyr)

# Paths
raw_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals/raw/1991"

# Create directory if it doesn't exist
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

# Files to download with prefixes
files <- list(
  AGE = "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1991/statistics/bst/e9101ea.zip",
  LANG = "https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1991/statistics/bst/l9101ea.zip"
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

  # Find .sav files recursively and rename with prefix
  sav_extracted <- list.files(temp_dir, pattern = "\\.sav$",
                               full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  for (f in sav_extracted) {
    new_name <- file.path(raw_dir, paste0(prefix, "_", basename(f)))
    file.copy(f, new_name)
    message("  Extracted: ", basename(new_name))
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