## Combine all census marginals into marginals_all.rds
## Reads marginals_YYYY.rds files and combines them

library(dplyr)
library(tidyr)

marginals_dir <- "SharedFolder_spsa_article_nationalisme/data/census/marginals"

# =============================================================================
# LOAD ALL MARGINALS FILES
# =============================================================================

message("Loading marginals files...")

files <- list.files(marginals_dir, pattern = "^marginals_\\d{4}\\.rds$", full.names = TRUE)
message("Found ", length(files), " files:")
message(paste(" -", basename(files), collapse = "\n"))

marginals_list <- lapply(files, function(f) {
  year <- as.integer(gsub(".*marginals_(\\d{4})\\.rds", "\\1", f))
  data <- readRDS(f)
  message("  ", year, ": ", nrow(data), " rows")
  data
})

# =============================================================================
# COMBINE
# =============================================================================

message("\nCombining...")
marginals_all <- bind_rows(marginals_list)

# =============================================================================
# SUMMARY
# =============================================================================

message("\n=== Summary by year ===")
marginals_all %>%
  group_by(census_year) %>%
  summarise(
    n_rows = n(),
    variables = paste(unique(variable), collapse = ", "),
    genders = paste(unique(gender), collapse = ", "),
    .groups = "drop"
  ) %>%
  print(n = 20)

message("\n=== Variables availability ===")
marginals_all %>%
  group_by(census_year, variable) %>%
  summarise(genders = paste(sort(unique(gender)), collapse = "/"), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = genders) %>%
  print(n = 20)

message("\n=== Total rows: ", nrow(marginals_all), " ===")

# =============================================================================
# SAVE
# =============================================================================

saveRDS(marginals_all, file.path(marginals_dir, "marginals_all.rds"))
message("\nSaved to: ", file.path(marginals_dir, "marginals_all.rds"))
