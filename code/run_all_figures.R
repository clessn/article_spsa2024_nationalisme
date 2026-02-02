# run_all_figures.R
# Orchestrator script to run all figure scripts in order

# Get all figure scripts
figure_scripts <- list.files(
  path = "code",
  pattern = "^figure.*\\.R$",
  full.names = TRUE
) |> sort()

# Add appendix scripts
appendix_scripts <- list.files(
  path = "code",
  pattern = "^appendix.*\\.R$",
  full.names = TRUE
) |> sort()

figure_scripts <- c(figure_scripts, appendix_scripts)

cat("=== Running all figure scripts ===\n\n")
cat("Scripts to run:\n")
cat(paste(" -", figure_scripts, collapse = "\n"), "\n\n")

# Track results
results <- data.frame(
 script = character(),
 status = character(),
 time = numeric(),
 stringsAsFactors = FALSE
)

for (script in figure_scripts) {
  cat(sprintf("\n[%s] Running %s...\n", Sys.time(), basename(script)))
  cat(paste(rep("-", 60), collapse = ""), "\n")

  start_time <- Sys.time()

  tryCatch({
    source(script, local = new.env())
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\n OK - completed in %.1f seconds\n", elapsed))
    results <- rbind(results, data.frame(
      script = basename(script),
      status = "OK",
      time = elapsed
    ))
  }, error = function(e) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\n FAILED after %.1f seconds\n", elapsed))
    cat("Error:", conditionMessage(e), "\n")
    results <<- rbind(results, data.frame(
      script = basename(script),
      status = paste("FAILED:", conditionMessage(e)),
      time = elapsed
    ))
  })
}

# Summary
cat("\n\n=== SUMMARY ===\n")
print(results, row.names = FALSE)

n_ok <- sum(results$status == "OK")
n_total <- nrow(results)
cat(sprintf("\n%d/%d scripts completed successfully\n", n_ok, n_total))
cat(sprintf("Total time: %.1f seconds\n", sum(results$time)))
