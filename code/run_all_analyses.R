# run_all_analyses.R
# Orchestrator script to run all analysis scripts in order
# Usage: source("code/run_all_analyses.R") from project root

# Setup ----
message("=== Starting analysis pipeline ===\n")
start_time <- Sys.time()

# Ensure we're in the project root
if (!file.exists("code/run_all_analyses.R")) {
  stop("Please run this script from the project root directory")
}

# Helper function to run scripts with timing and error handling
run_script <- function(script_path, description) {
  message(sprintf("\n--- %s ---", description))
  message(sprintf("Running: %s", script_path))

  script_start <- Sys.time()

  tryCatch({
    source(script_path, local = new.env())
    elapsed <- round(difftime(Sys.time(), script_start, units = "secs"), 1)
    message(sprintf("✓ Completed in %s seconds\n", elapsed))
    return(TRUE)
  }, error = function(e) {
    message(sprintf("✗ ERROR: %s\n", e$message))
    return(FALSE)
  })
}

# Track results
results <- list()

# 1. Data Preparation ----
results$census <- run_script(
  "code/pot_growth_of_independence/generate_clean_census_data.R",
  "Data Preparation: Census data processing"
)

# 2. Historical Analysis ----
results$figure1 <- run_script(
  "code/figure1_evolution_past_article.R",
  "Figure 1: Historical evolution by generation"
)

# 3. Current Attitudes Analysis (2021-2023) ----
results$figure2 <- run_script(
  "code/figure2_event_study_did_graph.R",
  "Figure 2: Event study / DID graph"
)

results$figure3 <- run_script(
  "code/figure3_current_attitudes.R",
  "Figure 3: Current attitudes"
)

results$figure4 <- run_script(
  "code/figure4_attitude_force.R",
  "Figure 4: Attitude strength"
)

results$figure5 <- run_script(
  "code/figure5_id_qc_can.R",
  "Figure 5: Quebec/Canada identity"
)

results$figure6 <- run_script(
  "code/figure6_attitude_force_idcan.R",
  "Figure 6: Attitude strength by identity"
)

# 4. Predictive Modeling ----
results$model <- run_script(
  "code/pot_growth_of_independence/model_and_predict.R",
  "Predictive Model: Generation × Region × Language"
)

# 5. Geographic Analysis ----
results$map <- run_script(
  "code/pot_growth_of_independence/map.R",
  "Maps: Choropleth by riding"
)

results$zoomers <- run_script(
  "code/pot_growth_of_independence/zoom_on_zoomers.R",
  "Zoom on Zoomers: Gen Z by riding"
)

results$zoomers_attitudes <- run_script(
  "code/pot_growth_of_independence/zoom_on_zoomers_attitudes.R",
  "Zoom on Zoomers: Attitudes analysis"
)

# Summary ----
total_time <- round(difftime(Sys.time(), start_time, units = "mins"), 1)

message("\n=== Pipeline Summary ===")
message(sprintf("Total runtime: %s minutes", total_time))

successful <- sum(unlist(results))
total <- length(results)

message(sprintf("Scripts completed: %d/%d", successful, total))

if (successful < total) {
  failed <- names(results)[!unlist(results)]
  message(sprintf("Failed scripts: %s", paste(failed, collapse = ", ")))
}

message("\n=== Pipeline finished ===")
