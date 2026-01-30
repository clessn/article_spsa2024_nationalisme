# ==============================================================================
# APPENDIX C: LATEX TABLES FOR DID AND HAPC ANALYSES
# ==============================================================================
# Produces 7 LaTeX tables:
#   Table 1: DID equation and model specification
#   Table 2: Complete DID effects (by generation-event + language)
#   Table 3: Parallel trends validation
#   Table 4: HAPC variance decomposition
#   Table 5: HAPC likelihood ratio tests
#   Table 6: HAPC cohort random effects
#   Table 7: HAPC period random effects + age fixed effects
# ==============================================================================

library(dplyr)
library(broom)
library(marginaleffects)
library(lme4)
library(xtable)

# Output directory
output_dir <- "SharedFolder_spsa_article_nationalisme/tables/appendix"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# PART 1: DIFFERENCE-IN-DIFFERENCES ANALYSIS
# ==============================================================================

message("\n========== LOADING DATA ==========\n")

Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>%
  mutate(
    yob = year - ses_age,
    generation = case_when(
      yob <= 1946 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob >= 1992 ~ "z"
    ),
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z"))
  )

# Define events
events <- list(
  ref_1980 = list(
    name = "Referendum 1980",
    year = 1980,
    pre_years = c(1974, 1979),
    post_years = c(1984, 1988),
    generations = c("preboomer", "boomer"),
    type = "treatment"
  ),
  meech_1990 = list(
    name = "Meech Lake 1990",
    year = 1990,
    pre_years = c(1984, 1988),
    post_years = c(1993, 1997),
    generations = c("preboomer", "boomer", "x"),
    type = "treatment"
  ),
  ref_1995 = list(
    name = "Referendum 1995",
    year = 1995,
    pre_years = c(1988, 1993),
    post_years = c(1997, 2000, 2004),
    generations = c("preboomer", "boomer", "x"),
    type = "treatment"
  ),
  sponsorship_2005 = list(
    name = "Sponsorship 2005",
    year = 2005,
    pre_years = c(2000, 2004),
    post_years = c(2006, 2008, 2011),
    generations = c("preboomer", "boomer", "x", "y"),
    type = "treatment"
  ),
  placebo_2012 = list(
    name = "Placebo 2012",
    year = 2012,
    pre_years = c(2008, 2011),
    post_years = c(2015, 2019),
    generations = c("preboomer", "boomer", "x", "y", "z"),
    type = "placebo"
  ),
  placebo_2020 = list(
    name = "Placebo 2020",
    year = 2020,
    pre_years = c(2015, 2019),
    post_years = c(2021, 2022, 2023),
    generations = c("preboomer", "boomer", "x", "y", "z"),
    type = "placebo"
  )
)

# ------------------------------------------------------------------------------
# TABLE 1: DID Model Specification
# ------------------------------------------------------------------------------

message("\n========== TABLE 1: DID EQUATION ==========\n")

table1_content <- "
\\begin{table}[htbp]
\\centering
\\caption{Difference-in-Differences Model Specification}
\\label{tab:appendixC_did_equation}
\\begin{tabular}{p{3cm}p{10cm}}
\\hline
\\textbf{Component} & \\textbf{Specification} \\\\
\\hline
\\textbf{Model} & $Y_{igt} = \\alpha + \\beta_1 \\text{Post}_t + \\beta_2 \\text{Generation}_g + \\beta_3 (\\text{Post}_t \\times \\text{Generation}_g) + \\gamma \\text{Language}_i + \\varepsilon_{igt}$ \\\\[0.5em]
\\textbf{Outcome} & $Y_{igt}$: Independence support (0-1 scale) for individual $i$ in generation $g$ at time $t$ \\\\[0.5em]
\\textbf{Treatment} & $\\text{Post}_t$: Binary indicator (1 if survey year $>$ event year) \\\\[0.5em]
\\textbf{Cohort} & $\\text{Generation}_g$: Factor (Pre-boomer, Boomer, X, Y, Z) \\\\[0.5em]
\\textbf{Control} & $\\text{Language}_i$: Mother tongue (French, English, Other) \\\\[0.5em]
\\textbf{Effect of interest} & $\\beta_3$: Differential post-event effect by generation \\\\
\\hline
\\multicolumn{2}{p{13cm}}{\\footnotesize\\textit{Note:} Treatment events: Referendum 1980, Meech Lake 1990, Referendum 1995, Sponsorship Scandal 2005. Placebo events: 2012, 2020. Generation-specific effects estimated via \\texttt{marginaleffects::avg\\_slopes()}.} \\\\
\\hline
\\end{tabular}
\\end{table}
"

writeLines(table1_content, file.path(output_dir, "appendixC_table_1.tex"))
message("Saved: appendixC_table_1.tex")

# ------------------------------------------------------------------------------
# TABLE 2: Complete DID Effects
# ------------------------------------------------------------------------------

message("\n========== TABLE 2: DID EFFECTS ==========\n")

run_did_full <- function(event, data) {
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    mutate(post_event = as.numeric(year > event$year))

  n_pre <- sum(data_event$post_event == 0)
  n_post <- sum(data_event$post_event == 1)

  # Full model with interaction
  model <- lm(iss_souv ~ post_event * generation + ses_lang.1, data = data_event)

  # Generation-specific effects
  gen_effects <- avg_slopes(model, variables = "post_event", by = "generation") %>%
    as.data.frame() %>%
    select(generation, estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(
      event_name = event$name,
      event_year = event$year,
      event_type = event$type,
      n_total = nrow(data_event),
      n_pre = n_pre,
      n_post = n_post,
      effect_type = "Generation"
    )

  # Language effects
  lang_effects <- tidy(model, conf.int = TRUE) %>%
    filter(grepl("ses_lang", term)) %>%
    mutate(
      generation = gsub("ses_lang.1", "", term),
      event_name = event$name,
      event_year = event$year,
      event_type = event$type,
      n_total = nrow(data_event),
      n_pre = n_pre,
      n_post = n_post,
      effect_type = "Language"
    ) %>%
    select(generation, estimate, std.error, p.value, conf.low, conf.high,
           event_name, event_year, event_type, n_total, n_pre, n_post, effect_type)

  return(list(
    model = model,
    gen_effects = gen_effects,
    lang_effects = lang_effects
  ))
}

# Run all events
did_results <- lapply(events, function(e) {
  message(paste("Running DiD for:", e$name))
  run_did_full(e, Data)
})

# Compile generation effects
gen_table <- bind_rows(lapply(did_results, function(r) r$gen_effects)) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, sig),
    se_fmt = sprintf("(%.3f)", std.error)
  )

# Compile language effects
lang_table <- bind_rows(lapply(did_results, function(r) r$lang_effects)) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, sig),
    se_fmt = sprintf("(%.3f)", std.error)
  )

# Create LaTeX table for generation effects
gen_wide <- gen_table %>%
  select(event_name, event_type, generation, estimate_fmt, se_fmt, n_total) %>%
  arrange(event_type, event_name, generation)

table2_header <- "
\\begin{table}[htbp]
\\centering
\\caption{Difference-in-Differences: Post-Event Effects by Generation}
\\label{tab:appendixC_did_effects}
\\small
\\begin{tabular}{llcccc}
\\hline
\\textbf{Event} & \\textbf{Type} & \\textbf{Generation} & \\textbf{Effect} & \\textbf{SE} & \\textbf{N} \\\\
\\hline
"

table2_body <- ""
current_event <- ""
for (i in 1:nrow(gen_wide)) {
  row <- gen_wide[i, ]
  if (row$event_name != current_event) {
    if (current_event != "") {
      table2_body <- paste0(table2_body, "\\hline\n")
    }
    current_event <- row$event_name
    table2_body <- paste0(table2_body,
      sprintf("%s & %s & %s & %s & %s & %s \\\\\n",
              row$event_name, row$event_type, row$generation,
              row$estimate_fmt, row$se_fmt, row$n_total))
  } else {
    table2_body <- paste0(table2_body,
      sprintf(" & & %s & %s & %s & \\\\\n",
              row$generation, row$estimate_fmt, row$se_fmt))
  }
}

table2_footer <- "
\\hline
\\multicolumn{6}{p{12cm}}{\\footnotesize\\textit{Note:} Effects represent the average change in independence support (0-1 scale) after the constitutional event, by generation. Estimated via marginal effects from DiD model with generation interaction. $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\
\\end{tabular}
\\end{table}
"

writeLines(paste0(table2_header, table2_body, table2_footer),
           file.path(output_dir, "appendixC_table_2.tex"))
message("Saved: appendixC_table_2.tex")

# Language effects table (Table 2b)
lang_wide <- lang_table %>%
  filter(!is.na(generation)) %>%
  select(event_name, generation, estimate_fmt, se_fmt) %>%
  distinct()

table2b_header <- "
\\begin{table}[htbp]
\\centering
\\caption{Difference-in-Differences: Language Control Effects}
\\label{tab:appendixC_did_language}
\\small
\\begin{tabular}{lccc}
\\hline
\\textbf{Event} & \\textbf{Language} & \\textbf{Effect} & \\textbf{SE} \\\\
\\hline
"

table2b_body <- ""
current_event <- ""
for (i in 1:nrow(lang_wide)) {
  row <- lang_wide[i, ]
  if (row$event_name != current_event) {
    if (current_event != "") {
      table2b_body <- paste0(table2b_body, "\\hline\n")
    }
    current_event <- row$event_name
    table2b_body <- paste0(table2b_body,
      sprintf("%s & %s & %s & %s \\\\\n",
              row$event_name, row$generation, row$estimate_fmt, row$se_fmt))
  } else {
    table2b_body <- paste0(table2b_body,
      sprintf(" & %s & %s & %s \\\\\n",
              row$generation, row$estimate_fmt, row$se_fmt))
  }
}

table2b_footer <- "
\\hline
\\multicolumn{4}{p{10cm}}{\\footnotesize\\textit{Note:} Language effects relative to French (reference). $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\
\\end{tabular}
\\end{table}
"

writeLines(paste0(table2b_header, table2b_body, table2b_footer),
           file.path(output_dir, "appendixC_table_2b.tex"))
message("Saved: appendixC_table_2b.tex")

# ------------------------------------------------------------------------------
# TABLE 3: Parallel Trends Validation
# ------------------------------------------------------------------------------

message("\n========== TABLE 3: PARALLEL TRENDS ==========\n")

run_event_study <- function(event, data) {
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    mutate(
      event_time = year - event$year,
      event_time_f = factor(event_time)
    )

  available_pre_years <- intersect(event$pre_years, unique(data_event$year))
  if (length(available_pre_years) == 0) return(NULL)

  ref_year <- max(available_pre_years)
  ref_time <- ref_year - event$year
  data_event$event_time_f <- relevel(data_event$event_time_f, ref = as.character(ref_time))

  model <- lm(iss_souv ~ event_time_f + generation + ses_lang.1, data = data_event)

  coefs <- tidy(model, conf.int = TRUE) %>%
    filter(grepl("^event_time_f", term)) %>%
    filter(!grepl(":", term)) %>%
    mutate(
      event_time = as.numeric(gsub("event_time_f", "", term)),
      ref_time = ref_time,
      event_name = event$name
    )

  return(coefs)
}

# Run for treatment events only
treatment_events <- events[sapply(events, function(e) e$type == "treatment")]
pt_results <- lapply(treatment_events, function(e) {
  message(paste("Running event-study for:", e$name))
  run_event_study(e, Data)
})
pt_results <- pt_results[!sapply(pt_results, is.null)]

# Compile pre-event coefficients
parallel_trends <- bind_rows(pt_results) %>%
  filter(event_time < 0) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, sig),
    se_fmt = sprintf("(%.3f)", std.error),
    ci_fmt = sprintf("[%.3f, %.3f]", conf.low, conf.high)
  ) %>%
  select(event_name, event_time, ref_time, estimate_fmt, se_fmt, ci_fmt, p.value)

table3_header <- "
\\begin{table}[htbp]
\\centering
\\caption{Parallel Trends Validation: Pre-Event Coefficients}
\\label{tab:appendixC_parallel_trends}
\\small
\\begin{tabular}{lcccccc}
\\hline
\\textbf{Event} & \\textbf{Time} & \\textbf{Ref.} & \\textbf{Coef.} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\
\\hline
"

table3_body <- ""
for (i in 1:nrow(parallel_trends)) {
  row <- parallel_trends[i, ]
  table3_body <- paste0(table3_body,
    sprintf("%s & %d & %d & %s & %s & %s & %.3f \\\\\n",
            row$event_name, row$event_time, row$ref_time,
            row$estimate_fmt, row$se_fmt, row$ci_fmt, row$p.value))
}

n_sig <- sum(parallel_trends$p.value < 0.05, na.rm = TRUE)
n_total <- nrow(parallel_trends)

table3_footer <- sprintf("
\\hline
\\multicolumn{7}{p{14cm}}{\\footnotesize\\textit{Note:} Pre-event coefficients test parallel trends assumption. Coefficients close to zero with non-significant p-values support the assumption. Time indicates years relative to event; Ref. is the reference period (omitted). Significant pre-event coefficients: %d/%d. $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\
\\end{tabular}
\\end{table}
", n_sig, n_total)

writeLines(paste0(table3_header, table3_body, table3_footer),
           file.path(output_dir, "appendixC_table_3.tex"))
message("Saved: appendixC_table_3.tex")

# ==============================================================================
# PART 2: HAPC-CCREM ANALYSIS
# ==============================================================================

message("\n========== HAPC MODEL ==========\n")

data_hapc <- Data %>%
  mutate(
    age_scaled = scale(ses_age)[,1],
    year_factor = factor(year)
  ) %>%
  filter(!is.na(generation), !is.na(iss_souv), !is.na(ses_age))

# Fit HAPC model
model_hapc <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) +
    (1 | generation) + (1 | year_factor),
  data = data_hapc,
  family = binomial
)

# Comparison models
model_no_cohort <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) + (1 | year_factor),
  data = data_hapc,
  family = binomial
)

model_no_period <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) + (1 | generation),
  data = data_hapc,
  family = binomial
)

# ------------------------------------------------------------------------------
# TABLE 4: Variance Decomposition
# ------------------------------------------------------------------------------

message("\n========== TABLE 4: VARIANCE DECOMPOSITION ==========\n")

var_components <- as.data.frame(VarCorr(model_hapc))
var_cohort <- var_components$vcov[var_components$grp == "generation"]
var_period <- var_components$vcov[var_components$grp == "year_factor"]
var_residual <- pi^2 / 3
var_total <- var_cohort + var_period + var_residual

table4_content <- sprintf("
\\begin{table}[htbp]
\\centering
\\caption{HAPC-CCREM: Variance Decomposition}
\\label{tab:appendixC_variance}
\\begin{tabular}{lcc}
\\hline
\\textbf{Component} & \\textbf{Variance} & \\textbf{Proportion} \\\\
\\hline
Cohort (Generation) & %.4f & %.1f\\%% \\\\
Period (Survey Year) & %.4f & %.1f\\%% \\\\
Residual ($\\pi^2/3$) & %.4f & %.1f\\%% \\\\
\\hline
\\textbf{Total} & %.4f & 100.0\\%% \\\\
\\hline
\\multicolumn{3}{p{8cm}}{\\footnotesize\\textit{Note:} Variance decomposition from HAPC-CCREM model (Yang \\& Land, 2008). Residual variance for logistic regression is $\\pi^2/3 \\approx 3.29$. N = %d observations.} \\\\
\\end{tabular}
\\end{table}
", var_cohort, var_cohort/var_total*100,
   var_period, var_period/var_total*100,
   var_residual, var_residual/var_total*100,
   var_total, nrow(model_hapc@frame))

writeLines(table4_content, file.path(output_dir, "appendixC_table_4.tex"))
message("Saved: appendixC_table_4.tex")

# ------------------------------------------------------------------------------
# TABLE 5: Likelihood Ratio Tests
# ------------------------------------------------------------------------------

message("\n========== TABLE 5: LIKELIHOOD RATIO TESTS ==========\n")

lrt_cohort <- anova(model_no_cohort, model_hapc)
lrt_period <- anova(model_no_period, model_hapc)

table5_content <- sprintf("
\\begin{table}[htbp]
\\centering
\\caption{HAPC-CCREM: Likelihood Ratio Tests for Random Effects}
\\label{tab:appendixC_lrt}
\\begin{tabular}{lccccc}
\\hline
\\textbf{Test} & \\textbf{df} & \\textbf{AIC (reduced)} & \\textbf{AIC (full)} & \\textbf{$\\chi^2$} & \\textbf{p-value} \\\\
\\hline
Cohort effect & %d & %.1f & %.1f & %.2f & %.4f \\\\
Period effect & %d & %.1f & %.1f & %.2f & %.4f \\\\
\\hline
\\multicolumn{6}{p{12cm}}{\\footnotesize\\textit{Note:} Likelihood ratio tests comparing full HAPC model to nested models without each random effect. Significant p-values indicate the random effect contributes meaningfully to model fit.} \\\\
\\end{tabular}
\\end{table}
",
  lrt_cohort$Df[2] - lrt_cohort$Df[1],
  lrt_cohort$AIC[1], lrt_cohort$AIC[2],
  lrt_cohort$Chisq[2], lrt_cohort$`Pr(>Chisq)`[2],
  lrt_period$Df[2] - lrt_period$Df[1],
  lrt_period$AIC[1], lrt_period$AIC[2],
  lrt_period$Chisq[2], lrt_period$`Pr(>Chisq)`[2])

writeLines(table5_content, file.path(output_dir, "appendixC_table_5.tex"))
message("Saved: appendixC_table_5.tex")

# ------------------------------------------------------------------------------
# TABLE 6: Cohort Random Effects
# ------------------------------------------------------------------------------

message("\n========== TABLE 6: COHORT RANDOM EFFECTS ==========\n")

re_cohort <- ranef(model_hapc)$generation
cohort_df <- data.frame(
  generation = rownames(re_cohort),
  effect = re_cohort$`(Intercept)`
) %>%
  mutate(
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z")),
    generation_label = case_when(
      generation == "preboomer" ~ "Pre-boomer (1925-1946)",
      generation == "boomer" ~ "Boomer (1947-1961)",
      generation == "x" ~ "Gen X (1962-1976)",
      generation == "y" ~ "Gen Y (1977-1991)",
      generation == "z" ~ "Gen Z (1992-2003)"
    ),
    prob_effect = sprintf("%.1f\\%%", (plogis(effect) - 0.5) * 100 * 2)  # Approx effect on prob
  ) %>%
  arrange(generation)

table6_header <- "
\\begin{table}[htbp]
\\centering
\\caption{HAPC-CCREM: Cohort (Generation) Random Effects}
\\label{tab:appendixC_cohort_re}
\\begin{tabular}{lcc}
\\hline
\\textbf{Generation} & \\textbf{Random Effect} & \\textbf{Direction} \\\\
\\hline
"

table6_body <- ""
for (i in 1:nrow(cohort_df)) {
  row <- cohort_df[i, ]
  direction <- ifelse(row$effect > 0, "Above average", "Below average")
  table6_body <- paste0(table6_body,
    sprintf("%s & %.4f & %s \\\\\n", row$generation_label, row$effect, direction))
}

table6_footer <- "
\\hline
\\multicolumn{3}{p{10cm}}{\\footnotesize\\textit{Note:} Random effects represent deviations from the overall mean on the log-odds scale, controlling for age (quadratic) and period effects. Positive values indicate higher-than-average independence support.} \\\\
\\end{tabular}
\\end{table}
"

writeLines(paste0(table6_header, table6_body, table6_footer),
           file.path(output_dir, "appendixC_table_6.tex"))
message("Saved: appendixC_table_6.tex")

# ------------------------------------------------------------------------------
# TABLE 7: Period Random Effects + Age Fixed Effects
# ------------------------------------------------------------------------------

message("\n========== TABLE 7: PERIOD + AGE EFFECTS ==========\n")

# Period effects
re_period <- ranef(model_hapc)$year_factor
period_df <- data.frame(
  year = as.numeric(as.character(rownames(re_period))),
  effect = re_period$`(Intercept)`
) %>%
  arrange(year)

# Age fixed effects
fixed_effects <- fixef(model_hapc)
age_mean <- mean(data_hapc$ses_age, na.rm = TRUE)
age_sd <- sd(data_hapc$ses_age, na.rm = TRUE)

# Create combined table
table7_content <- "
\\begin{table}[htbp]
\\centering
\\caption{HAPC-CCREM: Period Random Effects and Age Fixed Effects}
\\label{tab:appendixC_period_age}
\\begin{minipage}[t]{0.48\\textwidth}
\\centering
\\textbf{(a) Period (Year) Random Effects}\\\\[0.5em]
\\begin{tabular}{cc}
\\hline
\\textbf{Year} & \\textbf{Effect} \\\\
\\hline
"

for (i in 1:nrow(period_df)) {
  row <- period_df[i, ]
  table7_content <- paste0(table7_content,
    sprintf("%d & %.4f \\\\\n", row$year, row$effect))
}

table7_content <- paste0(table7_content, "
\\hline
\\end{tabular}
\\end{minipage}
\\hfill
\\begin{minipage}[t]{0.48\\textwidth}
\\centering
\\textbf{(b) Age Fixed Effects}\\\\[0.5em]
\\begin{tabular}{lc}
\\hline
\\textbf{Parameter} & \\textbf{Estimate} \\\\
\\hline
")

table7_content <- paste0(table7_content, sprintf("
Intercept & %.4f \\\\
Age (linear) & %.4f \\\\
Age (quadratic) & %.4f \\\\
\\hline
\\multicolumn{2}{l}{Age mean: %.1f years} \\\\
\\multicolumn{2}{l}{Age SD: %.1f years} \\\\
\\hline
\\end{tabular}
\\end{minipage}
\\\\[1em]
\\footnotesize\\textit{Note:} Period random effects are deviations from the overall mean (log-odds scale). Age effects are on standardized age (z-score). Positive period effects indicate years with higher-than-average support. The quadratic age term captures non-linear age patterns.
\\end{table}
", fixed_effects["(Intercept)"], fixed_effects["age_scaled"],
   fixed_effects["I(age_scaled^2)"], age_mean, age_sd))

writeLines(table7_content, file.path(output_dir, "appendixC_table_7.tex"))
message("Saved: appendixC_table_7.tex")

# ==============================================================================
# SUMMARY
# ==============================================================================

message("\n========== SUMMARY ==========\n")
message("Generated 7 LaTeX tables in: ", output_dir)
message("  - appendixC_table_1.tex: DID equation and model specification")
message("  - appendixC_table_2.tex: DID effects by generation")
message("  - appendixC_table_2b.tex: DID language control effects")
message("  - appendixC_table_3.tex: Parallel trends validation")
message("  - appendixC_table_4.tex: HAPC variance decomposition")
message("  - appendixC_table_5.tex: HAPC likelihood ratio tests")
message("  - appendixC_table_6.tex: HAPC cohort random effects")
message("  - appendixC_table_7.tex: HAPC period + age effects")
