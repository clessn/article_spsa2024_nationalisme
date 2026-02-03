# ==============================================================================
# APPENDIX D: DISENTANGLING COHORT AND PERIOD EFFECTS
# ==============================================================================
# Produces a single consolidated LaTeX file: appendixD.tex
# ==============================================================================

library(dplyr)
library(broom)
library(marginaleffects)
library(lme4)
library(survey)  # for svyglm with proper weighting and clustering

# Output directory
output_dir <- "SharedFolder_spsa_article_nationalisme/tables/appendix"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# PART 1: DIFFERENCE-IN-DIFFERENCES ANALYSIS
# ==============================================================================

message("\n========== LOADING DATA ==========\n")

Data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds") %>%
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
  pq_1976 = list(
    name = "PQ Election 1976",
    year = 1976,
    pre_years = c(1968, 1974),
    post_years = c(1979),  # Only 1979 to avoid contamination from 1980 referendum
    generations = c("preboomer"),  # Only preboomers have sufficient N in early surveys
    type = "treatment"
  ),
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
# DID Analysis Functions
# ------------------------------------------------------------------------------

run_did_full <- function(event, data) {
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    filter(!is.na(weight_trimmed)) %>%
    mutate(post_event = as.numeric(year > event$year))

  n_pre <- sum(data_event$post_event == 0)
  n_post <- sum(data_event$post_event == 1)

  # Create survey design with clustering by year
  des <- svydesign(
    ids = ~year,
    weights = ~weight_trimmed,
    data = data_event
  )

  # Check if we have multiple generations (need contrast for interaction)
  n_generations <- length(unique(data_event$generation))

  if (n_generations > 1) {
    # Multiple generations: use interaction model
    model <- svyglm(iss_souv ~ post_event * generation + ses_lang.1, design = des)

    gen_effects <- avg_slopes(model, variables = "post_event", by = "generation") %>%
      as.data.frame() %>%
      select(generation, estimate, std.error, p.value, conf.low, conf.high)
  } else {
    # Single generation: use simpler model without interaction
    model <- svyglm(iss_souv ~ post_event + ses_lang.1, design = des)

    # Extract post_event effect manually
    model_tidy <- tidy(model) %>%
      filter(term == "post_event")

    # Calculate CI and p-value manually using normal approximation (svyglm uses z-test)
    z_stat <- model_tidy$estimate / model_tidy$std.error
    gen_effects <- data.frame(
      generation = unique(data_event$generation),
      estimate = model_tidy$estimate,
      std.error = model_tidy$std.error,
      p.value = 2 * pnorm(-abs(z_stat)),
      conf.low = model_tidy$estimate - 1.96 * model_tidy$std.error,
      conf.high = model_tidy$estimate + 1.96 * model_tidy$std.error
    )
  }

  gen_effects <- gen_effects %>%
    mutate(
      event_name = event$name,
      event_year = event$year,
      event_type = event$type,
      n_total = nrow(data_event),
      n_pre = n_pre,
      n_post = n_post,
      effect_type = "Generation"
    )

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

run_event_study <- function(event, data) {
  all_years <- c(event$pre_years, event$post_years)

  data_event <- data %>%
    filter(year %in% all_years) %>%
    filter(generation %in% event$generations) %>%
    filter(!is.na(iss_souv)) %>%
    filter(!is.na(ses_lang.1)) %>%
    filter(!is.na(weight_trimmed)) %>%
    mutate(
      event_time = year - event$year,
      event_time_f = factor(event_time)
    )

  available_pre_years <- intersect(event$pre_years, unique(data_event$year))
  if (length(available_pre_years) == 0) return(NULL)

  ref_year <- max(available_pre_years)
  ref_time <- ref_year - event$year
  data_event$event_time_f <- relevel(data_event$event_time_f, ref = as.character(ref_time))

  # Create survey design with clustering by year
  des <- svydesign(
    ids = ~year,
    weights = ~weight_trimmed,
    data = data_event
  )

  # Check if we have multiple generations
  n_generations <- length(unique(data_event$generation))

  if (n_generations > 1) {
    model <- svyglm(iss_souv ~ event_time_f + generation + ses_lang.1, design = des)
  } else {
    model <- svyglm(iss_souv ~ event_time_f + ses_lang.1, design = des)
  }

  coefs <- tidy(model) %>%
    filter(grepl("^event_time_f", term)) %>%
    filter(!grepl(":", term)) %>%
    mutate(
      event_time = as.numeric(gsub("event_time_f", "", term)),
      ref_time = ref_time,
      event_name = event$name,
      # Calculate CI and p-value manually
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      z_stat = estimate / std.error,
      p.value = 2 * pnorm(-abs(z_stat))
    )

  return(coefs)
}

# ------------------------------------------------------------------------------
# Run DID Analysis
# ------------------------------------------------------------------------------

message("\n========== RUNNING DID ANALYSIS ==========\n")

did_results <- lapply(events, function(e) {
  message(paste("Running DiD for:", e$name))
  run_did_full(e, Data)
})

gen_table <- bind_rows(lapply(did_results, function(r) r$gen_effects)) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

lang_table <- bind_rows(lapply(did_results, function(r) r$lang_effects)) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Parallel trends
treatment_events <- events[sapply(events, function(e) e$type == "treatment")]
pt_results <- lapply(treatment_events, function(e) {
  message(paste("Running event-study for:", e$name))
  run_event_study(e, Data)
})
pt_results <- pt_results[!sapply(pt_results, is.null)]

parallel_trends <- bind_rows(pt_results) %>%
  filter(event_time < 0) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Save results for Figure 2 graph
dir.create("SharedFolder_spsa_article_nationalisme/tables/event_study", showWarnings = FALSE, recursive = TRUE)
write.csv(gen_table, "SharedFolder_spsa_article_nationalisme/tables/event_study/did_results.csv", row.names = FALSE)

# ==============================================================================
# PART 2: HAPC-CCREM ANALYSIS
# ==============================================================================

message("\n========== RUNNING HAPC MODEL ==========\n")

data_hapc <- Data %>%
  mutate(
    age_scaled = scale(ses_age)[,1],
    year_factor = factor(year)
  ) %>%
  filter(!is.na(generation), !is.na(iss_souv), !is.na(ses_age))

model_hapc <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) +
    (1 | generation) + (1 | year_factor),
  data = data_hapc,
  weights = weight_trimmed,
  family = binomial
)

model_no_cohort <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) + (1 | year_factor),
  data = data_hapc,
  weights = weight_trimmed,
  family = binomial
)

model_no_period <- glmer(
  iss_souv ~ age_scaled + I(age_scaled^2) + (1 | generation),
  data = data_hapc,
  weights = weight_trimmed,
  family = binomial
)

# Extract components
var_components <- as.data.frame(VarCorr(model_hapc))
var_cohort <- var_components$vcov[var_components$grp == "generation"]
var_period <- var_components$vcov[var_components$grp == "year_factor"]
var_residual <- pi^2 / 3
var_total <- var_cohort + var_period + var_residual

lrt_cohort <- anova(model_no_cohort, model_hapc)
lrt_period <- anova(model_no_period, model_hapc)

re_cohort <- ranef(model_hapc)$generation
cohort_df <- data.frame(
  generation = rownames(re_cohort),
  effect = re_cohort$`(Intercept)`
) %>%
  mutate(
    generation = factor(generation, levels = c("preboomer", "boomer", "x", "y", "z")),
    generation_label = case_when(
      generation == "preboomer" ~ "Pre-boomer (1925--1946)",
      generation == "boomer" ~ "Boomer (1947--1961)",
      generation == "x" ~ "Gen X (1962--1976)",
      generation == "y" ~ "Gen Y (1977--1991)",
      generation == "z" ~ "Gen Z (1992--2003)"
    )
  ) %>%
  arrange(generation)

re_period <- ranef(model_hapc)$year_factor
period_df <- data.frame(
  year = as.numeric(as.character(rownames(re_period))),
  effect = re_period$`(Intercept)`
) %>%
  arrange(year)

fixed_effects <- fixef(model_hapc)
age_mean <- mean(data_hapc$ses_age, na.rm = TRUE)
age_sd <- sd(data_hapc$ses_age, na.rm = TRUE)

# ==============================================================================
# PART 3: GENERATE CONSOLIDATED LATEX FILE
# ==============================================================================

message("\n========== GENERATING LATEX FILE ==========\n")

# Start building LaTeX content
latex <- character()

# Header
latex <- c(latex, "% ==============================================================================")
latex <- c(latex, "% APPENDIX D: DISENTANGLING COHORT AND PERIOD EFFECTS")
latex <- c(latex, "% Generated by figure2_appendix.R")
latex <- c(latex, "% ==============================================================================")
latex <- c(latex, "")
latex <- c(latex, "% ------------------------------------------------------------------------------")
latex <- c(latex, "% D.1 Difference-in-Differences Event Study")
latex <- c(latex, "% ------------------------------------------------------------------------------")
latex <- c(latex, "")

# Table 1: DID Effects
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{Difference-in-Differences: Post-Event Effects by Generation}")
latex <- c(latex, "\\label{tab:appendixD_did_effects}")
latex <- c(latex, "\\small")
latex <- c(latex, "\\begin{tabular}{llccccc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Event} & \\textbf{Type} & \\textbf{Generation} & \\textbf{Effect} & \\textbf{SE} & \\textbf{p} & \\textbf{N} \\\\")
latex <- c(latex, "\\hline")

# Order events properly
event_order <- c("PQ Election 1976", "Referendum 1980", "Meech Lake 1990", "Referendum 1995", "Sponsorship 2005", "Placebo 2012", "Placebo 2020")
gen_table <- gen_table %>%
  mutate(event_name = factor(event_name, levels = event_order)) %>%
  arrange(event_name, generation)

current_event <- ""
for (i in 1:nrow(gen_table)) {
  row <- gen_table[i, ]
  est_fmt <- ifelse(row$estimate < 0,
                    sprintf("$-$%.3f%s", abs(row$estimate), row$sig),
                    sprintf("%.3f%s", row$estimate, row$sig))
  se_fmt <- sprintf("(%.3f)", row$std.error)
  p_fmt <- ifelse(row$p.value < 0.001, "$<$0.001", sprintf("%.3f", row$p.value))

  if (as.character(row$event_name) != current_event) {
    if (current_event != "") {
      latex <- c(latex, "\\hline")
    }
    current_event <- as.character(row$event_name)
    n_fmt <- format(row$n_total, big.mark = ",")
    latex <- c(latex, sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
                              row$event_name, row$event_type, row$generation,
                              est_fmt, se_fmt, p_fmt, n_fmt))
  } else {
    latex <- c(latex, sprintf(" & & %s & %s & %s & %s & \\\\",
                              row$generation, est_fmt, se_fmt, p_fmt))
  }
}

latex <- c(latex, "\\hline")
latex <- c(latex, "\\multicolumn{7}{p{13cm}}{\\footnotesize\\textit{Note:} Effects represent the average change in independence support (0-1 scale) after the event, by generation. Estimated via marginal effects from survey-weighted DiD model with generation interaction and standard errors clustered by survey year. $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")

# Table 3: Language Effects
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{Difference-in-Differences: Language Control Effects}")
latex <- c(latex, "\\label{tab:appendixD_did_language}")
latex <- c(latex, "\\small")
latex <- c(latex, "\\begin{tabular}{lccc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Event} & \\textbf{Language} & \\textbf{Effect} & \\textbf{SE} \\\\")
latex <- c(latex, "\\hline")

lang_table <- lang_table %>%
  filter(!is.na(generation)) %>%
  mutate(event_name = factor(event_name, levels = event_order)) %>%
  arrange(event_name, generation)

current_event <- ""
for (i in 1:nrow(lang_table)) {
  row <- lang_table[i, ]
  est_fmt <- ifelse(row$estimate < 0,
                    sprintf("$-$%.3f%s", abs(row$estimate), row$sig),
                    sprintf("%.3f%s", row$estimate, row$sig))
  se_fmt <- sprintf("(%.3f)", row$std.error)

  if (as.character(row$event_name) != current_event) {
    if (current_event != "") {
      latex <- c(latex, "\\hline")
    }
    current_event <- as.character(row$event_name)
    latex <- c(latex, sprintf("%s & %s & %s & %s \\\\",
                              row$event_name, row$generation, est_fmt, se_fmt))
  } else {
    latex <- c(latex, sprintf(" & %s & %s & %s \\\\",
                              row$generation, est_fmt, se_fmt))
  }
}

latex <- c(latex, "\\hline")
latex <- c(latex, "\\multicolumn{4}{p{10cm}}{\\footnotesize\\textit{Note:} Language effects relative to English (reference category). $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")

# Table 4: Parallel Trends
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{Parallel Trends Validation: Pre-Event Coefficients}")
latex <- c(latex, "\\label{tab:appendixD_parallel_trends}")
latex <- c(latex, "\\small")
latex <- c(latex, "\\begin{tabular}{lcccccc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Event} & \\textbf{Time} & \\textbf{Ref.} & \\textbf{Coef.} & \\textbf{SE} & \\textbf{95\\% CI} & \\textbf{p-value} \\\\")
latex <- c(latex, "\\hline")

for (i in 1:nrow(parallel_trends)) {
  row <- parallel_trends[i, ]
  est_fmt <- ifelse(row$estimate < 0,
                    sprintf("$-$%.3f%s", abs(row$estimate), row$sig),
                    sprintf("%.3f%s", row$estimate, row$sig))
  se_fmt <- sprintf("(%.3f)", row$std.error)

  # Handle NA/NaN in CI and p-value
  if (is.na(row$conf.low) || is.na(row$conf.high)) {
    ci_fmt <- "---"
  } else {
    ci_lo <- ifelse(row$conf.low < 0, sprintf("$-$%.3f", abs(row$conf.low)), sprintf("%.3f", row$conf.low))
    ci_hi <- ifelse(row$conf.high < 0, sprintf("$-$%.3f", abs(row$conf.high)), sprintf("%.3f", row$conf.high))
    ci_fmt <- sprintf("[%s, %s]", ci_lo, ci_hi)
  }

  p_fmt <- ifelse(is.na(row$p.value) || is.nan(row$p.value), "---", sprintf("%.3f", row$p.value))

  latex <- c(latex, sprintf("%s & %d & %d & %s & %s & %s & %s \\\\",
                            row$event_name, row$event_time, row$ref_time,
                            est_fmt, se_fmt, ci_fmt, p_fmt))
}

n_sig <- sum(parallel_trends$p.value < 0.05, na.rm = TRUE)
n_total_pt <- nrow(parallel_trends)

latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("\\multicolumn{7}{p{14cm}}{\\footnotesize\\textit{Note:} Pre-event coefficients test parallel trends assumption. Coefficients close to zero with non-significant p-values support the assumption. Time indicates years relative to event; Ref. is the reference period (omitted). Significant pre-event coefficients: %d/%d. $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$.} \\\\", n_sig, n_total_pt))
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")
latex <- c(latex, "\\clearpage")
latex <- c(latex, "")
latex <- c(latex, "% ------------------------------------------------------------------------------")
latex <- c(latex, "% D.2 Hierarchical Age-Period-Cohort Model (HAPC-CCREM)")
latex <- c(latex, "% ------------------------------------------------------------------------------")
latex <- c(latex, "")

# Table 5: Variance Decomposition
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{HAPC-CCREM: Variance Decomposition}")
latex <- c(latex, "\\label{tab:appendixD_variance}")
latex <- c(latex, "\\begin{tabular}{lcc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Component} & \\textbf{Variance} & \\textbf{Proportion} \\\\")
latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("Cohort (Generation) & %.4f & %.1f\\%% \\\\", var_cohort, var_cohort/var_total*100))
latex <- c(latex, sprintf("Period (Survey Year) & %.4f & %.1f\\%% \\\\", var_period, var_period/var_total*100))
latex <- c(latex, sprintf("Residual ($\\pi^2/3$) & %.4f & %.1f\\%% \\\\", var_residual, var_residual/var_total*100))
latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("\\textbf{Total} & %.4f & 100.0\\%% \\\\", var_total))
latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("\\multicolumn{3}{p{10cm}}{\\footnotesize\\textit{Note:} Variance decomposition from HAPC-CCREM model. Residual variance for logistic regression is $\\pi^2/3 \\approx 3.29$. N = %s observations.} \\\\", format(nrow(model_hapc@frame), big.mark = ",")))
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")

# Table 6: LRT
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{HAPC-CCREM: Likelihood Ratio Tests for Random Effects}")
latex <- c(latex, "\\label{tab:appendixD_lrt}")
latex <- c(latex, "\\begin{tabular}{lcccc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Test} & \\textbf{AIC (reduced)} & \\textbf{AIC (full)} & \\textbf{$\\chi^2$} & \\textbf{p-value} \\\\")
latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("Cohort effect & %s & %s & %.2f & $<$0.0001 \\\\",
                          format(round(lrt_cohort$AIC[1], 1), big.mark = ","),
                          format(round(lrt_cohort$AIC[2], 1), big.mark = ","),
                          lrt_cohort$Chisq[2]))
latex <- c(latex, sprintf("Period effect & %s & %s & %.2f & $<$0.0001 \\\\",
                          format(round(lrt_period$AIC[1], 1), big.mark = ","),
                          format(round(lrt_period$AIC[2], 1), big.mark = ","),
                          lrt_period$Chisq[2]))
latex <- c(latex, "\\hline")
latex <- c(latex, "\\multicolumn{5}{p{12cm}}{\\footnotesize\\textit{Note:} Likelihood ratio tests comparing full HAPC model to nested models without each random effect. Significant p-values indicate the random effect contributes meaningfully to model fit.} \\\\")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")

# Table 7: Cohort Random Effects
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{HAPC-CCREM: Cohort (Generation) Random Effects}")
latex <- c(latex, "\\label{tab:appendixD_cohort_re}")
latex <- c(latex, "\\begin{tabular}{lcc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Generation} & \\textbf{Random Effect} & \\textbf{Direction} \\\\")
latex <- c(latex, "\\hline")

for (i in 1:nrow(cohort_df)) {
  row <- cohort_df[i, ]
  direction <- ifelse(row$effect > 0, "Above average", "Below average")
  eff_fmt <- ifelse(row$effect < 0,
                    sprintf("$-$%.4f", abs(row$effect)),
                    sprintf("%.4f", row$effect))
  latex <- c(latex, sprintf("%s & %s & %s \\\\", row$generation_label, eff_fmt, direction))
}

latex <- c(latex, "\\hline")
latex <- c(latex, "\\multicolumn{3}{p{10cm}}{\\footnotesize\\textit{Note:} Random effects represent deviations from the overall mean on the log-odds scale, controlling for age (quadratic) and period effects. Positive values indicate higher-than-average independence support.} \\\\")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{table}")
latex <- c(latex, "")

# Table 8: Period + Age
latex <- c(latex, "\\begin{table}[htbp]")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\caption{HAPC-CCREM: Period Random Effects and Age Fixed Effects}")
latex <- c(latex, "\\label{tab:appendixD_period_age}")
latex <- c(latex, "\\begin{minipage}[t]{0.48\\textwidth}")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\textbf{(a) Period (Year) Random Effects}\\\\[0.5em]")
latex <- c(latex, "\\begin{tabular}{cc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Year} & \\textbf{Effect} \\\\")
latex <- c(latex, "\\hline")

for (i in 1:nrow(period_df)) {
  row <- period_df[i, ]
  eff_fmt <- ifelse(row$effect < 0,
                    sprintf("$-$%.4f", abs(row$effect)),
                    sprintf("%.4f", row$effect))
  latex <- c(latex, sprintf("%d & %s \\\\", row$year, eff_fmt))
}

latex <- c(latex, "\\hline")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\end{minipage}")
latex <- c(latex, "\\hfill")
latex <- c(latex, "\\begin{minipage}[t]{0.48\\textwidth}")
latex <- c(latex, "\\centering")
latex <- c(latex, "\\textbf{(b) Age Fixed Effects}\\\\[0.5em]")
latex <- c(latex, "\\begin{tabular}{lc}")
latex <- c(latex, "\\hline")
latex <- c(latex, "\\textbf{Parameter} & \\textbf{Estimate} \\\\")
latex <- c(latex, "\\hline")

int_fmt <- ifelse(fixed_effects["(Intercept)"] < 0,
                  sprintf("$-$%.4f", abs(fixed_effects["(Intercept)"])),
                  sprintf("%.4f", fixed_effects["(Intercept)"]))
age_fmt <- ifelse(fixed_effects["age_scaled"] < 0,
                  sprintf("$-$%.4f", abs(fixed_effects["age_scaled"])),
                  sprintf("%.4f", fixed_effects["age_scaled"]))
age2_fmt <- ifelse(fixed_effects["I(age_scaled^2)"] < 0,
                   sprintf("$-$%.4f", abs(fixed_effects["I(age_scaled^2)"])),
                   sprintf("%.4f", fixed_effects["I(age_scaled^2)"]))

latex <- c(latex, sprintf("Intercept & %s \\\\", int_fmt))
latex <- c(latex, sprintf("Age (linear) & %s \\\\", age_fmt))
latex <- c(latex, sprintf("Age (quadratic) & %s \\\\", age2_fmt))
latex <- c(latex, "\\hline")
latex <- c(latex, sprintf("\\multicolumn{2}{l}{Age mean: %.1f years} \\\\", age_mean))
latex <- c(latex, sprintf("\\multicolumn{2}{l}{Age SD: %.1f years} \\\\", age_sd))
latex <- c(latex, "\\hline")
latex <- c(latex, "\\end{tabular}")
latex <- c(latex, "\\\\[2em]")
latex <- c(latex, "\\footnotesize\\textit{Note:} Age effects estimated on standardized age (z-score). The negative linear term and positive quadratic term indicate a U-shaped age pattern.")
latex <- c(latex, "\\end{minipage}")
latex <- c(latex, "\\\\[1em]")
latex <- c(latex, "\\footnotesize\\textit{Note:} Period random effects are deviations from the overall mean (log-odds scale). Positive values indicate years with higher-than-average support after controlling for age and cohort.")
latex <- c(latex, "\\end{table}")

# ==============================================================================
# Write output file
# ==============================================================================

output_file <- file.path(output_dir, "appendixD.tex")
writeLines(latex, output_file)

message("\n========== DONE ==========\n")
message("Generated: ", output_file)
