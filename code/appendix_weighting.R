# Appendix: Survey Weighting Documentation
# Responds to Reviewer 2 Issue #19
# Generates LaTeX appendix documenting weighting strategy

library(dplyr)
library(tibble)
library(kableExtra)

# =============================================================================
# LOAD DATA AND COMPUTE STATISTICS
# =============================================================================

data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds")

# ESS summary by year
ess_table <- data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    ess = round(first(ess_trimmed)),
    efficiency = round(first(efficiency_trimmed) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    efficiency_display = paste0(efficiency, "\\%")
  )

# Weight statistics
weight_stats <- data %>%
  group_by(year) %>%
  summarise(
    mean_w = round(mean(weight_trimmed, na.rm = TRUE), 2),
    sd_w = round(sd(weight_trimmed, na.rm = TRUE), 2),
    min_w = round(min(weight_trimmed, na.rm = TRUE), 2),
    max_w = round(max(weight_trimmed, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Merge for full table
full_table <- ess_table %>%
  left_join(weight_stats, by = "year") %>%
  select(year, n, mean_w, sd_w, min_w, max_w, ess, efficiency_display) %>%
  rename(
    Year = year,
    N = n,
    Mean = mean_w,
    SD = sd_w,
    Min = min_w,
    Max = max_w,
    ESS = ess,
    Efficiency = efficiency_display
  )

# =============================================================================
# GENERATE LATEX
# =============================================================================

dir.create("SharedFolder_spsa_article_nationalisme/tables/appendix",
           showWarnings = FALSE, recursive = TRUE)

# --- Table with caption and label ---
tab_weights <- kableExtra::kbl(
  full_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  align = "rrrrrrrr",
  caption = "Survey Weights and Effective Sample Size by Year",
  label = "weights"
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::add_header_above(c(" " = 2, "Weight Statistics" = 4, " " = 2))

# --- Appendix text (no citations, no bold, starts at subsection) ---
appendix_text <- '
\\subsection{Weighting Methodology}

Survey weights were constructed using sequential raking (iterative proportional fitting) to align sample distributions with Census of Canada marginals for Quebec. Two-stage procedure:

\\begin{enumerate}
    \\item Stage 1: Joint distribution of age group and gender
    \\item Stage 2: Marginal distribution of mother tongue (French, English, Other)
\\end{enumerate}

Each survey year was matched to the nearest preceding census (1961--2021). Age groups were harmonized to 10-year bands (15--24, 25--34, \\ldots, 65+).

\\subsection{Education: Regression Control vs. Weighting}

Education exhibits substantial sampling imbalance: only 18\\% of recent survey respondents lack post-secondary education, compared to 40\\% in the Quebec population. Simultaneous raking on age, gender, language, and education produced unstable weights (maximum $>$ 15) and low statistical efficiency (ESS $<$ 45\\%).

Education is instead included as a control variable in the regression analyses. This maintains weight stability and allows flexible modeling of education effects.

\\subsection{Weight Trimming}

Extreme weights were trimmed using adaptive thresholds:

\\begin{itemize}
    \\item Default years (1965--2014, 2019): bounds at [0.2, 5.0]
    \\item High-variance years (2015, 2021--2023): tighter bounds at [0.5, 2.5]
\\end{itemize}

After trimming, weights were renormalized so that $\\sum w_i = n$ within each survey year.

\\subsection{Standard Error Clustering}

All regression models use survey-weighted estimation with standard errors clustered by survey year to account for within-year correlation, mode effects, and temporal clustering.

\\subsection{Effective Sample Size}

Effective Sample Size (ESS) is calculated as:

$$\\text{ESS} = \\frac{(\\sum w_i)^2}{\\sum w_i^2}$$

Efficiency (ESS/N) exceeds 65\\% for most years. Years 2022--2023 show lower efficiency due to sampling limitations. Table~\\ref{tab:weights} reports weight statistics by year.
'

# Write LaTeX file (text + table together)
output_file <- "SharedFolder_spsa_article_nationalisme/tables/appendix/appendix_weighting.tex"

writeLines(c(appendix_text, as.character(tab_weights)), output_file)

message("Appendix saved to: ", output_file)
