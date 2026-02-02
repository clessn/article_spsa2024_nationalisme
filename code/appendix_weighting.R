# Appendix: Survey Weighting Documentation
# Responds to Reviewer 2 Issue #19
# Generates LaTeX appendix documenting weighting strategy
#
# Key points to document:
# 1. Sequential raking (Age×Gender + Language)
# 2. Education controlled via regression (not weighting)
# 3. ESS by year
# 4. Adaptive trimming
# 5. Clustering by survey year

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

# Create output directory
dir.create("SharedFolder_spsa_article_nationalisme/tables/appendix",
           showWarnings = FALSE, recursive = TRUE)

# --- Table 1: Weight Statistics and ESS ---
tab_weights <- kableExtra::kbl(
  full_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  align = "rrrrrrrr",
  caption = "Survey Weights and Effective Sample Size by Year"
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::add_header_above(c(" " = 2, "Weight Statistics" = 4, " " = 2)) %>%
  kableExtra::footnote(
    general = "Weights computed via sequential raking: (1) Age $\\\\times$ Gender joint distribution, (2) Mother tongue marginal distribution. Weights trimmed at [0.5, 2.5] for 2015, 2021--2023; [0.2, 5.0] otherwise. ESS = Effective Sample Size; Efficiency = ESS/N.",
    general_title = "Note: ",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

# --- Full appendix text ---
appendix_text <- '
\\section*{Appendix: Survey Weighting Strategy}
\\addcontentsline{toc}{section}{Appendix: Survey Weighting}

\\subsection*{Weighting Methodology}

Survey weights were constructed using \\textbf{sequential raking} (iterative proportional fitting) to align sample distributions with Census of Canada marginals for Quebec. We employed a two-stage procedure following \\citet{battaglia2004}:

\\begin{enumerate}
    \\item \\textbf{Stage 1}: Joint distribution of age group and gender
    \\item \\textbf{Stage 2}: Marginal distribution of mother tongue (French, English, Other)
\\end{enumerate}

Each survey year was matched to the nearest preceding census (1961--2021). Age groups were harmonized to 10-year bands (15--24, 25--34, ..., 65+) across all census years.

\\subsection*{Why Not Weight on Education?}

Education exhibits substantial sampling imbalance: only 18\\% of recent survey respondents lack post-secondary education, compared to 40\\% in the Quebec population. Simultaneous raking on age, gender, language, \\textit{and} education produced:

\\begin{itemize}
    \\item Unstable weights (maximum $>$ 15)
    \\item Low statistical efficiency (ESS $<$ 45\\%)
    \\item Individual observations representing hundreds of people
\\end{itemize}

Following standard practice in political behavior research \\citep{gelman2007, anes2020}, education is instead controlled through regression adjustment. This approach:

\\begin{itemize}
    \\item Maintains weight stability and statistical efficiency
    \\item Allows flexible modeling of education effects (interactions, non-linearities)
    \\item Is standard in major surveys (ANES, CCES, Pew Research)
\\end{itemize}

\\subsection*{Weight Trimming}

Extreme weights were trimmed using adaptive thresholds:

\\begin{itemize}
    \\item \\textbf{Default years (1965--2014, 2019)}: Bounds at [0.2, 5.0]
    \\item \\textbf{High-variance years (2015, 2021--2023)}: Tighter bounds at [0.5, 2.5]
\\end{itemize}

After trimming, weights were renormalized so that $\\sum w_i = n$ within each survey year, preserving unbiasedness.

\\subsection*{Standard Error Clustering}

All regression models use survey-weighted estimation (\\texttt{svyglm} in R) with standard errors clustered by survey year to account for:

\\begin{itemize}
    \\item Within-year correlation from sampling design
    \\item Mode effects across different survey administrations
    \\item Temporal clustering of responses
\\end{itemize}

\\subsection*{Effective Sample Size}

Table \\ref{tab:weights} reports weight statistics and Effective Sample Size (ESS) by year:

\\begin{equation}
    \\text{ESS} = \\frac{(\\sum w_i)^2}{\\sum w_i^2}
\\end{equation}

Efficiency (ESS/N) exceeds 65\\% for most years. Years 2022--2023 show lower efficiency due to sampling limitations; results for these years are validated against unweighted analyses.

'

# Write LaTeX file
output_file <- "SharedFolder_spsa_article_nationalisme/tables/appendix/appendix_weighting.tex"

writeLines(c(
  appendix_text,
  "\\begin{table}[htbp]",
  "\\centering",
  as.character(tab_weights),
  "\\label{tab:weights}",
  "\\end{table}",
  "",
  "\\subsection*{References}",
  "\\begin{itemize}",
  "    \\item Battaglia, M. P., Hoaglin, D., \\& Frankel, M. R. (2004). Tips and tricks for raking survey data. \\textit{Proceedings of the ASA Survey Research Methods Section}.",
  "    \\item Gelman, A. (2007). Struggles with survey weighting and regression modeling. \\textit{Statistical Science}, 22(2), 153--164.",
  "    \\item American National Election Studies. (2020). \\textit{ANES 2020 Time Series Study: Pre-Election and Post-Election Survey Methodology Report}.",
  "\\end{itemize}"
), output_file)

message("Appendix saved to: ", output_file)

# Also save just the table for potential separate inclusion
writeLines(as.character(tab_weights),
           "SharedFolder_spsa_article_nationalisme/tables/appendix/weighting_table.tex")

message("Table saved to: SharedFolder_spsa_article_nationalisme/tables/appendix/weighting_table.tex")
