# Appendix: Sample sizes by year
# Shows complete cases for core variables used across analyses

library(dplyr)
library(tidyr)

# Data --------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v2.rds")

# Core variables: sovereignty (DV), age, gender, language
# These are the minimum variables needed for most analyses

data_core <- data %>%
  mutate(
    # Unified sovereignty variable
    souv = case_when(
      year %in% c(1974, 1984) ~ iss_souv,
      TRUE ~ iss_souv2
    )
  ) %>%
  select(year, souv, ses_age, ses_gender, ses_lang.1)

# Complete cases by year --------------------------------------------------
sample_sizes <- data_core %>%
  filter(complete.cases(.)) %>%
  count(year, name = "n") %>%
  arrange(year)

# Total
total_n <- sum(sample_sizes$n)

# Print summary
cat("Complete cases by year (core variables: sovereignty, age, gender, language)\n\n")
print(sample_sizes, n = Inf)
cat("\nTotal:", total_n, "\n")

# LaTeX table -------------------------------------------------------------
latex <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Sample sizes by survey year}",
  "\\label{tab:sample-sizes}",
  "\\begin{tabular}{rr}",
  "\\toprule",
  "Year & n \\\\",
  "\\midrule"
)

for (i in 1:nrow(sample_sizes)) {
  latex <- c(latex, sprintf("%d & %s \\\\",
                            sample_sizes$year[i],
                            format(sample_sizes$n[i], big.mark = ",")))
}

latex <- c(latex,
  "\\midrule",
  sprintf("Total & %s \\\\", format(total_n, big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  sprintf("\\begin{tablenotes}"),
  sprintf("\\footnotesize"),
  sprintf("\\item \\textit{Note:} Complete cases for core variables (sovereignty attitude, age, gender, language)."),
  sprintf("\\end{tablenotes}"),
  "\\end{table}"
)

# Save
writeLines(latex, "SharedFolder_spsa_article_nationalisme/tables/appendix_sample_sizes.tex")
cat("\nLaTeX table saved to tables/appendix_sample_sizes.tex\n")
