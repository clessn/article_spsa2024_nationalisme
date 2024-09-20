# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
raw_data <- haven::read_dta("SharedFolder_spsa_article_nationalisme/data/ces2021.dta") |> 
  filter(
    cps21_province == 11
  )

# Clean ------------------------------------------------------------------

data <- data.frame(
  id = 1:nrow(raw_data),
  yob = 2021 - raw_data$cps21_yob) |>
  mutate(
    generation = case_when(
      yob <= 1947 ~ "preboomer",
      yob %in% 1947:1961 ~ "boomer",
      yob %in% 1962:1976 ~ "x",
      yob %in% 1977:1991 ~ "y",
      yob %in% 1992:2003 ~ "z"
    ),
    generation = factor(
      generation,
      levels = c("preboomer", "boomer", "x", "y", "z")
    ),
    souv = case_when(
      raw_data$cps21_quebec_sov %in% c(1, 2) ~ 1,
      raw_data$cps21_quebec_sov %in% c(3, 4) ~ 0
    )
)

### pes21_ethid

raw_data$pes21_ethid_1[raw_data$pes21_ethid_1 == 5] <- NA
raw_data$pes21_ethid_2[raw_data$pes21_ethid_2 == 5] <- NA
raw_data$pes21_ethid_3[raw_data$pes21_ethid_3 == 5] <- NA

table(raw_data$pes21_ethid_1)
data$identity_canadian <- sondr::clean_likert_numeric_vector(raw_data$pes21_ethid_1)
table(data$identity_canadian)

table(raw_data$pes21_ethid_2)
data$identity_ethnicity <- sondr::clean_likert_numeric_vector(raw_data$pes21_ethid_2)
table(data$identity_ethnicity)

table(raw_data$pes21_ethid_3)
data$identity_language <- sondr::clean_likert_numeric_vector(raw_data$pes21_ethid_3)
table(data$identity_language)

## Final data for graph

final_data <- data |>
  tidyr::drop_na() |> 
  tidyr::pivot_longer(
    cols = starts_with("identity_"),
    names_to = "caracteristic",
    names_prefix = "identity_",
    values_to = "likert_response"
  )

table(final_data$generation[final_data$caracteristic == "language"])


final_data |>
  group_by(generation, souv, caracteristic) |>
  summarise(
    mean = mean(likert_response)
  ) |> 
  ggplot(
    aes(x = generation, y = mean, group = souv)
  ) +
  facet_wrap(~caracteristic) +
  geom_point(
    aes(color = souv),
    size = 7,
    position = position_dodge(width = 0.5)
  ) +
  clessnize::theme_clean_light(base_size = 20)
