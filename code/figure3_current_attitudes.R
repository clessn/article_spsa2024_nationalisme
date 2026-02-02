# show how language is not the same predictor among different generations

# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data <- readRDS("SharedFolder_spsa_article_nationalisme/data/merged_v1.rds") %>% 
  filter(year >= 2021) |> 
  mutate(yob = year - ses_age,
         generation = case_when(
           yob %in% 1925:1946 ~ "preboomer",
           yob %in% 1947:1961 ~ "boomer",
           yob %in% 1962:1976 ~ "x",
           yob %in% 1977:1991 ~ "y",
           yob %in% 1992:2003 ~ "z"
         ),
         generation = factor(generation))

table(data$iss_souv2)
table(data$iss_souv2, data$year)
### Selon le sondage, on a 4 ou 5 niveaux likert
### On va donc tester 2 lm et les comparer:
#### 1. en assignant de façon random les neutres à une position nuancée
#### 2. en faisant une lm sur la variable raw

## VD 1
set.seed(123)
data$vd1 <- data$iss_souv2
data$vd1 <- ifelse(data$vd1 == 0.25, 0.33,
                   ifelse(data$vd1 == 0.75, 0.66, data$vd1))
data$vd1[!is.na(data$vd1) & data$vd1 == 0.5] <- sample(
  c(0.33, 0.66),
  size = sum(!is.na(data$vd1) & data$vd1 == 0.5),
  replace = TRUE
)
table(data$vd1)

## VD 2
data$vd2 <- data$iss_souv2
data$vd2[data$vd2 == 0.33] <- 0.25
data$vd2[data$vd2 == 0.66] <- 0.75
table(data$vd2)

# Functions --------------------------------------------------------------

create_model <- function(
  data,
  vd
){
  data$vd <- data[[vd]]
  model_data <- data |> 
    select(
      vd,
      generation, ses_lang.1,
      ses_gender,
      ses_family_income_centile_cat,
      ses_origin_from_canada.1,
      ses_educ
    ) |> 
    tidyr::drop_na()
  model <- lm(
    vd ~ generation * ses_lang.1 + .,
    data = model_data
  )
  return(model)
}

create_figure2 <- function(
  data, vd
){
  model <- create_model(
    data, vd
  )
  print(nrow(model$model))

  preds <- marginaleffects::predictions(
    model = model,
    newdata = marginaleffects::datagrid(
      model = model,
      generation = c("preboomer", "boomer", "x", "y", "z"),
      ses_lang.1 = c("english", "french", "other")
    )
  ) |> 
    mutate(
      generation = factor(
        generation,
        levels = c("preboomer", "boomer", "x", "y", "z"),
        labels = c("Preboomer", "Boomer", "X", "Y", "Z")
      ),
      ses_lang.1 = factor(
        ses_lang.1,
        levels = c("french", "english", "other"),
        labels = c("French", "English", "Other")
      )
    )
  
  print(as.data.frame(preds))

  ggplot(preds, aes(x = ses_lang.1, y = estimate, color = ses_lang.1)) +
    facet_wrap(~generation, nrow = 1) +
    geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
    geom_point() +
    scale_color_manual(
      values = c(
        "French" = "black",
        "English" = "grey30",
        "Other" = "grey60"
      )
    ) +
    clessnize::theme_clean_light() +
    xlab("") +
    labs(
      caption = paste0("Predicted position on the independence scale with interaction between generation and language\nwhile controlling for other socio-demographic variables, holding them constant. Data from 2021 to 2023, n = ", nrow(model$model), ".")
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      expand = c(0, 0),
      #labels = c("More\nFederalist", "", "", "", "More\nSeparatist"),
      name = "Predicted Position on\nIndependence Scale\n"
    ) +
    annotate(
      geom = "text",
      x = 0.725, y = 0.99,
      label = "More\nSeparatist",
      hjust = 1, angle = 90,
      size = 2.25, lineheight = 0.75
    ) +
    annotate(
      geom = "text",
      x = 0.725, y = 0.01,
      label = "More\nFederalist",
      hjust = 0, angle = 90,
      size = 2.25, lineheight = 0.75
    ) +
    guides(color = "none") +
    theme(
      panel.grid.major.y = element_line(linewidth = 0.2, color = "grey90"),
      #axis.text.y = element_text(angle = 90, hjust = 0.5),
      axis.text.y = element_blank(),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5),
      strip.text.x = element_text(size = 12),
      panel.background = element_rect(fill = NA, color = "grey75"),
      plot.caption = element_text(hjust = 1)
    )
}

# Graph and save -------------------------------------------------------------------

create_figure2(data, "vd1")
ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure2_language_effect_by_generation_4point.png",
  width = 9, height = 6
)

create_figure2(data, "vd2")
ggsave(
  "SharedFolder_spsa_article_nationalisme/figures/figure2_language_effect_by_generation_5point.png",
  width = 9, height = 6
)
