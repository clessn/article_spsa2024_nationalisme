library(dplyr)
library(ggplot2)
library(patchwork)

# Data --------------------------------------------------------------------

model <- readRDS("SharedFolder_spsa_article_nationalisme/modelsouv2023.rds")

data <- model$model %>% 
  filter(generation == "z")

table(data$region)
table(data$langue)
table(data$region, data$langue)

table(zoomers$region)
table(zoomers$region, zoomers$langue)
table(zoomers$langue)

Strat <- readRDS("SharedFolder_spsa_article_nationalisme/data/census/poststrat.rds") %>% 
  rename(region = large_region) %>% 
  mutate(region = ifelse(region == "other", "region", region),
         gender = ifelse(gender == "men", "male", "female"))

Preds <- marginaleffects::predictions(model, newdata = Strat) %>% 
  mutate(langue = factor(langue, levels = c("french", "english", "other")))

regions <- Strat %>% 
  group_by(riding_id) %>% 
  summarise(region = unique(region))

PredsZ <- Preds %>% 
  filter(generation == "z") %>% 
  group_by(riding_id) %>% 
  mutate(in_z_prct = prct / sum(prct),
         weighted_estimate = estimate * in_z_prct,
         weighted_stderr = std.error^2 * in_z_prct^2) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(in_z_prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(in_z_prct)^2 ),
            total_pop = mean(total_pop),
            total_weight = sum(prct)) %>% 
  mutate(low_ci = weighted_mean_estimate - (weighted_stderr * 1.96),
         high_ci = weighted_mean_estimate + (weighted_stderr * 1.96))

region_ridings <- read.csv("SharedFolder_spsa_article_nationalisme/data/census/region_ridings.csv")
riding_names <- region_ridings$riding_name
names(riding_names) <- region_ridings$riding_id
PredsZ$riding_name <- riding_names[as.character(PredsZ$riding_id)]

PredsZ$region <- regions$region[regions$riding_id == PredsZ$riding_id]

# Graph -------------------------------------------------------------------

table(PredsZ$region)

plot_theme <- function(){
  list(
    geom_point(size = 3, aes(color = total_weight)),
    geom_linerange(aes(xmin = low_ci, xmax = high_ci, color = total_weight),
                   linewidth = 1),
    #xlab("\nWeighted average of predicted\npositions on independence scale\n"),
    xlab(""),
    ylab(""),
    clessnverse::theme_clean_light(),
    scale_color_gradient(name = "Weight of Gen Z\nin riding (%)",
                         low = "grey95", high = "black",
                         breaks = c(12:16),
                         limits = c(12, 16)),
    scale_x_continuous(limits = c(0.2, 0.45)),
    theme(legend.position = "bottom",
          legend.title = element_text())
  )
}

region_names <- c(
  "region" = "Regions",
  "mtl" = "Montreal",
  "qc" = "Quebec City",
  "rmr" = "Greater\nMontreal Area"
)

for (i in 1:length(unique(Preds$region))){
  regioni <- unique(Preds$region)[i]
  ploti <- PredsZ %>% 
    mutate(total_weight = total_weight * 100,
           low_ci = ifelse(low_ci < 0.2, 0.2, low_ci),
           high_ci = ifelse(high_ci > 0.45, 0.45, high_ci)) %>% 
    filter(region == regioni) %>%
    ggplot(aes(x = weighted_mean_estimate, y = reorder(riding_name, weighted_mean_estimate))) +
    plot_theme() +
    ggtitle(region_names[regioni])
  assign(value = ploti, x = paste0("plot_", regioni))
}

plot_region + (plot_mtl / plot_rmr / plot_qc +
               plot_layout(heights = c(0.4354839, 0.4354839, 0.1290323))) +
  plot_layout(guides = 'collect') &
  plot_annotation(title = "Current Attitudes of Gen Z on Quebec Sovereignty",
                  subtitle = "By Provincial Electoral Riding",
                  caption = "Survey data from 2021 to 2023, n = 6687. The X-axis shows the riding-wise weighted average of\nattitudes towards Quebec sovereignty based on a linear model and the riding's 2021 census data.") &
  guides(color = guide_colorbar(title.position = "top")) &
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.2, face = "italic"),
        plot.title = )

ggsave("SharedFolder_spsa_article_nationalisme/graphs/zoomer_by_riding.png",
       width = 8, height = 14)


### wide 

plot_theme2 <- function(end){
  list(
    geom_point(size = 3, aes(color = total_weight)),
    geom_linerange(aes(ymin = low_ci, ymax = high_ci, color = total_weight),
                   linewidth = 1),
    #xlab("\nWeighted average of predicted\npositions on independence scale\n"),
    xlab(""),
    ylab(""),
    clessnverse::theme_clean_light(),
    scale_color_gradient(name = "Weight of Gen Z\nin riding (%)",
                         low = "grey95", high = "black",
                         breaks = c(12:16),
                         limits = c(12, 16)),
    scale_y_continuous(limits = c(0.2, 0.45),
                       expand = c(0, 0)),
    theme(legend.position = "bottom",
          legend.title = element_text(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  )
}

region_names <- c(
  "region" = "Regions",
  "mtl" = "Montreal",
  "qc" = "Quebec City",
  "rmr" = "Greater\nMontreal Area"
)

for (i in 1:length(unique(Preds$region))){
  regioni <- unique(Preds$region)[i]
  endi <- nrow(PredsZ %>% filter(region == regioni))
  ploti <- PredsZ %>% 
    mutate(total_weight = total_weight * 100,
           low_ci = ifelse(low_ci < 0.2, 0.2, low_ci),
           high_ci = ifelse(high_ci > 0.45, 0.45, high_ci)) %>% 
    filter(region == regioni) %>%
    ggplot(aes(y = weighted_mean_estimate, x = reorder(riding_name, -weighted_mean_estimate))) +
    plot_theme2(end = endi) +
    ggtitle(region_names[regioni])
  if (regioni == "region"){
    ploti <- ploti + ylab("\nWeighted average of predicted\npositions on independence scale\n") +
      geom_segment(x = endi - 1.75, y = 0.42, xend = endi - 1.75, yend = 0.45,
                   arrow = arrow(length = unit(0.03, "npc"))) +
      geom_text(label = "Less\nfederalist",
                x = endi, y = 0.45,
                angle = 90, size = 2.75, hjust = 1,
                vjust = 0, lineheight = 0.7)
  }
  assign(value = ploti, x = paste0("plot2_", regioni))
}

plot2_region / (plot2_mtl | plot2_rmr | plot2_qc +
                 plot_layout(widths = c(0.5, 0.5, 0.15))) +
  plot_layout(guides = 'collect') &
  plot_annotation(title = "Current Attitudes of Gen Z on Quebec Sovereignty",
                  subtitle = "By Provincial Electoral Riding",
                  caption = "Survey data from 2021 to 2023, n = 6687. The Y-axis shows the riding-wise weighted average of attitudes towards Quebec sovereignty based on a linear model and the riding's 2021 census data.\nThe dependent variable ranges from 0 to 1 where 0 indicates a strong federalist attitude and 1 indicates a strong separatist atittude.") &
  guides(color = guide_colorbar(title.position = "left")) &
  theme(legend.position = "right",
        legend.title = element_text(angle = 90),
        plot.caption = element_text(hjust = 0.1, face = "italic", color = "grey20"),
        plot.title = element_text(face = "bold", size = 24,
                                  hjust = 0.1),
        plot.subtitle = element_text(hjust = 0.058, size = 19, color = "grey20"))

ggsave("SharedFolder_spsa_article_nationalisme/graphs/zoomer_by_riding_wide.png",
       height = 11, width = 14)
