# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(patchwork)

# Data --------------------------------------------------------------------

Strat <- readRDS("SharedFolder_spsa_article_nationalisme/data/census/poststrat.rds") %>% 
  rename(region = large_region) %>% 
  mutate(region = ifelse(region == "other", "region", region),
         gender = ifelse(gender == "men", "male", "female"))

model <- readRDS("SharedFolder_spsa_article_nationalisme/modelsouv2023.rds")

region_ridings <- read.csv("SharedFolder_spsa_article_nationalisme/data/census/region_ridings.csv")

## Water Data ------------------------------------------------------------
### Coordinates to plot water on the maps
shapename <- readRDS('SharedFolder_spsa_article_nationalisme/data/census/geo/hydro_s.rds')
#### Only keep relevant water
stlaus <- which(shapename$HYS_NM_TOP %in% c("Fleuve Saint-Laurent", "Golfe du Saint-Laurent",
                                            "Lac Saint-Pierre",
                                            #"Rivière des Outaouais",
                                            "Rivière Rideau",
                                            "Lac Saint-Louis",
                                            "Rivière des Mille Îles",
                                            "Rivière des Prairies",
                                            "Lac des Deux Montagnes",
                                            "Canal de Beauharnois",
                                            "Rivière Saguenay",
                                            "Lac Saint-Jean"))
StLau <- shapename[stlaus,]
rownames(StLau) <- 1:nrow(StLau)


## Riding coordinates ------------------------------------------------------
riding_coordinates <- sf::read_sf('SharedFolder_spsa_article_nationalisme/data/census/geo/ridings/Circonscription_electorale_2022_shapefile.shp')
######### Check if ridings from Data are all in data2
sum(Strat$riding_id %in% riding_coordinates$CO_CEP)
########### Good!

## Riding centroids
riding_coordinates <- cbind(riding_coordinates,
                   sf::st_coordinates(sf::st_centroid(riding_coordinates$geometry))) %>% 
  rename(riding_id = "CO_CEP") %>% 
  left_join(., region_ridings, by = "riding_id")

region_coords <- riding_coordinates %>% 
  group_by(granular) %>% 
  summarise(X = mean(X),
            Y = mean(Y))

distances <- expand.grid(riding_id1 = region_coords$granular,
                         riding_id2 = region_coords$granular) %>%
  filter(riding_id1 != riding_id2) %>% 
  mutate(distance = sqrt((region_coords$X[match(riding_id1, region_coords$granular)] - 
                           region_coords$X[match(riding_id2, region_coords$granular)])^2 +
                          (region_coords$Y[match(riding_id1, region_coords$granular)] - 
                             region_coords$Y[match(riding_id2, region_coords$granular)])^2)) %>% 
  filter(!duplicated(distance))

ggplot(riding_coordinates, aes(x = X, y = Y)) +
  geom_point() +
  geom_point(data = region_coords,
             size = 5, color = "red",
             shape = 15)

# Predict -----------------------------------------------------------------

Preds <- marginaleffects::predictions(model, newdata = Strat) %>% 
  mutate(langue = factor(langue, levels = c("french", "english", "other")))

# Graphs ------------------------------------------------------------------

one_map <- function(data, fill_variable, alpha_variable = NULL, region = NULL,
                    low_color = "#ff0000", mid_color = "yellow", high_color = "dodgerblue",
                    legend = FALSE){
  data$n <- data[[fill_variable]]
  if (!is.null(alpha_variable)){
    data$alphavar <- data[[alpha_variable]]
  }
  plot <- data %>% 
    ggplot() +
    geom_sf(data = StLau, size = 0, color = NA, fill = "lightblue")
  if (!is.null(alpha_variable)){
    plot <- plot +
      geom_sf(fill = "black") +
      geom_sf(aes(fill = n, alpha = alphavar),
              color = NA) +
      scale_alpha_continuous(range = c(0.2, 1))
  } else {
    plot <- plot +
      geom_sf(aes(fill = n), color = NA)
  }
  plot <- plot +
    scale_fill_gradientn(colors = c(low_color, mid_color, high_color)) +
    geom_sf(data = StLau, size = 0, color = NA, fill = "lightblue") +
    cartoquebec::theme_map_minimal(legend)
  if (!is.null(region)){
    plot <- plot + cartoquebec::zoom_map(long_min = cartoquebec::region_coords[[region]]["long_min"],
                                         long_max = cartoquebec::region_coords[[region]]["long_max"],
                                         lat_min =  cartoquebec::region_coords[[region]]["lat_min"],
                                         lat_max =  cartoquebec::region_coords[[region]]["lat_max"]) +
      cartoquebec::theme_map_minimal()
  }
  return(plot)
}

## Everyone ----------------------------------------------------------------

PredsAll <- Preds %>% 
  mutate(weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 ),
            total_pop = mean(total_pop)) %>% 
  ungroup() %>% 
  mutate(weighted_mean_estimate = ifelse(weighted_mean_estimate < 0, 0, weighted_mean_estimate),
         weighted_mean_estimate = ifelse(weighted_mean_estimate > 1, 1, weighted_mean_estimate),
         prop_qcpop = total_pop / sum(total_pop))

mapdata_all <- left_join(riding_coordinates, PredsAll, by = "riding_id")

## with weight
all <- one_map(mapdata_all, fill_variable = "weighted_mean_estimate", "region", alpha_variable = "prop_qcpop")
mtl <- one_map(mapdata_all, "weighted_mean_estimate", "mtl", alpha_variable = "prop_qcpop")
rmr <- one_map(mapdata_all, "weighted_mean_estimate", "rmr", alpha_variable = "prop_qcpop")
qc <-  one_map(mapdata_all, "weighted_mean_estimate", "qc", alpha_variable = "prop_qcpop")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/everyone_weight.png",
       width = 8, height = 5)

## without weight
all <- one_map(mapdata_all, "weighted_mean_estimate", region = "region")
mtl <- one_map(mapdata_all, "weighted_mean_estimate", region = "mtl")
rmr <- one_map(mapdata_all, "weighted_mean_estimate", region = "rmr")
qc <-  one_map(mapdata_all, "weighted_mean_estimate", region = "qc")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/everyone_noweight.png",
       width = 8, height = 5)


## Only Y and Z ------------------------------------------------------------

PredsYZ <- Preds %>% 
  mutate(generation = as.character(generation),
         generation = ifelse(generation %in% c("y", "z"), "yz", generation),
         weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, generation) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 ),
            total_pop = mean(total_pop),
            total_weight = sum(prct)) %>%
  mutate(total_weight = ifelse(total_weight >= 0.4, 0.4, total_weight)) %>% 
  filter(generation == "yz")

mapdata_yz <- left_join(riding_coordinates, PredsYZ, by = "riding_id")

## without weight
all <- one_map(mapdata_yz, "weighted_mean_estimate", region = "region")
mtl <- one_map(mapdata_yz, "weighted_mean_estimate", region = "mtl")
rmr <- one_map(mapdata_yz, "weighted_mean_estimate", region = "rmr")
qc <-  one_map(mapdata_yz, "weighted_mean_estimate", region = "qc")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/yz_noweight.png",
       width = 8, height = 5)

## without alpha, only weight as fill
all <- one_map(mapdata_yz, "total_weight", region = "region",
               low_color = "white", mid_color = "grey", high_color = "black")
mtl <- one_map(mapdata_yz, "total_weight", region = "mtl",
               low_color = "white", mid_color = "grey", high_color = "black")
rmr <- one_map(mapdata_yz, "total_weight", region = "rmr",
               low_color = "white", mid_color = "grey", high_color = "black")
qc <-  one_map(mapdata_yz, "total_weight", region = "qc",
               low_color = "white", mid_color = "grey", high_color = "black")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/yz_fillweight.png",
       width = 8, height = 5)

## with weight
all <- one_map(mapdata_yz, fill_variable = "weighted_mean_estimate", "region", alpha_variable = "total_weight")
mtl <- one_map(mapdata_yz, "weighted_mean_estimate", "mtl", alpha_variable = "total_weight")
rmr <- one_map(mapdata_yz, "weighted_mean_estimate", "rmr", alpha_variable = "total_weight")
qc <-  one_map(mapdata_yz, "weighted_mean_estimate", "qc", alpha_variable = "total_weight")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/yz_weight.png",
       width = 8, height = 5)


## Only Y ------------------------------------------------------------------

PredsY <- Preds %>% 
  mutate(generation = as.character(generation),
         weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, generation) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 ),
            total_pop = mean(total_pop),
            total_weight = sum(prct)) %>%
  mutate(total_weight = ifelse(total_weight >= 0.4, 0.4, total_weight)) %>% 
  filter(generation == "y")

mapdata_y <- left_join(riding_coordinates, PredsY, by = "riding_id")

## without weight
all <- one_map(mapdata_y, "weighted_mean_estimate", region = "region")
mtl <- one_map(mapdata_y, "weighted_mean_estimate", region = "mtl")
rmr <- one_map(mapdata_y, "weighted_mean_estimate", region = "rmr")
qc <-  one_map(mapdata_y, "weighted_mean_estimate", region = "qc")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/y_noweight.png",
       width = 8, height = 5)

## without alpha, only weight as fill
all <- one_map(mapdata_y, "total_weight", region = "region",
               low_color = "white", mid_color = "grey", high_color = "black")
mtl <- one_map(mapdata_y, "total_weight", region = "mtl",
               low_color = "white", mid_color = "grey", high_color = "black")
rmr <- one_map(mapdata_y, "total_weight", region = "rmr",
               low_color = "white", mid_color = "grey", high_color = "black")
qc <-  one_map(mapdata_y, "total_weight", region = "qc",
               low_color = "white", mid_color = "grey", high_color = "black")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/y_fillweight.png",
       width = 8, height = 5)

## with weight
all <- one_map(mapdata_y, fill_variable = "weighted_mean_estimate", "region", alpha_variable = "total_weight")
mtl <- one_map(mapdata_y, "weighted_mean_estimate", "mtl", alpha_variable = "total_weight")
rmr <- one_map(mapdata_y, "weighted_mean_estimate", "rmr", alpha_variable = "total_weight")
qc <-  one_map(mapdata_y, "weighted_mean_estimate", "qc", alpha_variable = "total_weight")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/y_weight.png",
       width = 8, height = 5)


## Only Z ------------------------------------------------------------------

PredsZ <- Preds %>% 
  mutate(generation = as.character(generation),
         weighted_estimate = estimate * prct,
         weighted_stderr = std.error^2 * prct^2) %>% 
  group_by(riding_id, generation) %>% 
  summarise(weighted_mean_estimate = sum(weighted_estimate) / sum(prct),
            weighted_stderr = sqrt(sum(weighted_stderr) / sum(prct)^2 ),
            total_pop = mean(total_pop),
            total_weight = sum(prct)) %>% 
  filter(generation == "z")

mapdata_z <- left_join(riding_coordinates, PredsZ, by = "riding_id")

## without weight
all <- one_map(mapdata_z, "weighted_mean_estimate", region = "region")
mtl <- one_map(mapdata_z, "weighted_mean_estimate", region = "mtl")
rmr <- one_map(mapdata_z, "weighted_mean_estimate", region = "rmr")
qc <-  one_map(mapdata_z, "weighted_mean_estimate", region = "qc")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/z_noweight.png",
       width = 8, height = 5)

## without alpha, only weight as fill
all <- one_map(mapdata_z, "total_weight", region = "region",
               low_color = "white", mid_color = "grey", high_color = "black")
mtl <- one_map(mapdata_z, "total_weight", region = "mtl",
               low_color = "white", mid_color = "grey", high_color = "black")
rmr <- one_map(mapdata_z, "total_weight", region = "rmr",
               low_color = "white", mid_color = "grey", high_color = "black")
qc <-  one_map(mapdata_z, "total_weight", region = "qc",
               low_color = "white", mid_color = "grey", high_color = "black")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/z_fillweight.png",
       width = 8, height = 5)

## with weight
all <- one_map(mapdata_z, fill_variable = "weighted_mean_estimate", "region", alpha_variable = "total_weight")
mtl <- one_map(mapdata_z, "weighted_mean_estimate", "mtl", alpha_variable = "total_weight")
rmr <- one_map(mapdata_z, "weighted_mean_estimate", "rmr", alpha_variable = "total_weight")
qc <-  one_map(mapdata_z, "weighted_mean_estimate", "qc", alpha_variable = "total_weight")
cartoquebec::maps_layout_classic(all, mtl, rmr, qc)
ggsave("SharedFolder_spsa_article_nationalisme/graphs/maps/z_weight.png",
       width = 8, height = 5)


