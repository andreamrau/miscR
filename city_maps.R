library(tidyverse)
library(osmdata)
library(ggplot2)
library(tmap)
library(gridExtra)
library(sf)
library(sp)
library(osmplotr)
library(tmaptools)
library(OpenStreetMap)
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_blank(),  
      axis.text.y = element_blank(),  
      axis.ticks = element_blank(),  
      axis.title.x = element_blank(),  
      axis.title.y = element_blank(),  
      axis.ticks.length = unit(0, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.margin = unit(0, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

# https://stackoverflow.com/questions/59144491/r-unusual-error-plotting-multipolygons-with-ggplot-geom-sf-and-openstreetmap
# https://stat.ethz.ch/pipermail/r-help/2011-July/283281.html
# https://stackoverflow.com/questions/57124407/plotting-sea-areas-using-osmplotr-and-openstreetmap-in-r

## Chicago  -----------------------------------------------------
town <- "Chicago United States"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential"
                            #"living_street",
                            #"unclassified",
                            #"service", "footway"
                  )) %>%
  osmdata_sf()

river <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- getbb(town) %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Get information from Lake Michigan, to be plotted as a polygon
lake_gva <- getbb(town) %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sp()
lake_gva$osm_multipolygons@data$id <- rownames(lake_gva$osm_multipolygons@data)
df_lake_gva <- 
  fortify(lake_gva$osm_multipolygons[19,], region = "id") %>% 
  merge(lake_gva$osm_multipolygons@data, by = "id")

## Save city info
town <- list(streets = streets, small_streets = small_streets,
                lakes = lakes, river = river, df_lake_gva = df_lake_gva)
saveRDS(town, file = paste0(town, ".rds"))
streets <- town$streets
small_streets <- town$small_streets
lakes <- town$lakes
river <- town$river
df_lake_gva <- town$df_lake_gva

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  geom_polygon(data = select(df_lake_gva, long, lat, group),
    aes(x = long, y = lat, group = group),
    color = "white", fill = "dodgerblue3", alpha = 1) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)


## Stamford (TODO: plot water?) ---------------------------------------------------------
town <- "Stamford United States"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- getbb(town)%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- getbb(town) %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Coastline information

coast <- getbb(town) %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# coastline <- coast$osm_lines
# islands <- coast$osm_polygons
# background_data <- read_osm(sf::st_bbox(coastline))
# tm_shape(background_data) + 
#   tm_rgb() + 
#   tm_shape(coast$osm_lines) + 
#   tm_lines() + 
#   tm_shape(coast$osm_polygons) + 
#   tm_polygons() + 
#   tm_scale_bar() + 
#   tm_compass(type = "8star", position = c("left", "top"))
# bb <- osmdata::getbb(town)
# coastline0 <- extract_osm_objects (bbox = bb, key = "natural", 
#                                   value = "coastline",
#                               return_type = "line")
# coastline <- osm_line2poly (coastline0, bbox = bb) ## ERROR
# map <- osm_basemap (bbox = bb) %>%
#   add_osm_objects (coastline0, col = "lightsteelblue") %>%
#   print_osm_map ()

## Save city info
town <- list(streets = streets, small_streets = small_streets,
             lakes = lakes, river = river, coast = coast)
saveRDS(town, file = paste0(town, ".rds"))
streets <- town$streets
small_streets <- town$small_streets
lakes <- town$lakes
river <- town$river
coast <- town$coast

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  geom_sf(data = coast$osm_lines,
          color = "dodgerblue3",
          alpha = .65) +
  geom_sf(data = coast$osm_polygons,
          fill = "dodgerblue3",
          size = 1,
          alpha = .65) +
  # geom_polygon(data = select(df_lake_gva, long, lat, group),
  #              aes(x = long, y = lat, group = group),
  #              color = "white", fill = "dodgerblue3", alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)



## Kansas City ---------------------------------------------------------

town <- "Kansas City Missouri United States"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- gb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)




## West Lafayette (darker streets) ---------------------------------------------------------

town <- "West Lafayette United States"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- gb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey20",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 10,
          alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)

## Evry (adjust river size, darker streets) ---------------------------------------------------------

town <- "Evry France"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town) ## This is not quite right
gb[1,] <- c(2.399286, 2.481340)
gb[2,] <- c(48.608614, 48.646282)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- gb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey20",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)

## Versailles (adjust castle canal) ---------------------------------------------------------
town <- "Versailles France"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- gb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)

## Milwaukee -----------------------------------------------------
town <- "Milwaukee United States"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

## Get information from Lake Michigan, to be plotted as a polygon
lake_gva <- getbb(town) %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sp()
lake_gva$osm_multipolygons@data$id <- rownames(lake_gva$osm_multipolygons@data)
df_lake_gva <- 
  fortify(lake_gva$osm_multipolygons[5,], region = "id") %>% 
  merge(lake_gva$osm_multipolygons@data, by = "id")


marg <- .03
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  geom_polygon(data = select(df_lake_gva, long, lat, group),
               aes(x = long, y = lat, group = group),
               color = "white", fill = "dodgerblue3", alpha = 1) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)






## Saint Quentin (TODO fill in canal) -----------------------------------------------------

town <- "Saint Quentin France"
town_print <- strsplit(town, split = " ") %>% lapply(., head, 1) %>% unlist
gb <- getbb(town)

streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c(#"motorway", 
                            "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- gb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

river <- gb %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river", "canal")) %>%
  osmdata_sf()

lakes <- gb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

## Plot!
marg <- 0
g2 <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .6,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey50",
          size = .3,
          alpha = .5) +
  geom_sf(data = lakes$osm_polygons,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          fill = "dodgerblue3", alpha = .65, size = 0.5) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "dodgerblue3",
          size = 1,
          alpha = .65) +
  coord_sf(xlim = c(gb[1,1]-marg, gb[1,2]+marg), 
           ylim = c(gb[2,1]-marg, gb[2,2]+marg),
           expand = FALSE) +
  theme_void()
g2
## 28 x 24 in PPT
ggsave(g2, file=paste0(town_print, ".png"), width = 30, height = 35)

## Rennes