
library(tidyverse)
library(here)
library(janitor)
library(leaflet)
library(stars)
library(leafem)
library(caret)







area_sf <- st_read(here('data','SHP_Multi-benefit_Areas_Map','0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp')) %>% # Data subset for testing purposes
  clean_names() %>% 
  st_transform("EPSG:4326")

## Weighting themes (this will be integrated into a reactive() function to run the Shiny App):

range_norm_manual <- function(new_score) {
  (new_score - min(new_score,na.rm = TRUE)) / (max(new_score,na.rm = TRUE) - min(new_score,na.rm = TRUE))
}

range_norm <- function(new_score) {
  ss <- preProcess(as.data.frame(new_score), method=c("range"))
  gfg <- predict(ss, as.data.frame(new_score))
  return(gfg)
}

# Testing weights (which will be dynamically set using a slider)
weight_agr <- 10
weight_com <- 0
weight_flo <- 0
weight_wat <- 0


area_r_all <- st_rasterize(area_sf %>% dplyr::select(agricultur,
                                                 community,
                                                 flora_faun,
                                                 water_raw, 
                                                 eems_synth, 
                                                 geometry)) # Rasterizing shapefile


area_r <- area_r_all %>% 
  mutate("agricultur" = agricultur * weight_agr) %>% 
  mutate("community" = community * weight_com) %>% 
  mutate("flora_faun" = flora_faun * weight_flo) %>% 
  mutate("water_raw" = water_raw * weight_wat) %>% 
  mutate(score = agricultur + community + flora_faun + water_raw) %>% 
  mutate(norm_score = range_norm_manual(score))
  




area_r_test <- area_r["flora_faun"]*3


pal <- colorNumeric(palette= "Greens",
                    domain = area_r[[1]],
                    na.color = "transparent")

geojson::bbox_get(area_r["eems_synth"])

leaflet() %>% addTiles() %>%
  addGeoRaster(area_r["eems_synth"], 
               opacity = 1,
               colorOptions =leafem:::colorOptions(
                 palette = "Greens",
                 breaks = seq(min(area_r["eems_synth"][[1]], na.rm = TRUE),
                              max(area_r["eems_synth"][[1]], na.rm = TRUE),
                              100),
                 na.color = "transparent"
               ),
               resolution=10000) %>% 
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addLegend(pal = pal,
            values = area_r["eems_synth"][[1]]
  )
$