
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






jldp_sf <- st_read(here('data','jldp_boundary','jldp_boundary.shp')) %>%
  clean_names() %>% 
  st_transform("EPSG:4326")


draw <- read_csv(here("Drawings.csv"))

polygon <- draw %>% 
  dplyr::select(starts_with("features.geometry.coordinates")) %>% 
  as.numeric() %>% 
  matrix(ncol=2, byrow=TRUE) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc(crs = "EPSG:4326")

polygon%>% 
  as.data.frame()

test <- st_extract(area_r, polygon) %>% 
  st_as_sf() %>% 
  st_drop_geometry()

selected <- as.data.frame(test)

t(selected) %>% kbl() %>%
  kable_styling()

tbl_img <- data.frame(
  names= (colnames(selected[1:4])),
  score= (as.numeric(selected[1,1:4]))
)

tbl_img$ImageName =c(here("www","img","bio_icon.png"), 
                here("www","img","soil_icon.png"),
                here("www","img","bio_icon.png"),
                here("www","img","bio_icon.png"))

print(kable(tbl_img, format="html") %>% 
        kable_styling(full_width=FALSE) %>% 
        collapse_rows(columns=1, valign="top"))


tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(3, image = "bio_icon.png") %>% 
  

tbl_img %>%
  kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(3, image = spec_image(
    c(here("www","img","bio_icon.png"), 
      here("www","img","soil_icon.png"),
      here("www","img","bio_icon.png"),
      here("www","img","bio_icon.png")), 2, 2))

as.numeric(selected[1:4])

fig <- plot_ly(
  type = 'scatterpolar',
  r =   as.numeric(selected[1:4]),
  theta = c("Agriculture",
            "Community",
            "Biodiversity",
            "Water"),
  fill = 'toself'
)

fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,5)
      )
    ),
    showlegend = F
  )

fig



tbl_img %>%
  mutate(images = paste0("<","img src=",here("www","img","bio_icon.png"),">")) %>%
  kable(format = "html", escape = F) %>%
  kable_styling(bootstrap_options = "striped")


