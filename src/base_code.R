## ---------------------------
##
## Script name: 
## Interactive planner base code
##
## Purpose of script:
## This code defines the main functions and calculations needed to be integrated
## into a Shiny App in order to create an Interactive Planner for a 
## Multi-Benefit Conservation Assessment.
##
## Author: Pol Carbó Mestre
## Date Created: 2022-05-17
## Email: pcarbometre@bren.ucsb.edu
##
## ---------------------------
##
## Notes:
##   This is not a Shiny App, but the R code defining the processes that the
##   App will display. Next step will consist on integrating some of this
##   functions and calculation into reactive() functions and Shiny $outputs.
##
##   The Multi-Benefit Areas data used for testing the code is owned and
##   publicly distributed by the Conservation Biology Institute.
##
## ---------------------------

# Libraries
# ----------------------------------

library(tidyverse)
library(tmap)
library(sf)
library(here)
library(janitor)
library(caret)
library(rmapshaper)
library(profvis)
library(rgdal)
library(leaflet)
library(stars)

# Read in data
# ----------------------------------

## Shapefile containing the multi-benefit areas based on EEMS, produced by CBI.

# Original file
# area_sf <- st_read(here('data','SHP_Multi-benefit_Areas_Map','0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp')) %>%
#      clean_names()

area_sf <- st_read(here('data','small_MB_area','small_MB_area.shp')) %>% # Data subset for testing purposes
  clean_names() %>% 
  st_transform("EPSG:4326") # Set CRS (CRS for testing)

# Topologically-aware geometry simplification using rmapshaper package.
# Use only if need it
# area_sf_simple <- rmapshaper::ms_simplify(area_sf, keep = 0.05, keep_shapes = TRUE)


## Shape file containing partners parcels. The testing file only includes the Dangermont Preserve.

jldp_sf <- st_read(here('data','jldp_boundary','jldp_boundary.shp')) %>%
  clean_names() %>% 
  st_transform("EPSG:4326") # Set CRS


# Data Exploration
# ----------------------------------

summary(area_sf)

# There are 4 themes (agricultur, community, flora_faun and water_raw)
# eems_syn_1 is the sum of all 4 themes
# eems_synth is the normalization of eems_syn_1 using range method

## Note: the score values are based on Environmental Evaluation Modeling System (EEMS) 
## which is a tree-based, fuzzy logic modeling system.
## This system is based on the premise that each input value can be represented 
## by value ranging from -1 for fully false to +1 for fully true, allowing to
## standardize metrics to merge and compare.
## The resulting scores for each cell and theme (range normalized) ranges from
## 0 representing no value to 1 indicating the highest presence degree of that
## conservation value.

## More info: https://sbcblueprint.databasin.org/articles/2d84c23b3dfb4912b073353cbec576db/


# Data visualization (pre-weighting)
# ----------------------------------

# We have different options:
# tmaps:
tmap_mode(mode = "view")
tmap_options(check.and.fix = TRUE)

  tm_shape(area_sf) +
    tm_fill(col = 'eems_synth',
            title = "Area of interest",
            style = 'cont') +
    tm_basemap(leaflet::providers$Stamen.Terrain)
  
# ggplot:
ggplot2::ggplot(data = area_sf) +
  geom_sf(aes(fill=eems_synth),lwd = 0)

# leaflet (chosen package for the app):
binpal <- colorNumeric("Greens", area_sf$eems_synth) # defining pallete

leaflet(area_sf) %>% 
  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
              color = "grey",
              fillColor = ~binpal(eems_synth)) %>% 
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addLegend("bottomright", pal = binpal, values = ~eems_synth,
            title = "Score",
            opacity = 1)


## Rasterization test 
# Tested for computational efficiency purposes. (Discarded for its app inclusion)
# Well avoid the use of the raster package because it masks other packages functions
# The best alternative is the stars package

# stars:
area_r <- st_rasterize(area_sf %>% dplyr::select(eems_synth, geometry)) # Rasterizing shapefile
plot(area_r)

crop_r <- st_crop(area_r, jldp_sf) # Testing raster crop from shapefile
plot(crop_r)


# Setting weights
# ----------------------------------

## Transforming sf to df for its manipulation
areas_df <- st_drop_geometry(area_sf)

## Data Normalization using caret range method

range_norm_caret <- function(new_score) {
  ss <- preProcess(as.data.frame(new_score), method=c("range"))
  gfg <- predict(ss, as.data.frame(new_score))
  return(gfg)
}

range_norm_manual <- function(new_score) {
  (new_score - min(new_score)) / (max(new_score) - min(new_score))
  }

### Testing efficiency
# The weighting and normalization procedure are going to be the most computational demanding process
# Hence, well have to choose the most efficient function.
#We can evaluate code efficiency using profvis (https://rstudio.github.io/profvis/)

profvis({
  range_norm_caret(area_sf$eems_syn_1)
})

profvis({
  range_norm_manual(area_sf$eems_syn_1)
  })

# Alternatively we can check efficiency using system.time:
system.time({ range_norm_caret(area_sf$eems_syn_1) })
system.time({ range_norm_manual(area_sf$eems_syn_1) }) # more efficient


## Weighting themes (this will be integrated into a reactive() function to run the Shiny App):

# Testing weights (which will be dynamically set using a slider)
weight_agr <- 5
weight_com <- 2
weight_flo <- 8
weight_wat <- 7

# To integrate into reactive()
weighted_areas_df <- areas_df %>% 
  mutate_at(vars(agricultur), function(x) weight_agr * x) %>% 
  mutate_at(vars(community), function(x) weight_com * x) %>% 
  mutate_at(vars(flora_faun), function(x) weight_flo * x) %>% 
  mutate_at(vars(water_raw), function(x) weight_wat * x) %>% 
  mutate(score = agricultur + community + flora_faun + water_raw) %>% 
  mutate(norm_score = range_norm_manual(score),.after = objectid) %>% 
  select(c(objectid,norm_score))

# Merging new normalized overall score into the sf file
areas_sf_updated <- left_join(area_sf, weighted_areas_df, by = 'objectid')

## The resulting shapefile will be displayed dynamically using the visualization method defined before


# Combining Multi-benefit areas with partner's parcels
# ----------------------------------

## Displaying both shapefiles
map <- leaflet(area_sf) %>% 
  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
              color = "grey",
              fillColor = ~binpal(eems_synth)) %>% 
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addLegend("bottomright", pal = binpal, values = ~eems_synth,
            title = "Score",
            opacity = 1)
map %>% 
  addPolygons(data=jldp_sf,color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 1,
                                                  bringToFront = TRUE),
              layerId = jldp_sf$shape_leng)


## Intersecting shapefiles
# Cutting shapefile using another shapefile while extracting information

area_jldp_sf<- st_intersection(area_sf, jldp_sf) # Intersecting shapefiles

## Visualizing Multi-benefit areas on parcel of interest
leaflet(area_jldp_sf) %>% 
  addPolygons(stroke = FALSE, fillOpacity = 1, smoothFactor = 0.5,
              color = "grey",
              fillColor = ~binpal(eems_synth)) %>% 
  addProviderTiles(providers$Stamen.Terrain) %>% 
  addLegend("bottomright", pal = binpal, values = ~eems_synth,
            title = "Score",
            opacity = 1)

plot(area_jldp_sf[,c(2,3,6,7)])


## Extracting metrics from parcel of interest

mean_values <- st_intersection(area_sf, jldp_sf) %>% 
  st_drop_geometry() %>% 
  summarize(global_score = mean(eems_synth),
            agricultur_score = mean(agricultur),
            community_score = mean(community),
            bio_score = mean(flora_faun),
            water_score = mean(water_raw)) %>% 
  mutate(id=jldp_sf$global_id) # ID for map button display recognition (Shiny functionality)

t(mean_values)

drought <- c("drgh_m_fz","drgh_h_fz")

threats_axis_r %>%
  select(!all_of(drought)) %>% 
  rename("drgh_fz" = starts_with("drgh")) %>% 
  mutate("cli_exp_fz" = cli_exp_fz * 0.25) %>%
  mutate("drgh_m_fz" = drgh_fz * 0.25) %>%
  mutate("fld_m_fz" = fld_m_fz * 0.25) %>%
  mutate("wf_m_fz" = wf_m_fz * 0.25) %>%
  mutate(score = cli_exp_fz + drgh_m_fz + fld_m_fz + wf_m_fz) %>%
  mutate(norm_score = range_norm_manual(score))



input1 <- "All"

group1 <- ahp_weights %>% 
  filter(group %in% input1)

resources_axis_1 <-  resources_axis_r %>%
  mutate("soil_fz" = soil_fz * as.numeric(group1[3,3])) %>%
  mutate("resil_fz" = resil_fz * as.numeric(group1[4,3])) %>%
  mutate("bio_fz" = bio_fz * as.numeric(group1[1,3])) %>%
  mutate("water_fz" = water_fz * as.numeric(group1[2,3])) %>%
  mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
  mutate(norm_score = range_norm_manual(score))

input2 <- "Farm/Ranch"

group2 <- ahp_weights %>% 
  filter(group %in% input2)

resources_axis_2 <-  resources_axis_r %>%
  mutate("soil_fz" = soil_fz * as.numeric(group2[3,3])) %>%
  mutate("resil_fz" = resil_fz * as.numeric(group2[4,3])) %>%
  mutate("bio_fz" = bio_fz * as.numeric(group2[1,3])) %>%
  mutate("water_fz" = water_fz * as.numeric(group2[2,3])) %>%
  mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
  mutate(norm_score = range_norm_manual(score))

extracted_df_2 <- cbind(soil_fz= as.numeric(resources_axis_2$soil_fz),
                        resil_fz= as.numeric(resources_axis_2$resil_fz),
                        bio_fz= as.numeric(resources_axis_2$bio_fz),
                        water_fz= as.numeric(resources_axis_2$water_fz),
                        agg_val= as.numeric(resources_axis_2$norm_score)) %>%
  as.data.frame()
  
mean_extracted_values_2 <- apply(extracted_df_2,2,mean,na.rm=T)


summary_data <- data.frame(Resource=c("Soil","Resilience","Biodiversity","Water resources","Aggregated score"),
                           Score_1= mean_extracted_values_2,
                           Score_2= mean_extracted_values_2)

rownames(summary_data)<-NULL

dplyr::tibble(img=c(here("www","img","soil_icon.png"),
                    here("www","img","resilience_icon.png"),
                    here("www","img","bio_icon.png"),
                    here("www","img","water_icon.png")),
              summary_data %>%
                filter(!Resource %in% "Aggregated score")) %>%
  gt() %>%
  fmt_number(columns = c(Score_1,Score_2),decimals = 3) %>%
  cols_label(img = "") %>%
  gt_img_rows(columns = img, height = 25, img_source = "local") %>%
  tab_caption("Average scores without weighting:") %>%
  tab_options(table.background.color = "transparent",
              table.font.size = 17,
              data_row.padding = px(2),
              table.width = 300) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )



resources_comp <- resources_axis_1-resources_axis_2


palette <- colorNumeric(palette= color_pal,
             domain = resources_comp["norm_score"][[1]],
             na.color = "transparent",
             reverse = TRUE)
  
  ### Process for Raster data
  leaflet(options = leafletOptions(minZoom = 9)) %>% addTiles() %>%
    addGeoRaster(resources_comp["norm_score"],
                 opacity = 0.9,
                 colorOptions =leafem:::colorOptions(
                   palette = "RdBu",
                   domain = c(-1*value, 0, value),
                   breaks = seq(min(resources_comp["norm_score"][[1]], na.rm = TRUE),
                                max(resources_comp["norm_score"][[1]], na.rm = TRUE),
                                length.out=100),
                   na.color = "transparent"
                 ),
                 resolution=10000) %>%
    addProviderTiles(providers$Stamen.Terrain) %>%
    addLegendNumeric(pal = color_pal,
              values = resources_comp["norm_score"][[1]],
              position = "bottomright",
              tickLength = 2
              ) %>% 
    addLegend(pal = color_pal,
              values = resources_comp["norm_score"][[1]],
              position = "bottomright",
              labFormat = function(type, cuts, p) {  # Here's the trick
                paste0(c("Less Dense", "zero", "More Dense"))
              })
  
  
  # define the maximum and minimum values
  max_value <- max(resources_comp["norm_score"][[1]],na.rm = T)
  min_value <- min(resources_comp["norm_score"][[1]],na.rm = T)
  
  value <- max(abs(min_value), abs(max_value))
  
  color_pal <- colorNumeric(
    palette = "RdBu", 
    domain = c(-1*value, 0, value),
    na.color = "transparent"
  )
  
  # define the color palette
  color_pal <- colorNumeric(palette = c("red","white","blue"), domain = c(min_value, 0 , max_value),na.color = "transparent")
  


  