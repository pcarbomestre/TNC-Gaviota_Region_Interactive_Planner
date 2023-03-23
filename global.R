## _____________________________
### File: global.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## _____________________________
## Description:
##  Global functions to load packages, datasets and define functions
## _____________________________


# PACKAGES SETUP
# ----------------------
# Used packages
packs = c("tidyverse",
          "shinydashboard",
          "shinythemes",
          "shinyjs",
          "shinyWidgets",
          "rintrojs",
          "sf",
          "here",
          "janitor",
          "caret",
          "shiny",
          "leaflet",
          "bslib",
          "shinyWidgets",
          "stars",
          "leaflet.extras",
          "leafem",
          "plotly",
          "kableExtra",
          "rgeos",
          "shinyalert",
          "flexdashboard",
          "htmltools",
          "gt",
          "gtExtras",
          "RColorBrewer"
          )

# Run the following command to verify that the required packages are installed. 
# If some package is missing, it will be installed automatically
package.check <- lapply(packs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

library(shiny)


# DATASETS READ IN ----------------------

## Natural Resources Axis spatial data ----
resources_axis_sf <- st_read(here('data','shiny_inputs','natural_resources','natural_resources.shp')) %>% 
  clean_names() %>% 
  st_transform("EPSG:4326") # Reproject so it can be visualized using leaflet

resources_axis_r <- st_rasterize(resources_axis_sf %>% dplyr::select(-id)) # Rasterizing shapefile

resources_axis_df <- st_drop_geometry(resources_axis_sf)

### AHP weights ----
ahp_weights <- read_csv2(here('data','ahp_results.csv')) %>%
  clean_names() %>% 
  mutate(agg_pref = as.numeric(agg_pref)) %>% 
  group_by(group) %>% 
  mutate(weight = round(100*(agg_pref)/max(agg_pref),0))


## Environmental Threats Axis spatial data ----
threats_axis_sf <- st_read(here('data','shiny_inputs','environmental_threats','environmental_threats.shp')) %>% 
  clean_names() %>% 
  st_transform("EPSG:4326") # Reproject so it can be visualized using leaflet

threats_axis_r <- st_rasterize(threats_axis_sf %>% dplyr::select(-id)) # Rasterizing shapefile

threats_axis_df <- st_drop_geometry(threats_axis_sf)


# Equity Issues Axis spatial data
equity_axis_sf <- st_read(here('data','shiny_inputs','equity_issues','equity_issues.shp')) %>% 
  clean_names() %>% 
  st_transform("EPSG:4326")

equity_axis_r <- st_rasterize(equity_axis_sf %>% dplyr::select(-id)) # Rasterizing shapefile

equity_axis_df <- st_drop_geometry(equity_axis_sf)

# Dangermond Preserve boundaries
dp_boundaries_sf <- st_read(here('data','jldp_boundary','jldp_boundary.shp')) %>%
  clean_names()

dp_boundaries_df <- st_drop_geometry(dp_boundaries_sf)


# FUNCTIONS
# ----------------------
## normalizing data using caret range method
range_norm <- function(new_score) {
  ss <- preProcess(as.data.frame(new_score), method=c("range"))
  gfg <- predict(ss, as.data.frame(new_score))
  return(gfg)
}

range_norm_manual <- function(new_score) {
  (new_score - min(new_score,na.rm = TRUE)) / (max(new_score,na.rm = TRUE) - min(new_score,na.rm = TRUE))
}

#Additional functions and Shinny formatting
source("./lib/help_funs.R",
       encoding="latin1")
source("./lib/override.R", 
       local = TRUE)

# Intro steps
intro<-read.csv("./data/intro.csv")


# OTHER PARAMETERS
# ----------------------
# Map setup
pal <- colorNumeric("Greens", threats_axis_sf$eems_synth) # pallete for default leaflet

map_bounds <- st_bbox(resources_axis_sf) %>% 
  as.vector()

leaflet() %>% 
  addTiles() %>% 
  setMaxBounds(lng1=map_bounds[1], lat1= map_bounds[2], lng2=map_bounds[3], lat2= map_bounds[4])


leaflet(options = leafletOptions(minZoom = 11)) %>%
  addProviderTiles("OpenStreetMap") %>%
  setView( lng = -87.567215
           , lat = 41.822582
           , zoom = 11 ) %>%
  setMaxBounds( lng1 = -120.8998
                , lat1 = 34.27348
                , lng2 = -119.1025
                , lat2 = 35.31413 ) %>% 
  addDrawToolbar(targetGroup = "draw",
                 polylineOptions = FALSE,
                 circleOptions = FALSE,
                 markerOptions = FALSE,
                 circleMarkerOptions = FALSE,
                 editOptions = editToolbarOptions(
                   selectedPathOptions = selectedPathOptions()),
                 position = "topright",
                 singleFeature = TRUE)

# Custom theme
my_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = " #598048 ",
  primary = "#EC255A",
  base_font = font_google('Avenir')
)
