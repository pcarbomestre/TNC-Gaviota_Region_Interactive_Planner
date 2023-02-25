## ---------------------------
### File: global.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## ---------------------------
## Description:
##  Global functions to load packages, datasets and define functions
## ---------------------------
##

# PACKAGES SETUP
# ----------------------
# Used packages
packs = c("tidyverse",
          "tmap",
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
          "rmapshaper",
          "leaflet",
          "bslib",
          "shinyWidgets",
          "stars",
          "tmaptools",
          "leaflet.extras",
          "leafem",
          "plotly"
          )



# Run the following command to verify that the required packages are installed. 
# If some package is missing, it will be installed automatically
package.check <- lapply(packs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

library(shiny)


# DATASETS READ IN
# ----------------------

# Natural Resources Axis spatial data
resources_axis_sf <- st_read(here('data','SHP_Multi-benefit_Areas_Map','0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp')) %>% 
  clean_names() %>% 
  st_transform("EPSG:4326") # Reproject so it can be visualized using leaflet

resources_axis_r <- st_rasterize(resources_axis_sf %>% dplyr::select(agricultur,
                                                                      community,
                                                                      flora_faun,
                                                                      water_raw, 
                                                                      eems_synth, 
                                                                      geometry)) # Rasterizing shapefile


resources_axis_df <- st_drop_geometry(resources_axis_sf)

# AHP weights
ahp_weights <- read_csv2(here('data','ahp_weights.csv')) %>%
  clean_names()

# Environmental Threats Axis spatial data
threats_axis_sf <- st_read(here('data','small_MB_area','small_MB_area.shp')) %>% 
  clean_names()

threats_axis_df <- st_drop_geometry(threats_axis_sf)

# Equity Issues Axis spatial data
equity_axis_sf <- st_read(here('data','small_MB_area','small_MB_area.shp')) %>% 
  clean_names()

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

# Custom theme
my_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = " #598048 ",
  primary = "#EC255A",
  base_font = font_google('Avenir')
)
