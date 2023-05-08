## _____________________________
### File: global.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## _____________________________
## Description:
##  Global functions to load packages, datasets and interactive tour
## _____________________________


# PACKAGES SETUP ----

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(here)
library(janitor)
library(rmapshaper)
library(tmaptools)
library(caret)
library(shiny)
library(leaflet)
library(bslib)
library(shinyWidgets)
library(stars)
library(leaflet.extras)
library(leafem)
library(plotly)
library(kableExtra)
library(rgeos)
library(shinyalert)
library(flexdashboard)
library(htmltools)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(conductor)
     
    
# DATA READ IN ----

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

## Equity Issues Axis spatial data ----
equity_axis_sf <- st_read(here('data','shiny_inputs','equity_issues','equity_issues.shp')) %>% 
  clean_names() %>% 
  st_transform("EPSG:4326")

equity_axis_r <- st_rasterize(equity_axis_sf %>% dplyr::select(-id)) # Rasterizing shapefile
equity_axis_df <- st_drop_geometry(equity_axis_sf)


# FUNCTIONS ----

range_norm <- function(new_score) {
  ss <- preProcess(as.data.frame(new_score), method=c("range"))
  gfg <- predict(ss, as.data.frame(new_score))
  return(gfg)
}

range_norm_manual <- function(new_score) {
  (new_score - min(new_score,na.rm = TRUE)) / (max(new_score,na.rm = TRUE) - min(new_score,na.rm = TRUE))
}


# INTRO and TOUR ----
# (using Conductor)
## Conductor for natural resources map ----
conductor_nr <- Conductor$
  new(tourName = "natural_resources",
    defaultStepOptions = list(
    cancelIcon = list(enabled = TRUE, NULL)
  ))$
  step(
    title = "Natural resources axis",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour1.html", width = "600px",  height = "460px",  style = "border:none;"),
    buttons = list(
      list(
        action = "next",
        text = "Next"
      )
    )
  )$
  step(
    el = ".panel_sliders",
    title = "Select aggregated preferences",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour2.html", width = "800px",  height = "400px",  style = "border:none;"),
  )$
  step(
    el = ".selectize-input",
    title = "Apply Stakeholder and Rightsholder weights",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour3.html", width = "850px",  height = "330px",  style = "border:none;"),
    position = "left-end"
    )$
  step(
    el = ".col-sm-7",
    title = "Information details",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour4.html", width = "650px",  height = "330px",  style = "border:none;"),
    position = "top-start"
  )$
  step(
    el = ".leaflet-draw-section",
    title = "Select your areas of interest",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour5.html", width = "380px",  height = "440px",  style = "border:none;"),
    position = "right-start"
    )$
  step(
    el = "#map",
    title = "Time to explore",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour6.html", width = "470px",  height = "130px",  style = "border:none;"),
  position = "bottom-start"
    )$
  step(
    el = ".btn",
    title = "End of tour",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour7.html", width = "380px",  height = "80px",  style = "border:none;"),
    position = "top-start",
     buttons = list(
      list(
        action = "back",
        secondary = TRUE,
        text = "Previous"
      ),
      list(
        action = "next",
        text = "Finish"
      )
    )
  )

## Conductor for stakeholders priorities map ----
conductor_s <- Conductor$
  new(tourName = "Stackeholders priorities",
      defaultStepOptions = list(
        cancelIcon = list(enabled = TRUE, NULL)
      ))$
  step(
    title = "Stakeholder and rightsholder priorities",
    text = htmltools::tags$iframe(src = "tour/stakeholders/tour1.html", width = "600px",  height = "500px",  style = "border:none;"),
    buttons = list(
      list(
        action = "next",
        text = "Next"
      )
    )
  )$
  step(
    el = ".ahp_inputs",
    title = "Select two groups",
    text = htmltools::tags$iframe(src = "tour/stakeholders/tour2.html", width = "380px",  height = "150px",  style = "border:none;"),
    position = "right-end"
    )$
  step(
    el = "#map_stake",
    title = "Interpretation",
    text = htmltools::tags$iframe(src = "tour/stakeholders/tour3.html", width = "1150px",  height = "130px",  style = "border:none; padding: 0 0 0 0"),
    position = "bottom-start"
  )$
  step(
    el = ".btn",
    title = "End of tour",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour7.html", width = "380px",  height = "80px",  style = "border:none;"),
    position = "top-start",
    buttons = list(
      list(
        action = "back",
        secondary = TRUE,
        text = "Previous"
      ),
      list(
        action = "next",
        text = "Finish"
      )
    )
  )

## Conductor for natural resources map ----
conductor_t <- Conductor$
  new(tourName = "threats",
      defaultStepOptions = list(
        cancelIcon = list(enabled = TRUE, NULL)
      ))$
  step(
    title = "Environmental threats axis",
    text = htmltools::tags$iframe(src = "tour/threats/tour1.html", width = "600px",  height = "530px",  style = "border:none;"),
    buttons = list(
      list(
        action = "next",
        text = "Next"
      )
    )
  )$
  step(
    el = ".panel.panel-default.draggable.ui-draggable.ui-draggable-handle",
    title = "Select aggregated preferences",
    text = htmltools::tags$iframe(src = "tour/threats/tour2.html", width = "800px",  height = "420px",  style = "border:none;"),
  )$
  step(
    el = ".col-sm-7.threats",
    title = "Information details",
    text = htmltools::tags$iframe(src = "tour/threats/tour3.html", width = "650px",  height = "300px",  style = "border:none;"),
    position = "top-start"
  )$
  step(
    el = "#map_threats",
    title = "Time to explore",
    text = htmltools::tags$iframe(src = "tour/threats/tour4.html", width = "800px",  height = "130px",  style = "border:none;"),
    position = "bottom-start"
  )$
  step(
    el = ".btn",
    title = "End of tour",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour7.html", width = "380px",  height = "80px",  style = "border:none;"),
    position = "top-start",
    buttons = list(
      list(
        action = "back",
        secondary = TRUE,
        text = "Previous"
      ),
      list(
        action = "next",
        text = "Finish"
      )
    )
  )

## Conductor for DEI/EJ map ----
conductor_d <- Conductor$
  new(tourName = "dei_ej",
      defaultStepOptions = list(
        cancelIcon = list(enabled = TRUE, NULL)
      ))$
  step(
    title = "DEI/EJ issues axis",
    text = htmltools::tags$iframe(src = "tour/dei_ej/tour1.html", width = "600px",  height = "570px",  style = "border:none;"),
    buttons = list(
      list(
        action = "next",
        text = "Next"
      )
    )
  )$
  step(
    el = ".ej.draggable.ui-draggable.ui-draggable-handle",
    title = "Select aggregated preferences",
    text = htmltools::tags$iframe(src = "tour/dei_ej/tour2.html", width = "800px",  height = "400px",  style = "border:none;"),
  )$
  step(
    el = ".col-sm-7.ej",
    title = "Information details",
    text = htmltools::tags$iframe(src = "tour/dei_ej/tour3.html", width = "650px",  height = "300px",  style = "border:none;"),
    position = "top-start"
  )$
  step(
    el = "#map_equity",
    title = "Time to explore",
    text = htmltools::tags$iframe(src = "tour/dei_ej/tour4.html", width = "800px",  height = "130px",  style = "border:none;"),
    position = "bottom-start"
  )$
  step(
    el = ".btn",
    title = "End of tour",
    text = htmltools::tags$iframe(src = "tour/natural_resources/tour7.html", width = "380px",  height = "80px",  style = "border:none;"),
    position = "top-start",
    buttons = list(
      list(
        action = "back",
        secondary = TRUE,
        text = "Previous"
      ),
      list(
        action = "next",
        text = "Finish"
      )
    )
  )
