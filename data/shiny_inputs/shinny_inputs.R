## ---------------------------
## Script name: Shiny app input vector
##
## Purpose of script: 
##  Combine the EEMS output into a single .shp
##
## Author: Pol Carbó Mestre
##
## Date Created: 2023-02-27
##
## Email: pcarbomestre@bren.ucsb.edu
## ---------------------------
## Notes:
##  Environmental threats data
## ---------------------------

# Required packages:
library(rstudioapi)                                
library(tidyverse)
library(sf)
library(here)
library(caret)

# Set working directory to source file:
setwd(dirname(getActiveDocumentContext()$path))

# NATURAL RESOURCES:

EEMS_water <- st_read(here("..","resources","water & bio","SHP_Multi-benefit_Areas_Map",
                           "0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp")) %>% 
  mutate(water_FZ = Water_Raw) %>% 
  select(OBJECTID, water_FZ, geometry)

EEMS_bio <- st_read(here("..","resources","water & bio","SHP_Multi-benefit_Areas_Map",
                           "0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp")) %>% 
  mutate(bio_FZ = Flora_Faun) %>% 
  select(OBJECTID, bio_FZ, geometry)

EEMS_soil <- st_read(here("..","resources","soil","output","soil_layer","soil_layer.shp")) %>% 
  select(OBJECTID, soil_FZ, geometry)

EEMS_resc <- st_read(here("..","resources","resilience","EEMS_modelization","EEMS_resilience_output","EEMS_resilience.shp"))


range_norm_manual <- function(new_score) {
  (new_score - min(new_score)) / (max(new_score) - min(new_score))
}

# Check if ID and geometry are equal among sf objects
# summary(EEMS_water$OBJECTID == EEMS_soil$OBJECTID)
# summary(EEMS_water$geometry == EEMS_soil$geometry)
# If all TRUE, proceed

nat_resources_layer <- EEMS_water %>% 
  mutate(ID=OBJECTID, .before=OBJECTID) %>% # updating ID name to match 
  select(-OBJECTID) %>% # Keep only fuzzy values
  left_join(EEMS_bio %>% 
              as.data.frame() %>% 
              mutate(ID=OBJECTID) %>% # updating ID name to match 
              select(ID, bio_FZ, geometry),
            by=c("ID","geometry")) %>% 
  left_join(EEMS_soil %>% 
              as.data.frame() %>% 
              mutate(ID=OBJECTID) %>% # updating ID name to match 
              select(ID, soil_FZ, geometry),
            by=c("ID","geometry")) %>% 
  left_join(EEMS_resc %>% 
              as.data.frame() %>% 
              select(ID, resil_FZ, geometry),
            by=c("ID","geometry")) %>% 
  mutate(agg_val = range_norm_manual((water_FZ+bio_FZ+soil_FZ+resil_FZ)/4), .before= geometry)


# Save .shp
nat_resources_layer %>% 
  st_write(here("natural_resources"),
           delete_dsn = TRUE,
           driver = "ESRI Shapefile")

# ENVIRONMENTAL THREATS:

EEMS_climate <- st_read(here("..","threats3.0","climate","EEMS_modelization","R_processing","EEMS_climate_output","EEMS_climate.shp"))
EEMS_droughts <- st_read(here("..","threats3.0","droughts","EEMS_modelization","R_processing","EEMS_droughts_output","EEMS_droughts.shp"))
EEMS_floods <- st_read(here("..","threats3.0","floods","EEMS_modelization","R_processing","EEMS_floods_output","EEMS_floods.shp"))
EEMS_wildfires <- st_read(here("..","threats3.0","wildfires","EEMS_modelization","R_processing","EEMS_wildfires_output","EEMS_wildfires.shp"))

# Check if ID and geometry are equal among sf objects
# summary(EEMS_climate$ID == EEMS_droughts$ID)
# summary(EEMS_climate$geometry == EEMS_droughts$geometry)
# If all TRUE, proceed

env_threats_layer <- EEMS_climate %>% 
  select(-cli_exp_me) %>% # Keep only fuzzy values
  left_join(EEMS_droughts %>% 
              as.data.frame() %>% 
              select(ID, drgh_h_FZ, drgh_w_FZ, drgh_m_FZ, geometry),
            by=c("ID","geometry")) %>% 
  left_join(EEMS_floods %>% 
              as.data.frame() %>% 
              mutate(ID=OBJECTID) %>% # updating ID name to match 
              select(ID, fld_h_FZ, fld_w_FZ, fld_m_FZ, geometry),
            by=c("ID","geometry")) %>% 
  left_join(EEMS_wildfires %>% 
              as.data.frame() %>% 
              select(ID, wf_cn_FZ, wf_mi_FZ, wf_m_FZ, geometry),
            by= c("ID","geometry")) %>% 
  mutate(agg_val = range_norm_manual((cli_exp_FZ+drgh_m_FZ+fld_m_FZ+wf_m_FZ)/4), .before= geometry)

# Save .shp
env_threats_layer %>% 
  st_write(here("environmental_threats"),
           delete_dsn = TRUE,
           driver = "ESRI Shapefile")


# EQUITY ISSUES

EEMS_equity <- st_read(here("..","equity","EEMS_equity_output","EEMS_equity.shp")) %>% 
  select(-fid) %>% 
  rename(pollution_raw = Pollution_,
         demographics_raw =  Pop__Chara,
         nat_access_raw = utp_mean,
         ID = OBJECTID)

# Convert to fuzzy

## Defining function to normalize data using caret range method
range_norm <- function(new_score) {
  ss <- preProcess(as.data.frame(new_score), method=c("range"))
  gfg <- predict(ss, as.data.frame(new_score))
  return(gfg)
}


equity_layer <- EEMS_equity %>% 
  mutate(pol_FZ = range_norm(pollution_raw)[,1],
         demo_FZ = range_norm(demographics_raw)[,1],
         access_FZ = range_norm(nat_access_raw)[,1],.before = geometry) %>% 
  select(ID, pol_FZ, demo_FZ, access_FZ, geometry) %>% 
  mutate(agg_val = range_norm_manual((pol_FZ+demo_FZ+access_FZ)/4), .before= geometry)

# Save .shp
equity_layer %>% 
  st_write(here("equity_issues"),
           delete_dsn = TRUE,
           driver = "ESRI Shapefile")


