## ---------------------------
## Script name: pre-Fuzzy layers creation
##
## Purpose of script: 
##  Create the input_reporting_units.shp to feed the EEMS model
##
## Author: Pol Carbó Mestre
##
## Date Created: 2022-11-09
##
## Email: pcarbomestre@bren.ucsb.edu
## ---------------------------
## Notes:
##  Original tiff transformation into a single spatial vector
##  storing wrangled data as attributes.
##  DO NOT execute the script at once. Run section by section.
## ---------------------------
##

# ----------
# SETUP

# Required packages:
library(rstudioapi)                                
library(tidyverse)
library(raster)
library(sf)
library(stars)

# Set working directory to source file:
setwd(dirname(getActiveDocumentContext()$path)) 

# Read the .shp with the study area and quadrants of interest:
study_area_template <- st_read('../../base_features/study_area_template/study_area_template.shp')

# Set directory of the output file input_report_unit.shp 
input_report_unit_dir <- 'EEMS_modelization/input_reporting_units_for_droughts/input_reporting_units_for_droughts.shp'

### Create a blank input_report_unit.shp
# input_report_unit <- st_read(input_report_unit_dir) %>% 
#   dplyr::select(OBJECTID, geometry) %>% 
#   st_write(input_report_unit_dir,
#            delete_dsn = TRUE)

# ----------
# RECHARGE
## Read in data
library(here)

rch_proj <- raster(here("recharge","tiff","historical_recharge","rch_proj.tif"))
rch_miroc_proj <- raster(here("recharge","tiff","recharge_hot_scenario","rch_miroc_proj.tif"))
rch_ccsm4_proj <- raster(here("recharge","tiff","recharge_warm_scenario","rch_ccsm4_proj.tif"))

## Projected change calculation:
rch_hot_diff <- rch_miroc_proj-rch_proj # Difference in recharge between historic and hot projections
rch_warm_diff <- rch_ccsm4_proj-rch_proj # Difference in recharge between historic and hot projections

### Negative values in the new object represent 
### The higher values the greater the difference the projected recharge is from what the area experienced in the past.
### The negative values represent less recharge in the future.

## Averaging changes of both scenarios:
mean_rch_diff <- (rch_hot_diff+rch_warm_diff)/2 # Mean recharge values between scenarios

## Interpolate NA to closest pixel value:

#### install.packages("remotes")
#### remotes::install_github("JanCaha/r_package_qgis")
library(qgis)

tf_dir <- gdal_fillnodata(INPUT = mean_rch_diff,
                          BAND = 1,
                          DISTANCE = 10,
                          OUTPUT = here("temporary","temporary_file.tiff")) # Apply QGIS GDAL No Fill function

no_fill_tif <- raster(tf_dir$OUTPUT[1]) %>% 
  projectRaster(crs = CRS("EPSG:2229")) 

## Zonal statistics
### Remove previous rch_diff_m values
input_report_unit <- st_read(input_report_unit_dir) %>% 
  dplyr::select(-rch_diff_m) %>% 
  st_write(input_report_unit_dir,
           delete_dsn = TRUE)

### The zonal statistics outputs are stored as a new attribute in the imput file
qgis_zonalstatistics(INPUT_RASTER = no_fill_tif,
                      RASTER_BAND = 1,
                      INPUT_VECTOR = input_report_unit_dir,
                      COLUMN_PREFIX = "rch_diff_",
                      STATISTICS = "Mean")


# ----------
# PRECIPITATION
## Read in data
ppt_hot_diff <- raster(here("precipitation","tiff","precipitation_change_hot_scenario","ppt_hot_diff.tif"))
ppt_warm_diff <- raster(here("precipitation","tiff","precipitation_change_warm_scenario","ppt_warm_diff.tif")) 

### This layers are already representing changes in precipitation.
### Negative values in the new object represent 
### The higher values the greater the difference the projected precipitation is from what the area experienced in the past.
### The negative values represent less precipitation in the future.

## Averaging changes of both scenarios:
mean_ppt_diff <- (ppt_hot_diff+ppt_warm_diff)/2 # Mean precipitation values between scenarios


## Interpolate NA to closest pixel value:
tf_dir <- gdal_fillnodata(INPUT = mean_ppt_diff,
                          BAND = 1,
                          DISTANCE = 10,
                          OUTPUT = here("temporary","temporary_file.tiff")) # Apply QGIS GDAL No Fill function

no_fill_tif <- raster(tf_dir$OUTPUT[1]) %>% 
  projectRaster(crs = CRS("EPSG:2229")) 

## Zonal statistics
### Remove previous ppt_diff_m values
input_report_unit <- st_read(input_report_unit_dir) %>% 
  dplyr::select(-ppt_diff_m) %>% 
  st_write(input_report_unit_dir,
           delete_dsn = TRUE)

### The zonal statistics outputs are stored as a new attribute in the imput file
qgis_zonalstatistics(INPUT_RASTER = no_fill_tif,
                     RASTER_BAND = 1,
                     INPUT_VECTOR = input_report_unit_dir,
                     COLUMN_PREFIX = "ppt_diff_",
                     STATISTICS = "Mean")


# ----------
# WATER DEFICID
## Read in data
wd_hot_diff <- raster(here("water_deficid","tiff","cwd_hot_diff","cwd_hot_diff.tif"))
wd_warm_diff <- raster(here("water_deficid","tiff","cwd_warm_diff","cwd_warm_diff.tif"))

### This layers are already representing changes in precipitation.
### Negative values in the new object represent 
### The higher values the greater the difference the projected water deficit is from what the area experienced in the past.
### The negative values represent less recharge in the future.

## Averaging changes of both scenarios:
mean_wd_diff <- (wd_hot_diff+wd_warm_diff)/2 # Mean water deficit values between scenarios

## Interpolate NA to closest pixel value:
tf_dir <- gdal_fillnodata(INPUT = mean_wd_diff,
                          BAND = 1,
                          DISTANCE = 10,
                          OUTPUT = here("temporary","temporary_file.tiff")) # Apply QGIS GDAL No Fill function

no_fill_tif <- raster(tf_dir$OUTPUT[1]) %>% 
  projectRaster(crs = CRS("EPSG:2229")) 


## Zonal statistics:
### Remove previous wd_diff__m values
input_report_unit <- st_read(input_report_unit_dir) %>% 
  dplyr::select(-wd_diff_me) %>% 
  st_write(input_report_unit_dir,
           delete_dsn = TRUE)

### The zonal statistics outputs are stored as a new attribute in the imput file
qgis_zonalstatistics(INPUT_RASTER = no_fill_tif,
                     RASTER_BAND = 1,
                     INPUT_VECTOR = 'EEMS_modelization/input_reporting_units_for_droughts/input_reporting_units_for_droughts.shp',
                     COLUMN_PREFIX = "wd_diff_",
                     STATISTICS = "Mean")





#### DRAFT CODE ####

# # WATER DEFICID
# cwd_hot_diff <- st_read('shp/cwd_hot_diff/cwd_hot_diff.shp') %>% 
#   st_set_crs(st_crs(study_area_template))
# cwd_warm_diff <- st_read('shp/cwd_warm_diff/cwd_warm_diff.shp') %>% 
#   st_set_crs(st_crs(study_area_template))
# 
# # Join variables of interest from the .shp into one single sf object
# cwd_diff <- st_join(cwd_hot_diff, cwd_warm_diff, 
#                     join = st_equals, left = TRUE,
#                     suffix = c("_hot_diff", "_warm_diff")) %>% 
#   dplyr::select(-OBJECTID_warm_diff)
# 
# colnames(cwd_diff) <- c("ID", "hot_diff", "warm_diff", "geometry")
# 
# # Safe as .shp
# st_write(cwd_diff, "shp/cwd_all_diff/cwd_diff.shp")

## Data and package testing
# 
# plantilla_sf <- st_read('mba_cbi_shp/mba_cbi_shp.shp')
# cwd_hot_diff <- raster('cwd_hot_diff/cwd_hot_diff1.tif')
# cwd_warm_diff <- raster('cwd_warm_diff/cwd_warm_diff1.tif')
# 
# test <- extract(x = cwd_hot_diff, y = plantilla_sf)
# 
# cwd_diff <- plantilla_sf %>% 
#   mutate(cwd_hot_diff = extract(cwd_hot_diff, plantilla_sf, fun = mean, na.rm = TRUE),
#          cwd_hot_diff = raster_extract(cwd_hot_diff, plantilla_sf, fun = mean, na.rm = TRUE)
#   )
# 
# cwd_hot_diff_sf <-  cwd_hot_diff %>% 
#   rasterToPolygons() %>% 
#   st_as_sf()
# 
# cwd_warm_diff_sf <-  cwd_warm_diff %>% 
#   rasterToPolygons() %>% 
#   st_as_sf()
# 
# 
# ggplot(data = cwd_hot_diff_sf) +
#   geom_sf(aes(fill=cwd_hot_diff1),lwd = 0)
# 
# cwd_diff <- st_join(cwd_hot_diff_sf, cwd_warm_diff_sf, join = st_equals)
# st_write(cwd_diff, "my_cwd_diff_shapefile.shp")
