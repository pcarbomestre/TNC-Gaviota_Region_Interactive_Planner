### File: global.R
###
### Template Created: 14/07/2020	08:55:00
### Author: Guillermo Martin
###
####################################################################################################
### Description:
###
### Global functions to load packages and Inshore database data
###   
###

# Used packages
packs = c("shiny",
          "shinyBS",
          "shinydashboard", 
          "shinythemes",
          "shinyjs",
          "shinyWidgets",
          "leaflet",
          "dplyr",
          "tidyr",
          "ggplot2",
          "lubridate", 
          "plotly",
          "sp",
          "sf",
          "rgdal",
          "shinycssloaders",
          "ggsci",
          "colorRamps",
          "ggrepel",
          "rintrojs",
          "leafem",
          "caret"
          )

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})
library(shiny)

# Intro steps
intro<-read.csv("./data/intro.csv")

##Loading Inshore fisheries data:
catch_plot<-read.csv("./data/Data_prep_Output/catch_plot.csv")

#Biological data
bio<-read.csv("data/Data_prep_Output/bio_summary.csv")

#Summary statistics
dat_sta<-read.csv("data/Data_prep_Output/Programme_summary.csv")
minY<-as.numeric(min(dat_sta$Year))
maxY<-2021#as.numeric(max(dat_sta$Year))

#Landings table
landings<-read.csv("data/Data_prep_Output/Landings_Table_2022.csv")
landings <- landings %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "Landings") %>%
  mutate(Year=gsub("x","",Year,ignore.case = T)) %>%
  data.frame()

#Assessment and advice table
a_a<-read.csv("data/Data_prep_Output/Assessment and Advice.csv")

#Loading Spatial Data
source("./lib/01_Loading_Spatial_Data.R",
       encoding="latin1")

#Additional functions and Shinny formatting
source("./lib/help_funs.R",
       encoding="latin1")
source("./lib/override.R", 
       local = TRUE)

#Load data at ICES_rectangle
ICES_LPUE<-readOGR(dsn = "data/Data_prep_Output",
                   layer = "ICES_LPUE",
                   encoding = "utf8")
ICES_LPUE <- spTransform(ICES_LPUE, CRSobj=projWGS84)
ICES_LPUE<-st_as_sf(ICES_LPUE)


#Read rds file related to the polygons
ICES_dat<-readRDS(file=file.path("data/Data_prep_Output/ICES_slope_data.rds"))
ICES_dat<-subset(ICES_dat,Year <= maxY)
ICES_dat <- ICES_dat %>%
  group_by(Year) %>%
  arrange(EventStartDate) %>%
  ungroup() %>%
  mutate(units=ifelse(CommonName %in% "EDIBLE CRAB UNSEXED","kg/Pot","Number/100 Pots")) %>%
  data.frame()
