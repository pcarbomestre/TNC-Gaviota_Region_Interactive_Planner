### File: Loading spatial Data.R
###
### Template created: 17/01/2020	13:59
### Author: Guillermo Martin
###
####################################################################################################
### Description:
###
### Load required spatial data
###   
###

# Usual projections -------------------------------------------------------

projWGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Load spatial data -------------------------------------------------------
spatialdir<-file.path("./data/geoData")

ICES_rectangles<- readOGR(dsn = spatialdir,
                          layer = "ICES_Statistical_Rectangles_Eco",
                          encoding = "utf8")
ICES_rectangles <- spTransform(ICES_rectangles, CRSobj=projWGS84)

Inshore_Grid<- readOGR(dsn = spatialdir,
                          layer = "Inshore_grid_NEW",
                          encoding = "utf8")
Inshore_Grid <- spTransform(Inshore_Grid, CRSobj=projWGS84)
#coastline<-readOGR(dsn = spatialdir,
#                   layer = "Ireland",
#                   encoding = "utf8")
#coastline <- spTransform(coastline, CRSobj=projWGS84)

#coastline$id = rownames(coastline)
#coastline.points = fortify(coastline, region="id")
#coastline.df = st_join(coastline.points, coastline, by="id")

#Counties<- readOGR(dsn = spatialdir,
#                   layer = "County_Shapefile",
#                   encoding = "utf8")
#Counties <- spTransform(Counties, CRSobj=projWGS84)

#UK<-readOGR(dsn = spatialdir,
#            layer = "greatbritain",
#            encoding = "utf8")
#UK <- spTransform(UK, CRSobj=projWGS84)


#Scotland<-readOGR(dsn = spatialdir,
#            layer = "scotland",
#            encoding = "utf8")
#Scotland <- spTransform(Scotland, CRSobj=projWGS84)


#Wales<-readOGR(dsn = spatialdir,
#                  layer = "wales",
#               encoding = "utf8")
#Wales <- spTransform(Wales, CRSobj=projWGS84)

