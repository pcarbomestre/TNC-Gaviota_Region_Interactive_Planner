library(shiny)
library(leaflet)
library(leaflet.extras)
library(utils)

sh <- data.frame()

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10, width = 300,
                  style = "padding: 8px",
                  actionButton("printShapes", h5(strong("Generate Drawing File")))
    )
)

server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "Default", attribution = 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors') %>%
            setView(lng = -98, lat = 30, zoom = 4) %>%
            addDrawToolbar(targetGroup = "draw", position = "topleft", editOptions = editToolbarOptions(edit=TRUE))
    })
    
    # Generate Shape List Action Button
    observeEvent(input$printShapes, {
        shapedf <- data.frame()
        reactive(shapedf)
        shapedf <-input$mymap_draw_all_features
        sh <<- as.data.frame(shapedf)
        
        # sh <- t(sh) # This is easier to read manually, but not for reading into R.
        shpwrite <- write.csv(sh,paste0("Drawings", ".csv"))
    })
    
    # Intake Shape CSV
    observeEvent(input$drawingFile, {
        drawFile <- input$drawingFile
        ext <- file_ext(drawFile$datapath)
        req(drawFile)
        validate(need(ext == "csv", "Please upload a csv file."))
        
        ddf <- read.csv(drawFile$datapath, header = TRUE) # The drawing dataframe
        
        ind <- which(ddf == "Feature") # Index for drawing df to break up the df to redraw the shapes.
        ind <- as.array(ind)
        
        for (i in 1:nrow(ind)) {
            if(i != nrow(ind)) thisShape <- ddf[ind[i]:ind[i+1]]
            else thisShape <- ddf[ind[i]:ncol(ddf)]
            
            #####
            if(thisShape[3] == "polyline") {
                tf <- array(startsWith(names(thisShape),"features.geometry.coordinates"))
                w <- 1
                pnts <- array()
                for (i in 1:nrow(tf)) {
                    if(tf[i] == TRUE) {
                        pnts[w] <- thisShape[i]
                        w <- w+1
                    }
                }
                n <- 1
                m <- 1
                plng <- array()
                plat <- array()
                pnts <- as.array(pnts)
                for (j in 1:nrow(pnts)) {
                    if(j %% 2 == 1) {
                        plng[n] <- pnts[j]
                        n <- n+1
                    }
                    else if(j %% 2 == 0) {
                        plat[m] <- pnts[j]
                        m <- m+1
                    }
                }
                as.vector(plng, mode = "any")
                as.vector(plat, mode = "any")
                PL <- data.frame(matrix(unlist(plng)))
                PLsub <- data.frame(matrix(unlist(plat)))
                PL <- cbind(PL, PLsub)
                colnames(PL) <- c("lng","lat")
                PL1 <- reactiveVal(PL)
                
                proxy <- leafletProxy("mymap", data = PL1())
                proxy %>% addPolylines(lng = ~lng, lat = ~lat, group = "draw")
            }
            #####
            else if(thisShape[3] == "polygon") {
                tf <- array(startsWith(names(thisShape),"features.geometry.coordinates"))
                w <- 1
                pnts <- array()
                for (i in 1:nrow(tf)) {
                    if(tf[i] == TRUE) {
                        pnts[w] <- thisShape[i]
                        w <- w+1
                    }
                }
                n <- 1
                m <- 1
                plng <- array()
                plat <- array()
                pnts <- as.array(pnts)
                for (j in 1:nrow(pnts)) {
                    if(j %% 2 == 1) {
                        plng[n] <- pnts[j]
                        n <- n+1
                    }
                    else if(j %% 2 == 0) {
                        plat[m] <- pnts[j]
                        m <- m+1
                    }
                }
                as.vector(plng, mode = "any")
                as.vector(plat, mode = "any")
                PG <- data.frame(matrix(unlist(plng)))
                PGsub <- data.frame(matrix(unlist(plat)))
                PG <- cbind(PG, PGsub)
                colnames(PG) <- c("lng","lat")
                PG1 <- reactiveVal(PG)
                
                proxy <- leafletProxy("mymap", data = PG1())
                proxy %>% addPolygons(lng = ~lng, lat = ~lat, group = "draw")
            }
            #####
            else if(thisShape[3] == "rectangle"){
                rlng1 <- as.numeric(thisShape[5])
                rlat1 <- as.numeric(thisShape[6])
                rlng2 <- as.numeric(thisShape[9])
                rlat2 <- as.numeric(thisShape[10])
                
                proxy <- leafletProxy("mymap")
                proxy %>% addRectangles(lng1 = rlng1, lat1 = rlat1, lng2 = rlng2, lat2 = rlat2,
                                        group = "draw")
            }
            #####
            else if(thisShape[3] == "circle"){
                crad <- as.numeric(thisShape[4])
                clng <- as.numeric(thisShape[6])
                clat <- as.numeric(thisShape[7])
                
                proxy <- leafletProxy("mymap")
                proxy %>% addCircles(lng = clng, lat = clat, radius = crad, group = "draw")
            }
            #####
            else if(thisShape[3] == "marker") {
                mlng <- as.numeric(thisShape[5])
                mlat <- as.numeric(thisShape[6])
                
                proxy <- leafletProxy("mymap")
                proxy %>% addMarkers(lng = mlng, lat = mlat, group = "draw")
            }
            #####
            else if(thisShape[3] == "circlemarker") {
                cmlng <- as.numeric(thisShape[6])
                cmlat <- as.numeric(thisShape[7])
                
                proxy <- leafletProxy("mymap")
                proxy %>% addCircleMarkers(lng = cmlng, lat = cmlat, group = "draw")
            }
        }
    })
}

shinyApp(ui = ui, server = server)