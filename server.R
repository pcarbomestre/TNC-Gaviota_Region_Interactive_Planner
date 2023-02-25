## ---------------------------
### File: server.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## ---------------------------
## Description:
##  Server for the Interactive Planner app
## ---------------------------
##

server <- function(input, output, session) {

 
# Leaflet Map ------------------------------------------------------------------------

  # Default Map Display --------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    if (!input$run) {
      # default plot
      
      pal <- colorNumeric(palette= "Greens",
                          domain = resources_axis_r["eems_synth"][[1]],
                          na.color = "transparent") # pallete for default leaflet
      
      ## Process for Raster data --------------------
      leaflet() %>% addTiles() %>%
        addGeoRaster(resources_axis_r["eems_synth"], 
                     opacity = input$alpha,
                     colorOptions =leafem:::colorOptions(
                       palette = "Greens",
                       breaks = seq(min(resources_axis_r["eems_synth"][[1]], na.rm = TRUE),
                                    max(resources_axis_r["eems_synth"][[1]], na.rm = TRUE),
                                    100),
                       na.color = "transparent"
                     ),
                     resolution=10000) %>% 
        addProviderTiles(providers$Stamen.Terrain) %>% 
        addLegend(pal = pal,
                  values = resources_axis_r["eems_synth"][[1]],
                  position = "bottomright",
                  opacity = input$alpha) %>% 
        fitBounds(lng1=as.numeric(bb(resources_axis_r)[1]), 
                  lat1=as.numeric(bb(resources_axis_r)[2]),
                  lng2=as.numeric(bb(resources_axis_r)[3]),
                  lat2=as.numeric(bb(resources_axis_r)[4])) %>% 
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       editOptions = editToolbarOptions(
                         selectedPathOptions = selectedPathOptions()),
                       position = "topright",
                       singleFeature = TRUE) 
      
      ## Process for Vector data --------------------
      # leaflet(resources_axis_sf) %>% 
      #         setView(lng = -120.3, lat = 34.53, zoom =10 ) %>%
      #         addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
      #                 color = "grey",
      #                 fillColor = ~pal(eems_synth)) %>% 
      #     addProviderTiles(providers$Stamen.Terrain) %>% 
      #     addLegend("bottomright", pal = pal, values = ~resources_axis_sf$eems_synth,
      #               title = "Score",
      #               opacity = 1)
      
    }
  })
  
  # Display individual layers  ---------------------------------------------------------
  # ------------------------------------------------------------------------------------
  
  observeEvent(input$checkbox_water, {
    if (input$checkbox_water) {
      updateSliderInput(session, "water_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
    
    ## Leaflet display --------------------
    output$map <- renderLeaflet({
      pal <- colorNumeric(palette= "Greens",
                          domain = resources_axis_r["water_raw"][[1]],
                          na.color = "transparent") # pallete for default leaflet
      
      ### Process for Raster data --------------------
      leaflet() %>% addTiles() %>%
        addGeoRaster(resources_axis_r["water_raw"], 
                     opacity = input$alpha,
                     colorOptions =leafem:::colorOptions(
                       palette = "Greens",
                       breaks = seq(min(resources_axis_r["water_raw"][[1]], na.rm = TRUE),
                                    max(resources_axis_r["water_raw"][[1]], na.rm = TRUE),
                                    100),
                       na.color = "transparent"
                     ),
                     resolution=10000) %>% 
        addProviderTiles(providers$Stamen.Terrain) %>% 
        addLegend(pal = pal,
                  values = resources_axis_r["water_raw"][[1]],
                  position = "bottomright",
                  opacity = input$alpha) %>% 
        fitBounds(lng1=as.numeric(bb(resources_axis_r)[1]), 
                  lat1=as.numeric(bb(resources_axis_r)[2]),
                  lng2=as.numeric(bb(resources_axis_r)[3]),
                  lat2=as.numeric(bb(resources_axis_r)[4])) %>% 
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       editOptions = editToolbarOptions(
                         selectedPathOptions = selectedPathOptions()),
                       position = "topright",
                       singleFeature = TRUE) })
    
  })
  
  observeEvent(input$checkbox_agri, {
    if (input$checkbox_agri) {
      updateSliderInput(session, "agriculture_w", value = 5)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
    
    ## Leaflet display --------------------
    output$map <- renderLeaflet({
      pal <- colorNumeric(palette= "Greens",
                          domain = resources_axis_r["agricultur"][[1]],
                          na.color = "transparent") # pallete for default leaflet
    
    ### Process for Raster data --------------------
    leaflet() %>% addTiles() %>%
      addGeoRaster(resources_axis_r["agricultur"], 
                   opacity = input$alpha,
                   colorOptions =leafem:::colorOptions(
                     palette = "Greens",
                     breaks = seq(min(resources_axis_r["agricultur"][[1]], na.rm = TRUE),
                                  max(resources_axis_r["agricultur"][[1]], na.rm = TRUE),
                                  100),
                     na.color = "transparent"
                   ),
                   resolution=10000) %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addLegend(pal = pal,
                values = resources_axis_r["agricultur"][[1]],
                position = "bottomright",
                opacity = input$alpha) %>% 
      fitBounds(lng1=as.numeric(bb(resources_axis_r)[1]), 
                lat1=as.numeric(bb(resources_axis_r)[2]),
                lng2=as.numeric(bb(resources_axis_r)[3]),
                lat2=as.numeric(bb(resources_axis_r)[4])) %>% 
      addDrawToolbar(targetGroup = "draw",
                     polylineOptions = FALSE,
                     circleOptions = FALSE,
                     markerOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     editOptions = editToolbarOptions(
                       selectedPathOptions = selectedPathOptions()),
                     position = "topright",
                     singleFeature = TRUE) })
    })
    
  observeEvent(input$checkbox_bio, {
    if (input$checkbox_bio) {
      updateSliderInput(session, "biodiversity_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
    
    ## Leaflet display --------------------
    output$map <- renderLeaflet({
      pal <- colorNumeric(palette= "Greens",
                          domain = resources_axis_r["flora_faun"][[1]],
                          na.color = "transparent") # pallete for default leaflet
      
      ### Process for Raster data --------------------
      leaflet() %>% addTiles() %>%
        addGeoRaster(resources_axis_r["flora_faun"], 
                     opacity = input$alpha,
                     colorOptions =leafem:::colorOptions(
                       palette = "Greens",
                       breaks = seq(min(resources_axis_r["flora_faun"][[1]], na.rm = TRUE),
                                    max(resources_axis_r["flora_faun"][[1]], na.rm = TRUE),
                                    100),
                       na.color = "transparent"
                     ),
                     resolution=10000) %>% 
        addProviderTiles(providers$Stamen.Terrain) %>% 
        addLegend(pal = pal,
                  values = resources_axis_r["flora_faun"][[1]],
                  position = "bottomright",
                  opacity = input$alpha) %>% 
        fitBounds(lng1=as.numeric(bb(resources_axis_r)[1]), 
                  lat1=as.numeric(bb(resources_axis_r)[2]),
                  lng2=as.numeric(bb(resources_axis_r)[3]),
                  lat2=as.numeric(bb(resources_axis_r)[4])) %>% 
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       editOptions = editToolbarOptions(
                         selectedPathOptions = selectedPathOptions()),
                       position = "topright",
                       singleFeature = TRUE) })
  })
      
  observeEvent(input$checkbox_com, {
    if (input$checkbox_com) {
      updateSliderInput(session, "community_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
    }
    
    ## Leaflet display --------------------
    output$map <- renderLeaflet({
      pal <- colorNumeric(palette= "Greens",
                          domain = resources_axis_r["community"][[1]],
                          na.color = "transparent") # pallete for default leaflet
      
      ### Process for Raster data --------------------
      leaflet() %>% addTiles() %>%
        addGeoRaster(resources_axis_r["community"], 
                     opacity = input$alpha,
                     colorOptions =leafem:::colorOptions(
                       palette = "Greens",
                       breaks = seq(min(resources_axis_r["community"][[1]], na.rm = TRUE),
                                    max(resources_axis_r["community"][[1]], na.rm = TRUE),
                                    100),
                       na.color = "transparent"
                     ),
                     resolution=10000) %>% 
        addProviderTiles(providers$Stamen.Terrain) %>% 
        addLegend(pal = pal,
                  values = resources_axis_r["community"][[1]],
                  position = "bottomright",
                  opacity = input$alpha) %>% 
        fitBounds(lng1=as.numeric(bb(resources_axis_r)[1]), 
                  lat1=as.numeric(bb(resources_axis_r)[2]),
                  lng2=as.numeric(bb(resources_axis_r)[3]),
                  lat2=as.numeric(bb(resources_axis_r)[4])) %>% 
        addDrawToolbar(targetGroup = "draw",
                       polylineOptions = FALSE,
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       editOptions = editToolbarOptions(
                         selectedPathOptions = selectedPathOptions()),
                       position = "topright",
                       singleFeature = TRUE) })
  })

  # AHP weights  -----------------------------------------------------------------------
  # ------------------------------------------------------------------------------------
    
    # initialize the weights
    water_weights = reactiveValues(None=NULL,
                                   Government=3,
                                   NGO=6,
                                   Native=1,
                                   Private=5)
    
    # If selected element changes, then update the slider
    observeEvent(input$stakeholder_w, {
      selected_weight = water_weights[[input$stakeholder_w]]
      updateSliderInput(session, "water_w", value = selected_weight)
    })
    
    agri_weights = reactiveValues(None=NULL,
                                   Government=6,
                                   NGO=1,
                                   Native=4,
                                   Private=8)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = agri_weights[[input$stakeholder_w]]
      updateSliderInput(session, "agriculture_w", value = selected_weight)
    })
    
    bio_weights = reactiveValues(None=NULL,
                                  Government=9,
                                  NGO=5,
                                  Native=1,
                                  Private=5)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = bio_weights[[input$stakeholder_w]]
      updateSliderInput(session, "biodiversity_w", value = selected_weight)
    })
    
    com_weights = reactiveValues(None=NULL,
                                 Government=7,
                                 NGO=2,
                                 Native=5,
                                 Private=3)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = com_weights[[input$stakeholder_w]]
      updateSliderInput(session, "community_w", value = selected_weight)
    })
    
    
  # Free weights display ---------------------------------------------------------------
  # ------------------------------------------------------------------------------------
    
    weights_reactive <- eventReactive(input$run,{
        
        ## Process for Raster data --------------------
        resources_axis_r %>% 
            mutate("agricultur" = agricultur * input$agriculture_w) %>% 
            mutate("community" = community * input$community_w) %>% 
            mutate("flora_faun" = flora_faun * input$biodiversity_w) %>% 
            mutate("water_raw" = water_raw * input$water_w) %>% 
            mutate(score = agricultur + community + flora_faun + water_raw) %>% 
            mutate(norm_score = range_norm_manual(score))
        
        ## Process for Vector data --------------------
        # resources_axis_df %>%
        #     mutate_at(vars(agricultur), function(x) input$agriculture_w * x) %>% 
        #     mutate_at(vars(community), function(x) input$community_w * x) %>% 
        #     mutate_at(vars(flora_faun), function(x) input$biodiversity_w * x) %>% 
        #     mutate_at(vars(water_raw), function(x) input$water_w * x) %>% 
        #     mutate(score = agricultur+community+flora_faun+water_raw) %>% 
        #     mutate(norm_score = range_norm(score)[,1],.after = objectid) %>% 
        #     select(c(objectid,norm_score))
        
    }) # end weights_reactive
    
    
    pallete_reactive <- reactive({
        
        ## Process for Raster data --------------------
        colorNumeric(palette= "Greens",
                     domain = weights_reactive()["norm_score"][[1]],
                     na.color = "transparent")
        
        ## Process for Vector data --------------------
        # colorNumeric("Greens", weights_reactive()$norm_score) # pallete for leaflet
        
    }) # end pallete_reactive
    
    ## Process for Vector data --------------------
    # map_reactive <- reactive({
    # 
    #     left_join(resources_axis_sf, weights_reactive(), by = 'objectid')
    #     
    # }) # end map_reactive
    

    observeEvent(input$run,{
    output$map <- renderLeaflet({

        ## Process for Raster data --------------------
        map <- leaflet() %>% addTiles() %>%
            addGeoRaster(weights_reactive()["norm_score"], 
                         opacity = input$alpha,
                         colorOptions =leafem:::colorOptions(
                             palette = "Greens",
                             breaks = seq(min(weights_reactive()["norm_score"][[1]], na.rm = TRUE),
                                          max(weights_reactive()["norm_score"][[1]], na.rm = TRUE),
                                          100),
                             na.color = "transparent"
                         ),
                         resolution=10000) %>% 
            addProviderTiles(providers$Stamen.Terrain) %>% 
            addLegend(pal = pallete_reactive(),
                      values = weights_reactive()["norm_score"][[1]],
                      position = "bottomright",
                      opacity = input$alpha) %>% 
            fitBounds(lng1=as.numeric(bb(weights_reactive())[1]), 
                      lat1=as.numeric(bb(weights_reactive())[2]),
                      lng2=as.numeric(bb(weights_reactive())[3]),
                      lat2=as.numeric(bb(weights_reactive())[4])) %>% 
            addDrawToolbar(targetGroup = "draw",
                           polylineOptions = FALSE,
                           circleOptions = FALSE,
                           markerOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           editOptions = editToolbarOptions(
                               selectedPathOptions = selectedPathOptions()),
                           position = "topright",
                           singleFeature = TRUE) 
        
        ## Process for Raster data --------------------
        # map <-  leaflet(map_reactive()) %>% 
        #     setView(lng = -120.3, lat = 34.53, zoom =10 ) %>%
        #     addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
        #                 color = "grey",
        #                 fillColor = ~pallete_reactive()(norm_score)) %>% 
        #     addProviderTiles(providers$Stamen.Terrain) %>% 
        #     addLegend("bottomright", pal = pallete_reactive(), values = ~map_reactive()$norm_score,
        #               title = "Score",
        #               opacity = 1)
    
    })
    })
    
 
    
# Extract data from the map ---------------------------------------------------------------
# -----------------------------------------------------------------------------------------
    
    # Generate Shape List Action Button
    observeEvent(input$printShapes, {
        shapedf <- data.frame()
        reactive(shapedf)
        shapedf <-input$map_draw_all_features
        sh <- as.data.frame(shapedf)
        
        polygon <- sh %>% 
            dplyr::select(starts_with("features.geometry.coordinates")) %>% 
            as.numeric() %>% 
            matrix(ncol=2, byrow=TRUE) %>% 
            list() %>% 
            st_polygon() %>% 
            st_sfc(crs="EPSG:4326")
        
        
        if (!input$run) {
            
            df <- st_extract(resources_axis_r, polygon) %>%
                st_as_sf() %>%
                st_drop_geometry()
            
            output$mytable = renderTable({
                
                selected <- as.data.frame(df)
                rownames(selected) <- "Score"
                cbind(c("Agriculture",
                        "Community",
                        "Biodiversity",
                        "Water",
                        "Aggregated"), round(t(selected[-6]),3))
            })
            
            output$radar_graph <- renderPlotly({
                
                selected <- as.data.frame(df)
                
                fig <- plot_ly(
                    type = 'scatterpolar',
                    r =   as.numeric(selected[1:4]),
                    theta = c("Agriculture",
                              "Community",
                              "Biodiversity",
                                "Water"),
                    fill = 'toself',
                )

                fig <- fig %>%
                    layout(
                        polar = list(radialaxis = list(
                            visible = T,
                            range = c(0,1))
                            ),
                        plot_bgcolor  = "rgba(0, 0, 0, 0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0)",
                        fig_bgcolor   = "rgba(0, 0, 0, 0)",
                        showlegend = F
                    )
                fig
            })
        
            
        }
        
        else{
            
            df <- st_extract(resources_axis_r, polygon) %>%
                st_as_sf() %>%
                st_drop_geometry()
            
            df_aggregated <- st_extract(weights_reactive(), polygon) %>%
                st_as_sf() %>%
                st_drop_geometry()
            
            output$mytable = renderTable({
                
                selected <- cbind(as.data.frame(df[1:4]),
                                  as.data.frame(df_aggregated[7]))
                rownames(selected) <- "Score"
                cbind(c("Agriculture",
                        "Community",
                        "Biodiversity",
                        "Water",
                        "Aggregated (weighted)"), round(t(selected[-6]),3))
            })
            
            output$radar_graph <- renderPlotly({
                
                selected <- as.data.frame(df)
                
                fig <- plot_ly(
                    type = 'scatterpolar',
                    r =   as.numeric(selected[1:4]),
                    theta = c("Agriculture",
                              "Community",
                              "Biodiversity",
                              "Water"),
                    fill = 'toself',
                )
                
                fig <- fig %>%
                    layout(
                        polar = list(radialaxis = list(
                            visible = T,
                            range = c(0,1))
                        ),
                        plot_bgcolor  = "rgba(0, 0, 0, 0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0)",
                        fig_bgcolor   = "rgba(0, 0, 0, 0)",
                        showlegend = F
                    )
                fig
            })
        }

      
        
        

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
                
                proxy <- leafletProxy("map", data = PL1())
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
                
                proxy <- leafletProxy("map", data = PG1())
                proxy %>% addPolygons(lng = ~lng, lat = ~lat, group = "draw")
            }
            #####
            else if(thisShape[3] == "rectangle"){
                rlng1 <- as.numeric(thisShape[5])
                rlat1 <- as.numeric(thisShape[6])
                rlng2 <- as.numeric(thisShape[9])
                rlat2 <- as.numeric(thisShape[10])
                
                proxy <- leafletProxy("map")
                proxy %>% addRectangles(lng1 = rlng1, lat1 = rlat1, lng2 = rlng2, lat2 = rlat2,
                                        group = "draw")
            }
            #####
            else if(thisShape[3] == "circle"){
                crad <- as.numeric(thisShape[4])
                clng <- as.numeric(thisShape[6])
                clat <- as.numeric(thisShape[7])
                
                proxy <- leafletProxy("map")
                proxy %>% addCircles(lng = clng, lat = clat, radius = crad, group = "draw")
            }
            #####
            else if(thisShape[3] == "marker") {
                mlng <- as.numeric(thisShape[5])
                mlat <- as.numeric(thisShape[6])
                
                proxy <- leafletProxy("map")
                proxy %>% addMarkers(lng = mlng, lat = mlat, group = "draw")
            }
            #####
            else if(thisShape[3] == "circlemarker") {
                cmlng <- as.numeric(thisShape[6])
                cmlat <- as.numeric(thisShape[7])
                
                proxy <- leafletProxy("map")
                proxy %>% addCircleMarkers(lng = cmlng, lat = cmlat, group = "draw")
            }
        }
    })
    
     
    
       
}
    
