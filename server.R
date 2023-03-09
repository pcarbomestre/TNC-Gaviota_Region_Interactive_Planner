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
  
  # Display individual layers  ---------------------------------------------------------
  # ------------------------------------------------------------------------------------
  
  observeEvent(input$checkbox_water, {
    if (input$checkbox_water) {
      updateSliderInput(session, "water_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
  })
  
  observeEvent(input$checkbox_agri, {
    if (input$checkbox_agri) {
      updateSliderInput(session, "agriculture_w", value = 5)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
    })
    
  observeEvent(input$checkbox_bio, {
    if (input$checkbox_bio) {
      updateSliderInput(session, "biodiversity_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "community_w", value = 0)
    }
  })
      
  observeEvent(input$checkbox_com, {
    if (input$checkbox_com) {
      updateSliderInput(session, "community_w", value = 5)
      updateSliderInput(session, "agriculture_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
    }
  })
  
  # AHP weights  -----------------------------------------------------------------------
  # ------------------------------------------------------------------------------------
    
  # Calculate aggregated preference values from weights
  agg_pref_df <- reactive({
    data.frame(resources = c("agricultur","community","flora_faun","water_raw"),
               weights= c(input$agriculture_w,
                          input$community_w,
                          input$biodiversity_w,
                          input$water_w)) %>% 
      mutate(agg_pref = weights/sum(weights))
  })
  
  output$agriculture_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[1,3]),2)))
  output$community_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[2,3]),2)))
  output$biodiversity_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[3,3]),2)))
  output$water_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[4,3]),2)))
  

    # initialize the weights
    water_weights = reactiveValues(None=NULL,
                                   All=88,
                                   Government=78,
                                   "Non-profit"=100,
                                   "Farm/Ranch"=76,
                                   Indigenous=58)
    
    
    
    # If selected element changes, then update the slider
    observeEvent(input$stakeholder_w, {
      selected_weight = water_weights[[input$stakeholder_w]]
      updateSliderInput(session, "water_w", value = selected_weight)
    })
    
    agri_weights = reactiveValues(None=NULL,
                                  All=53,
                                  Government=32,
                                  "Non-profit"=96,
                                  "Farm/Ranch"=100,
                                  Indigenous=44)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = agri_weights[[input$stakeholder_w]]
      updateSliderInput(session, "agriculture_w", value = selected_weight)
    })
    
    bio_weights = reactiveValues(None=NULL,
                                 All= 100,
                                 Government=100,
                                 "Non-profit"=100,
                                 "Farm/Ranch"=20,
                                 Indigenous=75)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = bio_weights[[input$stakeholder_w]]
      updateSliderInput(session, "biodiversity_w", value = selected_weight)
    })
    
    com_weights = reactiveValues(None=NULL,
                                 All=72,
                                 Government=59,
                                 "Non-profit"=74,
                                 "Farm/Ranch"=22,
                                 Indigenous=100)
    
    observeEvent(input$stakeholder_w, {
      selected_weight = com_weights[[input$stakeholder_w]]
      updateSliderInput(session, "community_w", value = selected_weight)
    
    })
    
    # If slide bar values do not match group weights
    observe({
      if(input$community_w == com_weights[["Government"]] &
         input$biodiversity_w == bio_weights[["Government"]] &
         input$agriculture_w == agri_weights[["Government"]] &
         input$water_w == water_weights[["Government"]]){
      } else if(input$community_w == com_weights[["All"]] &
                input$biodiversity_w == bio_weights[["All"]] &
                input$agriculture_w == agri_weights[["All"]] &
                input$water_w == water_weights[["All"]]) {
      } else if(input$community_w == com_weights[["Non-profit"]] &
                input$biodiversity_w == bio_weights[["Non-profit"]] &
                input$agriculture_w == agri_weights[["Non-profit"]] &
                input$water_w == water_weights[["Non-profit"]]) {
      } else if(input$community_w == com_weights[["Farm/Ranch"]] &
                input$biodiversity_w == bio_weights[["Farm/Ranch"]] &
                input$agriculture_w == agri_weights[["Farm/Ranch"]] &
                input$water_w == water_weights[["Farm/Ranch"]]) {
      } else if(input$community_w == com_weights[["Indigenous"]] &
                input$biodiversity_w == bio_weights[["Indigenous"]] &
                input$agriculture_w == agri_weights[["Indigenous"]] &
                input$water_w == water_weights[["Indigenous"]]) {
      } else {
        updateSelectInput(session,"stakeholder_w",selected="None")
      }
    })

    
  # Apply selected weights -------------------------------------------------------------
  # ------------------------------------------------------------------------------------
    
 
    weights_reactive <- reactive({
        
      # Using Aggregated Preference values
      resources_axis_r %>% 
            mutate("agricultur" = agricultur * agg_pref_df()[1,3]) %>%
            mutate("community" = community * agg_pref_df()[2,3]) %>%
            mutate("flora_faun" = flora_faun * agg_pref_df()[3,3]) %>%
            mutate("water_raw" = water_raw * agg_pref_df()[4,3]) %>%
            mutate(score = agricultur + community + flora_faun + water_raw) %>%
            mutate(norm_score = range_norm_manual(score))
      
      
      # Applying weights directly
        ## Process for Raster data --------------------
        # resources_axis_r %>% 
        #     mutate("agricultur" = agricultur * input$agriculture_w) %>% 
        #     mutate("community" = community * input$community_w) %>% 
        #     mutate("flora_faun" = flora_faun * input$biodiversity_w) %>% 
        #     mutate("water_raw" = water_raw * input$water_w) %>% 
        #     mutate(score = agricultur + community + flora_faun + water_raw) %>% 
        #     mutate(norm_score = range_norm_manual(score))
        
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
                     na.color = "transparent",
                     reverse = TRUE)
        
        ## Process for Vector data --------------------
        # colorNumeric("Greens", weights_reactive()$norm_score) # pallete for leaflet
        
    }) # end pallete_reactive
    
    ## Process for Vector data --------------------
    # map_reactive <- reactive({
    # 
    #     left_join(resources_axis_sf, weights_reactive(), by = 'objectid')
    #     
    # }) # end map_reactive
    
    
    # Map Display ------------------------------------------------------------------------
    # ------------------------------------------------------------------------------------
    
    output$map <- renderLeaflet({

        ## Process for Raster data --------------------
        map <- leaflet(options = leafletOptions(minZoom = 9)) %>% addTiles() %>%
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
                      opacity = input$alpha,
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
            fitBounds(lng1=as.numeric(bb(weights_reactive())[1]), 
                      lat1=as.numeric(bb(weights_reactive())[2]),
                      lng2=as.numeric(bb(weights_reactive())[3]),
                      lat2=as.numeric(bb(weights_reactive())[4])) %>% 
          setMaxBounds(lng1=as.numeric(bb(weights_reactive())[1])-1, 
                       lat1=as.numeric(bb(weights_reactive())[2])-0.4,
                       lng2=as.numeric(bb(weights_reactive())[3])+1,
                       lat2=as.numeric(bb(weights_reactive())[4])+0.4) %>% 
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
        
        polygon_sp <- as(polygon, Class = "Spatial")
        
        if (gIsValid(polygon_sp) == FALSE){
            shinyalert("Shape edges can not cross!", 
                       "Draw again the polygon to extract statistics", 
                       type = "warning",
                       size="xs",
                       animation=T)
        }
      
        
        else {

            extracted_area <- st_crop(resources_axis_r, polygon) # Original spatial data
            extracted_area_aggr <- st_crop(weights_reactive(), polygon) # Aggregated spatial data
            
            extracted_df <- cbind(agricultur= as.numeric(extracted_area$agricultur),
                                  community= as.numeric(extracted_area$community),
                                  flora_faun= as.numeric(extracted_area$flora_faun),
                                  water_raw= as.numeric(extracted_area$water_raw),
                                  aggregated_val= as.numeric(extracted_area_aggr$norm_score)) %>%
              as.data.frame() %>% 
              na.omit()
            
         
            mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
            
            summary_data <- data.frame(value=c("Agriculture","Community","bio","water","aggregated"),
                                       score=mean_extracted_values)
            rownames(summary_data)<-NULL
            
          
            output$mytable = renderTable({
              summary_data
            })
            
            output$gauge = renderGauge({
              gauge(as.numeric(mean_extracted_values[5]), 
                    min = 0, 
                    max = 1, 
                    abbreviateDecimals=2,
                    sectors = gaugeSectors(success = c(0.5, 1), 
                                           warning = c(0.3, 0.5),
                                           danger = c(0, 0.3)))
            })
            
            output$radar_graph <- renderPlotly({
              
              selected <- as.data.frame(df)
              
              fig <- plot_ly(
                type = 'scatterpolar',
                r =   as.numeric(mean_extracted_values[1:4]),
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
          #}
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
            
             if(thisShape[3] == "polygon") {
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
        }
    })
    

    
    
}
    
