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

 
# Defining Map Elements ------------------------------------------------------------------------
  
## Select layers individualy  ---------------------------------------------------------
  
  observeEvent(input$checkbox_water, {
    if (input$checkbox_water) {
      updateSliderInput(session, "water_w", value = 100)
      updateSliderInput(session, "soil_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "resil_w", value = 0)
    }
  })
  
  observeEvent(input$checkbox_agri, {
    if (input$checkbox_agri) {
      updateSliderInput(session, "soil_w", value = 100)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "resil_w", value = 0)
    }
    })
    
  observeEvent(input$checkbox_bio, {
    if (input$checkbox_bio) {
      updateSliderInput(session, "biodiversity_w", value = 100)
      updateSliderInput(session, "soil_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
      updateSliderInput(session, "resil_w", value = 0)
    }
  })
      
  observeEvent(input$checkbox_com, {
    if (input$checkbox_com) {
      updateSliderInput(session, "resil_w", value = 100)
      updateSliderInput(session, "soil_w", value = 0)
      updateSliderInput(session, "biodiversity_w", value = 0)
      updateSliderInput(session, "water_w", value = 0)
    }
  })
  
## AHP weights  -----------------------------------------------------------------------

### Calculate aggregated preference values from weights --------------------
  agg_pref_df <- reactive({
    data.frame(resources = c("soil_fz","resil_fz","bio_fz","water_fz"),
               weights= c(input$soil_w,
                          input$resil_w,
                          input$biodiversity_w,
                          input$water_w)) %>% 
      mutate(agg_pref = weights/sum(weights))
  })
  
  output$soil_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[1,3]),2)))
  output$resilience_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[2,3]),2)))
  output$biodiversity_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[3,3]),2)))
  output$water_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df()[4,3]),2)))
  

### initialize the weights --------------------
    water_weights = reactiveValues(None=NULL,
                                   All=88,
                                   Government=78,
                                   "Non-profit"=100,
                                   "Farm/Ranch"=76,
                                   Indigenous=58)
    
    
### If selected element changes, then update the slider --------------------
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
      updateSliderInput(session, "soil_w", value = selected_weight)
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
      updateSliderInput(session, "resil_w", value = selected_weight)
    
    })
    
### If slide bar values do not match group weights --------------------
    observe({
      if(input$resil_w == com_weights[["Government"]] &
         input$biodiversity_w == bio_weights[["Government"]] &
         input$soil_w == agri_weights[["Government"]] &
         input$water_w == water_weights[["Government"]]){
      } else if(input$resil_w == com_weights[["All"]] &
                input$biodiversity_w == bio_weights[["All"]] &
                input$soil_w == agri_weights[["All"]] &
                input$water_w == water_weights[["All"]]) {
      } else if(input$resil_w == com_weights[["Non-profit"]] &
                input$biodiversity_w == bio_weights[["Non-profit"]] &
                input$soil_w == agri_weights[["Non-profit"]] &
                input$water_w == water_weights[["Non-profit"]]) {
      } else if(input$resil_w == com_weights[["Farm/Ranch"]] &
                input$biodiversity_w == bio_weights[["Farm/Ranch"]] &
                input$soil_w == agri_weights[["Farm/Ranch"]] &
                input$water_w == water_weights[["Farm/Ranch"]]) {
      } else if(input$resil_w == com_weights[["Indigenous"]] &
                input$biodiversity_w == bio_weights[["Indigenous"]] &
                input$soil_w == agri_weights[["Indigenous"]] &
                input$water_w == water_weights[["Indigenous"]]) {
      } else {
        updateSelectInput(session,"stakeholder_w",selected="None")
      }
    })

    
## Apply selected weights -------------------------------------------------------------

    weights_reactive <- reactive({
        
      ### Using Aggregated Preference values  --------------------
      resources_axis_r %>% 
            mutate("soil_fz" = soil_fz * agg_pref_df()[1,3]) %>%
            mutate("resil_fz" = resil_fz * agg_pref_df()[2,3]) %>%
            mutate("bio_fz" = bio_fz * agg_pref_df()[3,3]) %>%
            mutate("water_fz" = water_fz * agg_pref_df()[4,3]) %>%
            mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
            mutate(norm_score = range_norm_manual(score))
      
      
      ### Applying weights directly  --------------------
        #### Process for Raster data
        # resources_axis_r %>% 
        #     mutate("soil_fz" = soil_fz * input$soil_w) %>% 
        #     mutate("resil_fz" = resil_fz * input$resil_fz) %>% 
        #     mutate("bio_fz" = bio_fz * input$biodiversity_w) %>% 
        #     mutate("water_fz" = water_fz * input$water_w) %>% 
        #     mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>% 
        #     mutate(norm_score = range_norm_manual(score))
        
        #### Process for Vector data
        # resources_axis_df %>%
        #     mutate_at(vars(soil_fz), function(x) input$soil_w * x) %>% 
        #     mutate_at(vars(resil_fz), function(x) input$resil_fz * x) %>% 
        #     mutate_at(vars(bio_fz), function(x) input$biodiversity_w * x) %>% 
        #     mutate_at(vars(water_fz), function(x) input$water_w * x) %>% 
        #     mutate(score = soil_fz+resil_fz+bio_fz+water_fz) %>% 
        #     mutate(norm_score = range_norm(score)[,1],.after = objectid) %>% 
        #     select(c(objectid,norm_score))
        
    }) # end weights_reactive
    
    
    ## Create palette  --------------------
    pallete_reactive <- reactive({
        
        #### Process for Raster data
        colorNumeric(palette= "Greens",
                     domain = weights_reactive()["norm_score"][[1]],
                     na.color = "transparent",
                     reverse = TRUE)
        
        #### Process for Vector data
        # colorNumeric("Greens", weights_reactive()$norm_score) # pallete for leaflet
        
    }) # end pallete_reactive
    
    ### Process for Vector data
    # map_reactive <- reactive({
    # 
    #     left_join(resources_axis_sf, weights_reactive(), by = 'objectid')
    #     
    # }) # end map_reactive
    
    
# Leaflet Map Display ------------------------------------------------------------------------

    output$map <- renderLeaflet({

        ### Process for Raster data
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
                             edit = TRUE, remove = FALSE, selectedPathOptions = NULL,
                             allowIntersection = FALSE
                               ),
                           position = "topright",
                           singleFeature = TRUE)
        
        ## Process for Raster data
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

    ## Extract polygon spatial information ------------------
    observeEvent(input$drawing, {
      
      ddf <- data.frame() # The drawing dataframe
      ind <- which(ddf == "Feature") # Index for drawing df to break up the df to redraw the shapes.
      ind <- as.array(ind)
      
      for (i in 1:nrow(ind)) {
        if(i != nrow(ind)) thisShape <- ddf[ind[i]:ind[i+1]]
        else thisShape <- ddf[ind[i]:ncol(ddf)]
        
        
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
          
          output$statsBut <- renderUI({
            actionButton("removeShapes", h5(strong("Generate Stats")))
          })
          
        }

        else if(thisShape[3] == "rectangle"){
          rlng1 <- as.numeric(thisShape[5])
          rlat1 <- as.numeric(thisShape[6])
          rlng2 <- as.numeric(thisShape[9])
          rlat2 <- as.numeric(thisShape[10])
          
          proxy <- leafletProxy("map")
          proxy %>% addRectangles(lng1 = rlng1, lat1 = rlat1, lng2 = rlng2, lat2 = rlat2,
                                  group = "draw")
          
        }

      }
    })
    
    ## All study area Summary and plot tabs  --------------------
    observe({
    # if(!input$printShapes) {
    #   # Output to be displayed if button has been clicked
    # extracted_df <- cbind(soil_fz= as.numeric(resources_axis_r$soil_fz),
    #                       resil_fz= as.numeric(resources_axis_r$resil_fz),
    #                       bio_fz= as.numeric(resources_axis_r$bio_fz),
    #                       water_fz= as.numeric(resources_axis_r$water_fz),
    #                       agg_val= as.numeric(resources_axis_r$agg_val)) %>%
    #   as.data.frame() %>% 
    #   na.omit()
    # 
    # mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
    # 
    # summary_data <- data.frame(Resource=c("Soil","Resilience","Biodiversity","Water resources","Aggregated score"),
    #                            Score= mean_extracted_values)
    # rownames(summary_data)<-NULL
    # 
    # ### Table  --------------------
    # output$mytable =  render_gt({
    #   dplyr::tibble(img=c( "www/img/soil_icon.png",
    #                        "www/img/resilience_icon.png",
    #                        "www/img/bio_icon.png",
    #                        "www/img/water_icon.png"),
    #                 summary_data %>%
    #                   filter(!Resource %in% "Aggregated score")) %>% 
    #     arrange(desc(Score)) %>%
    #     gt() %>% 
    #     fmt_number(columns = Score,decimals = 3) %>% 
    #     cols_label(img = "") %>% 
    #     gt_img_rows(columns = img, height = 25, img_source = "local") %>% 
    #     tab_caption("Average scores without weighting:") %>% 
    #     tab_options(table.background.color = "transparent",
    #                 table.font.size = 17,
    #                 data_row.padding = px(2),
    #                 table.width = 300) %>% 
    #     tab_style(
    #       style = list(
    #         cell_text(weight = "bold")
    #       ),
    #       locations = cells_column_labels()
    #     )
    # })
    # 
    # ### Gauge --------------------
    # output$gauge = renderGauge({
    #   gauge(mean(as.numeric(weights_reactive()$norm_score), na.rm=T), 
    #         min = 0, 
    #         max = 1, 
    #         abbreviateDecimals=2,
    #         label ="Aggregated score",
    #         sectors = gaugeSectors(success = c(0.6, 1), 
    #                                warning = c(0.35, 0.6),
    #                                danger = c(0, 0.35),
    #                                colors = c("#3e8536","#83c47c","#bad9b6"))
    #   )
    # })
    # 
    # ### Boxplot   --------------------
    # output$boxplot <- renderPlot({
    #   extracted_df %>%
    #     rename(Soil = soil_fz, Resilience = resil_fz, Biodiversity = bio_fz, Water = water_fz) %>%
    #     pivot_longer(cols=c(Soil, Resilience, Biodiversity, Water)) %>%
    #     group_by(name) %>%
    #     mutate(mean_value = mean(value)) %>%
    #     ungroup() %>%
    #     arrange(mean_value) %>%
    #     mutate(name = factor(name, levels = unique(name))) %>%
    #     ggplot(aes(x=name, y=value, fill=name)) +
    #     geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7, 
    #                  outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
    #     scale_fill_manual(values=brewer.pal(n=4, name="Greens")) +
    #     theme_minimal() +
    #     labs(x="", y="", title="Resources data distribution (without weighting):") +
    #     theme(plot.title = element_text(hjust = 6, vjust = -2,size=15,
    #                                     color="#808080", margin = margin(0,0,15,0)),
    #           axis.text.y = element_text( size=12, face="bold"),
    #           axis.text.x = element_text(angle=45, vjust=0.7, hjust=0.7,
    #                                      size=10, face="bold"),
    #           panel.grid.major.y = element_blank(),
    #           panel.grid.minor.y = element_blank(),
    #           panel.grid.major.x = element_line(color = "gray87"),
    #           panel.grid.minor.x = element_blank(),
    #           panel.background = element_blank(),
    #           legend.position="none") +
    #     coord_flip()
    # },bg="transparent",height = 230, width = 400 )
    # 
    # 
    # ### Radar graph   --------------------
    # output$radar_graph <- renderPlotly({
    #   
    #   selected <- as.data.frame(df)
    #   
    #   fig <- plot_ly(
    #     type = 'scatterpolar',
    #     r = as.numeric(mean_extracted_values[1:4]),
    #     theta = c("Soil", "Resilience", "Biodiversity", "Water"),
    #     fill = 'toself',
    #     marker = list(color = 'rgba(23, 135, 53, 0.9)', size = 5),
    #     fillcolor = list(color = 'rgba(27, 181, 68, 0.5)')
    #   )
    #   
    #   fig <- fig %>%
    #     layout(
    #       polar = list(radialaxis = list(
    #         visible = T,
    #         range = c(0,1))
    #       ),
    #       plot_bgcolor  = "rgba(0, 0, 0, 0)",
    #       paper_bgcolor = "rgba(0, 0, 0, 0)",
    #       fig_bgcolor   = "rgba(0, 0, 0, 0)",
    #       showlegend = F
    #     )
    #   fig
    # })
    # 
    # 
    # } else if (input$printShapes) {
      
      ## Selected area Summary and plot tabs  --------------------

    observeEvent(input$map_draw_all_features, {

      shapedf <- data.frame()
      reactive(shapedf)
      shapedf <- input$map_draw_all_features
      sh <- as.data.frame(shapedf)

      ### Alert not drawn shape --------------------

      # num_features <- length(input$map_draw_all_features$features)
      # 
      # if(is.null(unlist(input$map_draw_all_features$features[num_features]))){
      #   shinyalert("Draw a shape",
      #              "Draw a polygon to extract statistics",
      #              type = "info",
      #              size="xs",
      #              animation=F,
      #              closeOnClickOutside = TRUE)
      # }
      # 
      # ### Clip raster --------------------
      # else {
        polygon <- sh %>% 
            dplyr::select(starts_with("features.geometry.coordinates")) %>% 
            as.numeric() %>% 
            matrix(ncol=2, byrow=TRUE) %>% 
            list() %>% 
            st_polygon() %>% 
            st_sfc(crs="EPSG:4326")
        
        polygon_sp <- as(polygon, Class = "Spatial")
        
        ### Alert not invalid shape --------------------
        
        if (gIsValid(polygon_sp) == FALSE){
            shinyalert(html = TRUE,
                       "Shape edges can not cross!", 
                       "Draw again the polygon to extract statistics", 
                       type = "warning",
                       size="xs",
                       animation=F,
                       closeOnClickOutside = TRUE)
          
          remove(shapedf)
        }
      
        
        else {

            extracted_area <- st_crop(resources_axis_r, polygon) # Original spatial data
            extracted_area_aggr <- st_crop(weights_reactive(), polygon) # Aggregated spatial data
            
            extracted_df <- cbind(soil_fz= as.numeric(extracted_area$soil_fz),
                                  resil_fz= as.numeric(extracted_area$resil_fz),
                                  bio_fz= as.numeric(extracted_area$bio_fz),
                                  water_fz= as.numeric(extracted_area$water_fz),
                                  aggregated_val= as.numeric(extracted_area_aggr$norm_score)) %>%
              as.data.frame() %>% 
              na.omit()
            
         
            mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
            
            summary_data <- data.frame(Resource=c("Soil","Resilience","Biodiversity","Water resources","Aggregated score"),
                                       Score=mean_extracted_values)
            rownames(summary_data)<-NULL
            
            ### Table --------------------
            
            output$mytable =  render_gt({
              dplyr::tibble(img=c( "www/img/soil_icon.png",
                                   "www/img/resilience_icon.png",
                                   "www/img/bio_icon.png",
                                   "www/img/water_icon.png"),
                            summary_data %>%
                              filter(!Resource %in% "Aggregated score")) %>% 
                arrange(desc(Score)) %>%
                gt() %>% 
                fmt_number(columns = Score,decimals = 3) %>% 
                cols_label(img = "") %>% 
                gt_img_rows(columns = img, height = 25, img_source = "local") %>% 
                tab_caption("Average scores without weighting:") %>% 
                tab_options(table.background.color = "transparent",
                            table.font.size = 17,
                            data_row.padding = px(2),
                            table.width = 300) %>% 
                tab_style(
                  style = list(
                    cell_text(weight = "bold")
                  ),
                  locations = cells_column_labels()
                )
            },height = 210)
            
            ### Gauge --------------------
            
            output$gauge = renderGauge({
              gauge(as.numeric(mean_extracted_values[5]), 
                    min = 0, 
                    max = 1, 
                    abbreviateDecimals=2,
                    label ="Aggregated score",
                    sectors = gaugeSectors(success = c(0.6, 1), 
                                           warning = c(0.35, 0.6),
                                           danger = c(0, 0.35),
                                           colors = c("#3e8536","#83c47c","#bad9b6"))
              )
            })
            
            
            ### Radar graph --------------------
            
            output$radar_graph <- renderPlotly({
              
              selected <- as.data.frame(df)
              
              fig <- plot_ly(
                type = 'scatterpolar',
                r = as.numeric(mean_extracted_values[1:4]),
                theta = c("Soil", "Resilience", "Biodiversity", "Water"),
                fill = 'toself',
                marker = list(color = 'rgba(23, 135, 53, 0.9)', size = 5),
                fillcolor = list(color = 'rgba(27, 181, 68, 0.5)')
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
        
        
            ### Boxplot --------------------
            
            output$boxplot <- renderPlot({
              extracted_df %>%
              rename(Soil = soil_fz, Resilience = resil_fz, Biodiversity = bio_fz, Water = water_fz) %>%
              pivot_longer(cols=c(Soil, Resilience, Biodiversity, Water)) %>%
              group_by(name) %>%
              mutate(mean_value = mean(value)) %>%
              ungroup() %>%
              arrange(mean_value) %>%
              mutate(name = factor(name, levels = unique(name))) %>%
              ggplot(aes(x=name, y=value, fill=name)) +
              geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7, 
                           outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
              scale_fill_manual(values=brewer.pal(n=4, name="Greens")) +
              theme_minimal() +
              labs(x="", y="", title="Resources data distribution (without weighting):") +
              theme(plot.title = element_text(hjust = 6, vjust = -2,size=15,
                                              color="#808080", margin = margin(0,0,15,0)),
                    axis.text.y = element_text( size=12, face="bold"),
                    axis.text.x = element_text(angle=45, vjust=0.7, hjust=0.7,
                                               size=10, face="bold"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_line(color = "gray87"),
                    panel.grid.minor.x = element_blank(),
                    panel.background = element_blank(),
                    legend.position="none") +
              coord_flip()
            },bg="transparent",height = 230, width = 400 )
        }
      # }

    })
    })

    

    ## Return to the whole area if the map is refreshed --------------------
    
    observeEvent(c(input$removeShapes, input$soil_w,input$water_w,input$biodiversity_w,input$resil_w), {
      
      extracted_df <- cbind(soil_fz= as.numeric(resources_axis_r$soil_fz),
                            resil_fz= as.numeric(resources_axis_r$resil_fz),
                            bio_fz= as.numeric(resources_axis_r$bio_fz),
                            water_fz= as.numeric(resources_axis_r$water_fz),
                            agg_val= as.numeric(resources_axis_r$agg_val)) %>%
        as.data.frame() %>% 
        na.omit()
      
      mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
      
      summary_data <- data.frame(Resource=c("Soil","Resilience","Biodiversity","Water resources","Aggregated score"),
                                 Score= mean_extracted_values)
      rownames(summary_data)<-NULL
      
      ### Table  --------------------
      output$mytable =  render_gt({
        dplyr::tibble(img=c( "www/img/soil_icon.png",
                             "www/img/resilience_icon.png",
                             "www/img/bio_icon.png",
                             "www/img/water_icon.png"),
                      summary_data %>%
                        filter(!Resource %in% "Aggregated score")) %>% 
          arrange(desc(Score)) %>%
          gt() %>% 
          fmt_number(columns = Score,decimals = 3) %>% 
          cols_label(img = "") %>% 
          gt_img_rows(columns = img, height = 25, img_source = "local") %>% 
          tab_caption("Average scores without weighting:") %>% 
          tab_options(table.background.color = "transparent",
                      table.font.size = 17,
                      data_row.padding = px(2),
                      table.width = 300) %>% 
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),
            locations = cells_column_labels()
          )
      })
      
      ### Gauge --------------------
      output$gauge = renderGauge({
        gauge(mean(as.numeric(weights_reactive()$norm_score), na.rm=T), 
              min = 0, 
              max = 1, 
              abbreviateDecimals=2,
              label ="Aggregated score",
              sectors = gaugeSectors(success = c(0.6, 1), 
                                     warning = c(0.35, 0.6),
                                     danger = c(0, 0.35),
                                     colors = c("#3e8536","#83c47c","#bad9b6"))
        )
      })
      
      ### Boxplot   --------------------
      output$boxplot <- renderPlot({
        extracted_df %>%
          rename(Soil = soil_fz, Resilience = resil_fz, Biodiversity = bio_fz, Water = water_fz) %>%
          pivot_longer(cols=c(Soil, Resilience, Biodiversity, Water)) %>%
          group_by(name) %>%
          mutate(mean_value = mean(value)) %>%
          ungroup() %>%
          arrange(mean_value) %>%
          mutate(name = factor(name, levels = unique(name))) %>%
          ggplot(aes(x=name, y=value, fill=name)) +
          geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7, 
                       outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
          scale_fill_manual(values=brewer.pal(n=4, name="Greens")) +
          theme_minimal() +
          labs(x="", y="", title="Resources data distribution (without weighting):") +
          theme(plot.title = element_text(hjust = 6, vjust = -2,size=15,
                                          color="#808080", margin = margin(0,0,15,0)),
                axis.text.y = element_text( size=12, face="bold"),
                axis.text.x = element_text(angle=45, vjust=0.7, hjust=0.7,
                                           size=10, face="bold"),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_line(color = "gray87"),
                panel.grid.minor.x = element_blank(),
                panel.background = element_blank(),
                legend.position="none") +
          coord_flip()
      },bg="transparent",height = 230, width = 400 )
      
      
      ### Radar graph   --------------------
      output$radar_graph <- renderPlotly({
        
        selected <- as.data.frame(df)
        
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.numeric(mean_extracted_values[1:4]),
          theta = c("Soil", "Resilience", "Biodiversity", "Water"),
          fill = 'toself',
          marker = list(color = 'rgba(23, 135, 53, 0.9)', size = 5),
          fillcolor = list(color = 'rgba(27, 181, 68, 0.5)')
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
      
    })
    
    
        ## Edit tab's note about data displayed  ------------------
        
        observeEvent(c(input$alpha, input$soil_w,input$water_w,input$biodiversity_w,input$resil_w), {
          output$data_displayed_note_summary <- renderText({
            "Data for the entire region"
          })
          output$data_displayed_note_plot <- renderText({
            "Data for the entire region"
          })
        })
        
        observeEvent(input$map_draw_all_features, {
          output$data_displayed_note_summary <- renderText({
            "Data for the selected area"
          })
          output$data_displayed_note_plot <- renderText({
            "Data for the selected area"
          })
        })
    
        ## Select summary tab when stats are generated if About tab is open  ------------------
        

        observeEvent(input$map_draw_all_features, {
          if (input$tabs == "about") {
            # Add a new tab panel to the tabset
            updateTabsetPanel(session, "tabs",
                              selected = paste0("summary", input$controller)
            )
          }
          })

        ## Reset map to remove shapes
        ### Uses the update of the alpha slider to force refresh
        observeEvent(input$removeShapes,{
          reset_value = input$alpha
          updateSliderInput(session, "alpha", value = reset_value+0.01)
        })

    
    
}
    
