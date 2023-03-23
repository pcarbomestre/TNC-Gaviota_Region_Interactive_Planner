## _____________________________
### File: server.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## _____________________________
## Description:
##  Server for the Interactive Planner app
## _____________________________


server <- function(input, output, session) {

  
  
  # NATURAL RESOURCES AXIS (MAIN TAB) ----
  # Defining Map Elements ----

  ## Select layers individually ----
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

  ## AHP weights ----

  ### Calculate aggregated preference values from weights ----
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

  ### Set group weights ----
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

  ### Update selection to "None" ----
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

  ## Apply selected weights ----

  weights_reactive <- reactive({
    ### Using Aggregated Preference values ----
    resources_axis_r %>%
      mutate("soil_fz" = soil_fz * agg_pref_df()[1,3]) %>%
      mutate("resil_fz" = resil_fz * agg_pref_df()[2,3]) %>%
      mutate("bio_fz" = bio_fz * agg_pref_df()[3,3]) %>%
      mutate("water_fz" = water_fz * agg_pref_df()[4,3]) %>%
      mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
      mutate(norm_score = range_norm_manual(score))



    #### Applying weights directly ----
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



  # Leaflet Map Display ----

  ## Create palette ----
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


  ## Create Map ----
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

  # Extracting data from the map ----

  ## Extract drawn polygon ----
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


  ## Extract statistics  ----

  ### All study area Summary and plot tabs ----

  observe({
    #### Using Generate Stats button ----
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

    ## Selected area Summary and plot tabs ----

    observeEvent(input$map_draw_all_features, {

      shapedf <- data.frame()
      reactive(shapedf)
      shapedf <- input$map_draw_all_features
      sh <- as.data.frame(shapedf)

      ### Alert not drawn shape if Generate stats button is used
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
      #
      # else {

      ### Clip raster ----
      polygon <- sh %>%
        dplyr::select(starts_with("features.geometry.coordinates")) %>%
        as.numeric() %>%
        matrix(ncol=2, byrow=TRUE) %>%
        list() %>%
        st_polygon() %>%
        st_sfc(crs="EPSG:4326")

      polygon_sp <- as(polygon, Class = "Spatial")

      ### Alert for invalid shape ----
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

      ### Extract data ----
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

        #### Table ----
        output$mytable =  render_gt({
          dplyr::tibble(img=c(here("www","img","soil_icon.png"),
                              here("www","img","resilience_icon.png"),
                              here("www","img","bio_icon.png"),
                              here("www","img","water_icon.png")),
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
            tab_style(style = list(cell_text(weight = "bold")),
                      locations = cells_column_labels())
        },height = 210)

        #### Gauge ----
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


        #### Radar graph ----
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


        #### Boxplot ----
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

  ## Return to the whole area if the map is refreshed  ----

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

    ### Table ----
    output$mytable =  render_gt({
      dplyr::tibble(img=c(here("www","img","soil_icon.png"),
                          here("www","img","resilience_icon.png"),
                          here("www","img","bio_icon.png"),
                          here("www","img","water_icon.png")),
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

    ### Gauge ----
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

    ### Boxplot ----
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


    ### Radar graph ----
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


  ## Edit tab's note about data displayed ----
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

  ## Select summary tab when stats are generated if About tab is open ----
  observeEvent(input$map_draw_all_features, {
    if (input$tabs == "about") {
      # Add a new tab panel to the tabset
      updateTabsetPanel(session, "tabs",
                        selected = paste0("summary", input$controller)
      )
    }
  })

  ## Reset map to remove shapes ----
  # Uses the update of the alpha slider to force refresh (there must be a better solution)
  observeEvent(input$removeShapes,{
    reset_value = input$alpha
    updateSliderInput(session, "alpha", value = reset_value+0.01)
  })

  
  
  
  # >-----
  # STAKEHODLERS PRIORITIES  ----
  
  ### Selecting group preferences ----
  observeEvent(c(input$group_1, input$group_2), {
  
  if(input$group_1 == "My") {
    resources_axis_1 <- weights_reactive()
    }
    else {
      group1 <- ahp_weights %>% 
        filter(group %in% paste0(input$group_1))
      
      resources_axis_1 <-  resources_axis_r %>%
        mutate("soil_fz" = soil_fz * as.numeric(group1[3,3])) %>%
        mutate("resil_fz" = resil_fz * as.numeric(group1[4,3])) %>%
        mutate("bio_fz" = bio_fz * as.numeric(group1[1,3])) %>%
        mutate("water_fz" = water_fz * as.numeric(group1[2,3])) %>%
        mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
        mutate(norm_score = range_norm_manual(score))
      
      }
  
    if(input$group_2 == "My") {
      resources_axis_2 <- weights_reactive()
    }
    else {
      group2 <- ahp_weights %>% 
        filter(group %in% paste0(input$group_2))
  
      resources_axis_2 <-  resources_axis_r %>%
        mutate("soil_fz" = soil_fz * as.numeric(group2[3,3])) %>%
        mutate("resil_fz" = resil_fz * as.numeric(group2[4,3])) %>%
        mutate("bio_fz" = bio_fz * as.numeric(group2[1,3])) %>%
        mutate("water_fz" = water_fz * as.numeric(group2[2,3])) %>%
        mutate(score = soil_fz + resil_fz + bio_fz + water_fz) %>%
        mutate(norm_score = range_norm_manual(score))
    }
    
    if (input$group_1 == input$group_2) {
      shinyalert(html = TRUE,
                 "Select another group",
                 "Same group comparison leads to no detectable differences",
                 type = "warning",
                 size="xs",
                 animation=F,
                 closeOnClickOutside = TRUE)
    }
    
    ### Calculating group differences ----
  resources_comp <- reactive({resources_axis_1-resources_axis_2})
  
  ### Displaying differences on a map ----
  
  value <- reactive({
    max(abs(max(resources_comp()["norm_score"][[1]], na.rm = T)),
               abs(min(resources_comp()["norm_score"][[1]], na.rm = T)))
  })
  
  color_pal <- reactive({
    colorNumeric(
    palette = "RdBu", 
    domain = c(-1*value(), 0,  value()),
    na.color = "transparent")
  })
  
  output$map_stake <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 9)) %>% addTiles() %>%
      addGeoRaster(resources_comp()["norm_score"],
                   opacity = input$alpha_stake,
                   colorOptions =leafem:::colorOptions(
                     palette = "RdBu",
                     domain = c(-1*value(), 0,  value()),
                     breaks = seq(min(resources_comp()["norm_score"][[1]], na.rm = TRUE),
                                  max(resources_comp()["norm_score"][[1]], na.rm = TRUE),
                                  length.out=100),
                     na.color = "transparent"
                   ),
                   resolution=10000) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addLegend(pal = color_pal(),
                       values = resources_comp()["norm_score"][[1]],
                       position = "bottomright",
                       opacity= input$alpha_stake
      )
    })
  })
  
  
# >-----
# ENVIRONMENTAL THREATS AXIS (MAIN TAB) ----

  # Defining Map Elements ----
  ## Select layers individually ----
  observeEvent(input$checkbox_climate, {
    if (input$checkbox_climate) {
      updateSliderInput(session, "cli_w", value = 100)
      updateSliderInput(session, "drgh_w", value = 0)
      updateSliderInput(session, "fld_w", value = 0)
      updateSliderInput(session, "wf_w", value = 0)
    }
  })

  observeEvent(input$checkbox_droughts, {
    if (input$checkbox_droughts) {
      updateSliderInput(session, "drgh_w", value = 100)
      updateSliderInput(session, "cli_w", value = 0)
      updateSliderInput(session, "fld_w", value = 0)
      updateSliderInput(session, "wf_w", value = 0)
    }
  })

  observeEvent(input$checkbox_floods, {
    if (input$checkbox_floods) {
      updateSliderInput(session, "fld_w", value = 100)
      updateSliderInput(session, "drgh_w", value = 0)
      updateSliderInput(session, "cli_w", value = 0)
      updateSliderInput(session, "wf_w", value = 0)
    }
  })

  observeEvent(input$checkbox_wildfires, {
    if (input$checkbox_wildfires) {
      updateSliderInput(session, "wf_w", value = 100)
      updateSliderInput(session, "drgh_w", value = 0)
      updateSliderInput(session, "cli_w", value = 0)
      updateSliderInput(session, "fld_w", value = 0)
    }
  })

  ## AHP weights ----

  ### Calculate aggregated preference values from weights ----
  agg_pref_df_threats <- reactive({
    data.frame(resources = c("cli_exp_fz","drgh_m_fz","fld_m_fz","wf_m_fz"),
               weights= c(input$cli_w,
                          input$drgh_w,
                          input$fld_w,
                          input$wf_w)) %>%
      mutate(agg_pref = weights/sum(weights))
  })

  output$cli_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_threats()[1,3]),2)))
  output$drgh_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_threats()[2,3]),2)))
  output$fld_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_threats()[3,3]),2)))
  output$wf_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_threats()[4,3]),2)))


  ## Select climate model ----
  drgh_model_selected <- reactive({
    if (input$model_drgh == "(Ensemble)") {
      c("drgh_h_fz","drgh_w_fz")
    } else if (input$model_drgh == "(MIROC-esm)") {
      c("drgh_m_fz","drgh_w_fz")
    }
    else if (input$model_drgh == "(CCSM4)") {
      c("drgh_m_fz","drgh_h_fz")
    }
    })
  
  fld_model_selected <- reactive({
    if (input$model_fld == "(Ensemble)") {
      c("fld_h_fz","fld_w_fz")
    } else if (input$model_fld == "(MIROC-esm)") {
      c("fld_m_fz","fld_w_fz")
    }
    else if (input$model_fld == "(CCSM4)") {
      c("fld_m_fz","fld_h_fz")
    }
  })
  
  wf_model_selected <- reactive({
    if (input$model_wf == "(Ensemble)") {
      c("wf_cn_fz","wf_mi_fz")
    } else if (input$model_wf == "(MIROC5)") {
      c("wf_m_fz","wf_cn_fz")
    }
    else if (input$model_wf == "(CNRM-CM5)") {
      c("wf_m_fz","wf_mi_fz")
    }
  })
  
  
  ## Apply selected weights ----
  weights_reactive_threats <- reactive({
    ### Using Aggregated Preference values ----
    threats_axis_r %>%
      # Select model from the inputSelect
      select(!all_of(drgh_model_selected())) %>% 
      rename("drgh_fz" = starts_with("drgh")) %>% 
      select(!all_of(fld_model_selected())) %>% 
      rename("fld_fz" = starts_with("fld")) %>% 
      select(!all_of(wf_model_selected())) %>% 
      rename("wf_fz" = starts_with("wf")) %>% 
      # Apply weights
      mutate("cli_exp_fz" = cli_exp_fz * agg_pref_df_threats()[1,3]) %>%
      mutate("drgh_m_fz" = drgh_fz * agg_pref_df_threats()[2,3]) %>%
      mutate("fld_m_fz" = fld_fz * agg_pref_df_threats()[3,3]) %>%
      mutate("wf_m_fz" = wf_fz * agg_pref_df_threats()[4,3]) %>%
      mutate(score = cli_exp_fz + drgh_m_fz + fld_m_fz + wf_m_fz) %>%
      mutate(norm_score = range_norm_manual(score))
  }) # end weights_reactive_threats



  # Leaflet Map Display ----

  ## Create palette ----
  pallete_reactive_threats <- reactive({
        colorNumeric(palette= "Oranges",
                 domain = weights_reactive_threats()["norm_score"][[1]],
                 na.color = "transparent",
                 reverse = TRUE)

  }) # end pallete_reactive

  ## Create Map ----
  output$map_threats <- renderLeaflet({

    ### Process for Raster data
    map_threats <- leaflet(options = leafletOptions(minZoom = 9)) %>% addTiles() %>%
      addGeoRaster(weights_reactive_threats()["norm_score"],
                   opacity = input$alpha_threats,
                   colorOptions =leafem:::colorOptions(
                     palette = "Oranges",
                     breaks = seq(min(weights_reactive_threats()["norm_score"][[1]], na.rm = TRUE),
                                  max(weights_reactive_threats()["norm_score"][[1]], na.rm = TRUE),
                                  100),
                     na.color = "transparent"
                   ),
                   resolution=10000) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addLegend(pal = pallete_reactive_threats(),
                values = weights_reactive_threats()["norm_score"][[1]],
                position = "bottomright",
                opacity = input$alpha_threats,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      fitBounds(lng1=as.numeric(bb(weights_reactive_threats())[1]),
                lat1=as.numeric(bb(weights_reactive_threats())[2]),
                lng2=as.numeric(bb(weights_reactive_threats())[3]),
                lat2=as.numeric(bb(weights_reactive_threats())[4])) %>%
      setMaxBounds(lng1=as.numeric(bb(weights_reactive_threats())[1])-1,
                   lat1=as.numeric(bb(weights_reactive_threats())[2])-0.4,
                   lng2=as.numeric(bb(weights_reactive_threats())[3])+1,
                   lat2=as.numeric(bb(weights_reactive_threats())[4])+0.4) %>%
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
  })

  # Extracting data from the map ----

  ## Extract drawn polygon ----
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
          actionButton("removeShapes_threats", h5(strong("Generate Stats")))
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


  ## Extract statistics  ----

  ### All study area Summary and plot tabs ----

  observe({
    
    
    ## Selected area Summary and plot tabs ----
    
    observeEvent(input$map_threats_draw_all_features, {
      
      shapedf <- data.frame()
      reactive(shapedf)
      shapedf <- input$map_threats_draw_all_features
      sh <- as.data.frame(shapedf)
      
      ### Clip raster ----
      polygon <- sh %>%
        dplyr::select(starts_with("features.geometry.coordinates")) %>%
        as.numeric() %>%
        matrix(ncol=2, byrow=TRUE) %>%
        list() %>%
        st_polygon() %>%
        st_sfc(crs="EPSG:4326")
      
      polygon_sp <- as(polygon, Class = "Spatial")
      
      ### Alert for invalid shape ----
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
      
      ### Extract data ----
      else {
        
        
        extracted_area <- st_crop(threats_axis_r, polygon) # Original spatial data
        extracted_area_aggr <- st_crop(weights_reactive_threats(), polygon) # Aggregated spatial data
        
        extracted_df <- cbind(cli_exp_fz= as.numeric(extracted_area$cli_exp_fz),
                              drgh_m_fz= as.numeric(extracted_area$drgh_m_fz),
                              fld_m_fz= as.numeric(extracted_area$fld_m_fz),
                              wf_m_fz= as.numeric(extracted_area$wf_m_fz),
                              aggregated_val= as.numeric(extracted_area_aggr$norm_score)) %>%
          as.data.frame() %>%
          na.omit()
        
        
        mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
        
        summary_data <- data.frame(Threat=c("Climate","Droughts","Floods","Wildfires","Aggregated score"),
                                   Score=mean_extracted_values)
        rownames(summary_data)<-NULL
        
        #### Table ----
        output$mytable_threats =  render_gt({
          dplyr::tibble(img=c(here("www","img","climate_icon.png"),
                              here("www","img","drought_icon.png"),
                              here("www","img","flood_icon.png"),
                              here("www","img","fire_icon.png")),
                        summary_data %>%
                          filter(!Threat %in% "Aggregated score")) %>%
            arrange(desc(Score)) %>%
            gt() %>%
            fmt_number(columns = Score,decimals = 3) %>%
            cols_label(img = "") %>%
            gt_img_rows(columns = img, height = 28, img_source = "local") %>%
            tab_caption("Average scores without weighting:") %>%
            tab_options(table.background.color = "transparent",
                        table.font.size = 17,
                        data_row.padding = px(2),
                        table.width = 300) %>%
            tab_style(style = list(cell_text(weight = "bold")),
                      locations = cells_column_labels())
        },height = 210)
        
        #### Gauge ----
        output$gauge_threats = renderGauge({
          gauge(as.numeric(mean_extracted_values[5]),
                min = 0,
                max = 1,
                abbreviateDecimals=2,
                label ="Aggregated score",
                sectors = gaugeSectors(success = c(0.70, 1),
                                       warning = c(0.35, 0.70),
                                       danger = c(0, 0.35),
                                       colors = c("#eb7900","#ff9900","#f5c57d"))
          )
        })
        
        
        #### Radar graph ----
        output$radar_graph_threats <- renderPlotly({
          
          selected <- as.data.frame(df)
          
          fig <- plot_ly(
            type = 'scatterpolar',
            r = as.numeric(mean_extracted_values[1:4]),
            theta = c("Climate", "Drought", "Floods", "Wildfires"),
            fill = 'toself',
            marker = list(color = 'rgba(224, 136, 4, 0.9)', size = 5),
            fillcolor = list(color = 'rgba(237, 164, 55, 0.5)')
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
        
        
        #### Boxplot ----
        output$boxplot_threats <- renderPlot({
          extracted_df %>%
            rename(Climate = cli_exp_fz, Droughts = drgh_m_fz, Floods = fld_m_fz, Wildfires = wf_m_fz) %>%
            pivot_longer(cols=c(Climate, Droughts, Floods, Wildfires)) %>%
            group_by(name) %>%
            mutate(mean_value = mean(value)) %>%
            ungroup() %>%
            arrange(mean_value) %>%
            mutate(name = factor(name, levels = unique(name))) %>%
            ggplot(aes(x=name, y=value, fill=name)) +
            geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7,
                         outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
            scale_fill_manual(values=brewer.pal(n=4, name="Oranges")) +
            theme_minimal() +
            labs(x="", y="", title="Threats data distribution (without weighting):") +
            theme(plot.title = element_text(hjust = -3, vjust = -2,size=15,
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
  


  ## Return to the whole area if the map is refreshed  ----

  observeEvent(c(input$removeShapes_threats, input$cli_w,input$drgh_w,input$fld_w,input$wf_w,
                 input$model_drgh,input$model_fld,input$model_wf), {

    threats_axis_all <- threats_axis_r %>% 
      select(!all_of(drgh_model_selected())) %>% 
      rename("drgh_fz" = starts_with("drgh")) %>% 
      select(!all_of(fld_model_selected())) %>% 
      rename("fld_fz" = starts_with("fld")) %>% 
      select(!all_of(wf_model_selected())) %>% 
      rename("wf_fz" = starts_with("wf"))
    
    extracted_df <- cbind(cli_exp_fz= as.numeric(threats_axis_all$cli_exp_fz),
                          drgh_m_fz= as.numeric(threats_axis_all$drgh_fz),
                          fld_m_fz= as.numeric(threats_axis_all$fld_fz),
                          wf_m_fz= as.numeric(threats_axis_all$wf_fz),
                          agg_val= as.numeric(threats_axis_all$agg_val)) %>%
      as.data.frame() %>%
      na.omit()

    mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)

    summary_data <- data.frame(Threat=c("Climate","Droughts","Floods","Wildfires","Aggregated score"),
                               Score= mean_extracted_values)
    rownames(summary_data)<-NULL

    ### Table ----
    output$mytable_threats =  render_gt({
      dplyr::tibble(img=c(here("www","img","climate_icon.png"),
                          here("www","img","drought_icon.png"),
                          here("www","img","flood_icon.png"),
                          here("www","img","fire_icon.png")),
                    summary_data %>%
                      filter(!Threat %in% "Aggregated score")) %>%
        arrange(desc(Score)) %>%
        gt() %>%
        fmt_number(columns = Score,decimals = 3) %>%
        cols_label(img = "") %>%
        gt_img_rows(columns = img, height = 28, img_source = "local") %>%
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

    ### Gauge ----
    output$gauge_threats = renderGauge({
      gauge(mean(as.numeric(weights_reactive_threats()$norm_score), na.rm=T),
            min = 0,
            max = 1,
            abbreviateDecimals=2,
            label ="Aggregated score",
            sectors = gaugeSectors(success = c(0.70, 1),
                                   warning = c(0.35, 0.70),
                                   danger = c(0, 0.35),
                                   colors = c("#eb7900","#ff9900","#f5c57d"))
      )
    })

    ### Boxplot ----
    output$boxplot_threats <- renderPlot({
      extracted_df %>%
        rename(Climate = cli_exp_fz, Droughts = drgh_m_fz, Floods = fld_m_fz, Wildfires = wf_m_fz) %>%
        pivot_longer(cols=c(Climate, Droughts, Floods, Wildfires)) %>%
        group_by(name) %>%
        mutate(mean_value = mean(value)) %>%
        ungroup() %>%
        arrange(mean_value) %>%
        mutate(name = factor(name, levels = unique(name))) %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7,
                     outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
        scale_fill_manual(values=brewer.pal(n=4, name="Oranges")) +
        theme_minimal() +
        labs(x="", y="", title="Threats data distribution (without weighting):") +
        theme(plot.title = element_text(hjust = -3, vjust = -2,size=15,
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


    ### Radar graph ----
    output$radar_graph_threats <- renderPlotly({

      selected <- as.data.frame(df)

      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.numeric(mean_extracted_values[1:4]),
        theta = c("Climate", "Droughts", "Floods", "Wildfires"),
        fill = 'toself',
        marker = list(color = 'rgba(224, 136, 4, 0.9)', size = 5),
        fillcolor = list(color = 'rgba(237, 164, 55, 0.5)')
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


  ## Edit tab's note about data displayed ----
  observeEvent(c(input$alpha_threats, input$cli_w,input$drgh_w,input$fld_w,input$wf_w), {
    output$data_displayed_note_summary_threats <- renderText({
      "Data for the entire region"
    })
    output$data_displayed_note_plot_threats <- renderText({
      "Data for the entire region"
    })
  })

  observeEvent(input$map_threats_draw_all_features, {
    output$data_displayed_note_summary_threats <- renderText({
      "Data for the selected area"
    })
    output$data_displayed_note_plot_threats <- renderText({
      "Data for the selected area"
    })
  })

  ## Select summary tab when stats are generated if About tab is open ----
  observeEvent(input$map_threats_draw_all_features, {
    if (input$tabs_threats == "about_threats") {
      # Add a new tab panel to the tabset
      updateTabsetPanel(session, "tabs_threats",
                        selected = paste0("summary_threats", input$controller)
      )
    }
  })

  ## Reset map to remove shapes ----
  # Uses the update of the alpha slider to force refresh (there must be a better solution)
  observeEvent(input$removeShapes_threats,{
    reset_value = input$alpha_threats
    updateSliderInput(session, "alpha_threats", value = reset_value+0.01)
  })

  # >-----
  # EQUITY ISSUES AXIS (MAIN TAB) ----
  
  # Defining Map Elements ----
  ## Select layers individually ----
  observeEvent(input$checkbox_pollution, {
    if (input$checkbox_pollution) {
      updateSliderInput(session, "pol_w", value = 100)
      updateSliderInput(session, "demo_w", value = 0)
      updateSliderInput(session, "access_w", value = 0)
    }
  })
  
  observeEvent(input$checkbox_demographics, {
    if (input$checkbox_demographics) {
      updateSliderInput(session, "demo_w", value = 100)
      updateSliderInput(session, "pol_w", value = 0)
      updateSliderInput(session, "access_w", value = 0)
    }
  })
  
  observeEvent(input$checkbox_access, {
    if (input$checkbox_access) {
      updateSliderInput(session, "access_w", value = 100)
      updateSliderInput(session, "pol_w", value = 0)
      updateSliderInput(session, "demo_w", value = 0)
    }
  })
  
  ## AHP weights ----
  
  ### Calculate aggregated preference values from weights ----
  agg_pref_df_equity <- reactive({
    data.frame(resources = c("pol_fz","demo_fz","access_fz"),
               weights= c(input$pol_w,
                          input$demo_w,
                          input$access_w)) %>%
      mutate(agg_pref = weights/sum(weights))
  })
  
  output$pol_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_equity()[1,3]),2)))
  output$demo_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_equity()[2,3]),2)))
  output$access_agg_pref <- renderText(paste(round(as.numeric(agg_pref_df_equity()[3,3]),2)))

  ## Apply selected weights ----
  
  weights_reactive_equity <- reactive({
    ### Using Aggregated Preference values ----
    equity_axis_r %>%
      mutate("pol_fz" = pol_fz * agg_pref_df_equity()[1,3]) %>%
      mutate("demo_fz" = demo_fz * agg_pref_df_equity()[2,3]) %>%
      mutate("access_fz" = access_fz * agg_pref_df_equity()[3,3]) %>%
      mutate(score = pol_fz + demo_fz + access_fz) %>%
      mutate(norm_score = range_norm_manual(score))
  }) # end weights_reactive_equity
  
  
  
  # Leaflet Map Display ----
  
  ## Create palette ----
  pallete_reactive_equity <- reactive({
    colorNumeric(palette= "Purples",
                 domain = weights_reactive_equity()["norm_score"][[1]],
                 na.color = "transparent",
                 reverse = TRUE)
    
  }) # end pallete_reactive
  
  ## Create Map ----
  output$map_equity <- renderLeaflet({
    
    ### Process for Raster data
    map_equity <- leaflet(options = leafletOptions(minZoom = 9)) %>% addTiles() %>%
      addGeoRaster(weights_reactive_equity()["norm_score"],
                   opacity = input$alpha_equity,
                   colorOptions =leafem:::colorOptions(
                     palette = "Purples",
                     breaks = seq(min(weights_reactive_equity()["norm_score"][[1]], na.rm = TRUE),
                                  max(weights_reactive_equity()["norm_score"][[1]], na.rm = TRUE),
                                  100),
                     na.color = "transparent"
                   ),
                   resolution=10000) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addLegend(pal = pallete_reactive_equity(),
                values = weights_reactive_equity()["norm_score"][[1]],
                position = "bottomright",
                opacity = input$alpha_equity,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      fitBounds(lng1=as.numeric(bb(weights_reactive_equity())[1]),
                lat1=as.numeric(bb(weights_reactive_equity())[2]),
                lng2=as.numeric(bb(weights_reactive_equity())[3]),
                lat2=as.numeric(bb(weights_reactive_equity())[4])) %>%
      setMaxBounds(lng1=as.numeric(bb(weights_reactive_equity())[1])-1,
                   lat1=as.numeric(bb(weights_reactive_equity())[2])-0.4,
                   lng2=as.numeric(bb(weights_reactive_equity())[3])+1,
                   lat2=as.numeric(bb(weights_reactive_equity())[4])+0.4) %>%
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
  })
  
  # Extracting data from the map ----
  
  ## Extract drawn polygon ----
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
          actionButton("removeShapes_equity", h5(strong("Generate Stats")))
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
  
  
  ## Extract statistics  ----
  
  ### All study area Summary and plot tabs ----
  
  observe({
    
    
    ## Selected area Summary and plot tabs ----
    
    observeEvent(input$map_equity_draw_all_features, {
      
      shapedf <- data.frame()
      reactive(shapedf)
      shapedf <- input$map_equity_draw_all_features
      sh <- as.data.frame(shapedf)
      
      ### Clip raster ----
      polygon <- sh %>%
        dplyr::select(starts_with("features.geometry.coordinates")) %>%
        as.numeric() %>%
        matrix(ncol=2, byrow=TRUE) %>%
        list() %>%
        st_polygon() %>%
        st_sfc(crs="EPSG:4326")
      
      polygon_sp <- as(polygon, Class = "Spatial")
      
      ### Alert for invalid shape ----
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
      
      ### Extract data ----
      else {
        
        extracted_area <- st_crop(equity_axis_r, polygon) # Original spatial data
        extracted_area_aggr <- st_crop(weights_reactive_equity(), polygon) # Aggregated spatial data
        
        extracted_df <- cbind(pol_fz= as.numeric(extracted_area$pol_fz),
                              demo_fz= as.numeric(extracted_area$demo_fz),
                              access_fz= as.numeric(extracted_area$access_fz),
                              aggregated_val= as.numeric(extracted_area_aggr$norm_score)) %>%
          as.data.frame() %>%
          na.omit()
        
        
        mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
        
        summary_data <- data.frame(Issue=c("Pollution","Demographics","Isolation from Nature","Aggregated score"),
                                   Score=mean_extracted_values)
        rownames(summary_data)<-NULL
        
        #### Table ----
        output$mytable_equity =  render_gt({
          dplyr::tibble(img=c(here("www","img","pollution_icon.png"),
                              here("www","img","demo_icon.png"),
                              here("www","img","access_icon.png")),
                        summary_data %>%
                          filter(!Issue %in% "Aggregated score")) %>%
            arrange(desc(Score)) %>%
            gt() %>%
            fmt_number(columns = Score,decimals = 3) %>%
            cols_label(img = "") %>%
            gt_img_rows(columns = img, height = 28, img_source = "local") %>%
            tab_caption("Average scores without weighting:") %>%
            tab_options(table.background.color = "transparent",
                        table.font.size = 17,
                        data_row.padding = px(2),
                        table.width = 300) %>%
            tab_style(style = list(cell_text(weight = "bold")),
                      locations = cells_column_labels())
        },height = 210)
        
        #### Gauge ----
        output$gauge_equity = renderGauge({
          gauge(as.numeric(mean_extracted_values[4]),
                min = 0,
                max = 1,
                abbreviateDecimals=2,
                label ="Aggregated score",
                sectors = gaugeSectors(success = c(0.70, 1),
                                       warning = c(0.35, 0.70),
                                       danger = c(0, 0.35),
                                       colors = c("#5f0185","#845f94","#be9bcc"))
          )
        })
        
        
        #### Radar graph ----
        output$radar_graph_equity <- renderPlotly({
          
          selected <- as.data.frame(df)
          
          fig <- plot_ly(
            type = 'scatterpolar',
            r = as.numeric(mean_extracted_values[1:3]),
            theta = c("Pollution", "Demographics", "Isolation"),
            fill = 'toself',
            marker = list(color = 'rgba(96, 1, 133, 0.9)', size = 5),
            fillcolor = list(color = 'rgba(160, 107, 181, 0.5)')
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
        
        
        #### Boxplot ----
        output$boxplot_equity <- renderPlot({
          extracted_df %>%
            rename(Pollution = pol_fz, Demographics = demo_fz, "Isolation from Nature" = access_fz) %>%
            pivot_longer(cols=c(Pollution, Demographics, "Isolation from Nature")) %>%
            group_by(name) %>%
            mutate(mean_value = mean(value)) %>%
            ungroup() %>%
            arrange(mean_value) %>%
            mutate(name = factor(name, levels = unique(name))) %>%
            ggplot(aes(x=name, y=value, fill=name)) +
            geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7,
                         outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
            scale_fill_manual(values=brewer.pal(n=4, name="Purples")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
            theme_minimal() +
            labs(x="", y="", title="DEIJ/EJ data distribution (without weighting):") +
            theme(plot.title = element_text(hjust = 9.5, vjust = -2,size=15,
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
  
  
  
  ## Return to the whole area if the map is refreshed  ----
  
  observeEvent(c(input$removeShapes_equity, input$pol_w,input$demo_w,input$access_w), {
    
    extracted_df <- cbind(pol_fz= as.numeric(equity_axis_r$pol_fz),
                          demo_fz= as.numeric(equity_axis_r$demo_fz),
                          access_fz= as.numeric(equity_axis_r$access_fz),
                          agg_val= as.numeric(equity_axis_r$agg_val)) %>%
      as.data.frame() %>%
      na.omit()
    
    mean_extracted_values <- apply(extracted_df,2,mean,na.rm=T)
    
    summary_data <- data.frame(Issue=c("Pollution","Demographics","Isolation from Nature","Aggregated score"),
                               Score= mean_extracted_values)
    rownames(summary_data)<-NULL
    
    ### Table ----
    output$mytable_equity =  render_gt({
      dplyr::tibble(img=c(here("www","img","pollution_icon.png"),
                          here("www","img","demo_icon.png"),
                          here("www","img","access_icon.png")),
                    summary_data %>%
                      filter(!Issue %in% "Aggregated score")) %>%
        arrange(desc(Score)) %>%
        gt() %>%
        fmt_number(columns = Score,decimals = 3) %>%
        cols_label(img = "") %>%
        gt_img_rows(columns = img, height = 28, img_source = "local") %>%
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
    
    ### Gauge ----
    output$gauge_equity = renderGauge({
      gauge(mean(as.numeric(weights_reactive_equity()$norm_score), na.rm=T),
            min = 0,
            max = 1,
            abbreviateDecimals=2,
            label ="Aggregated score",
            sectors = gaugeSectors(success = c(0.70, 1),
                                   warning = c(0.35, 0.70),
                                   danger = c(0, 0.35),
                                   colors = c("#5f0185","#845f94","#be9bcc"))
      )
    })
    
    ### Boxplot ----
    output$boxplot_equity <- renderPlot({
      extracted_df %>%
        rename(Pollution = pol_fz, Demographics = demo_fz, "Isolation from Nature" = access_fz) %>%
        pivot_longer(cols=c(Pollution, Demographics, "Isolation from Nature")) %>%
        group_by(name) %>%
        mutate(mean_value = mean(value)) %>%
        ungroup() %>%
        arrange(mean_value) %>%
        mutate(name = factor(name, levels = unique(name))) %>%
        ggplot(aes(x=name, y=value, fill=name)) +
        geom_boxplot(color="black", alpha=0.9, lwd=0.3, outlier.size=0.7,
                     outlier.stroke=0, outlier.alpha=0.5, outlier.color="black") +
        scale_fill_manual(values=brewer.pal(n=4, name="Purples")) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
        theme_minimal() +
        labs(x="", y="", title="DEIJ/EJ data distribution (without weighting):") +
        theme(plot.title = element_text(hjust = 9.5, vjust = -2,size=15,
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
    
    
    ### Radar graph ----
    output$radar_graph_equity <- renderPlotly({
      
      selected <- as.data.frame(df)
      
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.numeric(mean_extracted_values[1:3]),
        theta = c("Pollution", "Demographics", "Isolation"),
        fill = 'toself',
        marker = list(color = 'rgba(96, 1, 133, 0.9)', size = 5),
        fillcolor = list(color = 'rgba(160, 107, 181, 0.5)')
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
  
  
  ## Edit tab's note about data displayed ----
  observeEvent(c(input$alpha_equity, input$pol_w,input$demo_w,input$access_w), {
    output$data_displayed_note_summary_equity <- renderText({
      "Data for the entire region"
    })
    output$data_displayed_note_plot_equity <- renderText({
      "Data for the entire region"
    })
  })
  
  observeEvent(input$map_equity_draw_all_features, {
    output$data_displayed_note_summary_equity <- renderText({
      "Data for the selected area"
    })
    output$data_displayed_note_plot_equity <- renderText({
      "Data for the selected area"
    })
  })
  
  ## Select summary tab when stats are generated if About tab is open ----
  observeEvent(input$map_equity_draw_all_features, {
    if (input$tabs_equity == "about_equity") {
      # Add a new tab panel to the tabset
      updateTabsetPanel(session, "tabs_equity",
                        selected = paste0("summary_equity", input$controller)
      )
    }
  })
  
  ## Reset map to remove shapes ----
  # Uses the update of the alpha slider to force refresh (there must be a better solution)
  observeEvent(input$removeShapes_equity,{
    reset_value = input$alpha_equity
    updateSliderInput(session, "alpha_equity", value = reset_value+0.01)
  })
        
}
    
