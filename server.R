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
 
    #Leaflet section
 
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
                               editOptions = editToolbarOptions(
                                   selectedPathOptions = selectedPathOptions()),
                               position = "topright") 
            
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
        
    } else{
        
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
                           editOptions = editToolbarOptions(
                               selectedPathOptions = selectedPathOptions()),
                           position = "topright") 
        
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
    }
    })
    
     
    
       
}
    
