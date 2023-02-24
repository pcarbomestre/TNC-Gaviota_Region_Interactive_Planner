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
        
        resources_axis_df %>%
            mutate_at(vars(agricultur), function(x) input$agriculture_w * x) %>% 
            mutate_at(vars(community), function(x) input$community_w * x) %>% 
            mutate_at(vars(flora_faun), function(x) input$biodiversity_w * x) %>% 
            mutate_at(vars(water_raw), function(x) input$water_w * x) %>% 
            mutate(score = agricultur+community+flora_faun+water_raw) %>% 
            mutate(norm_score = range_norm(score)[,1],.after = objectid) %>% 
            select(c(objectid,norm_score))
        
    }) # end weights_reactive
    
    
    pallete_reactive <- reactive({
        
        colorNumeric("Greens", weights_reactive()$norm_score) # pallete for leaflet
        
    }) # end pallete_reactive
    
    map_reactive <- reactive({

        left_join(resources_axis_sf, weights_reactive(), by = 'objectid')
        
    }) # end map_reactive
    
    pal <- colorNumeric("Greens", resources_axis_sf$eems_synth) # pallete for default leaflet
    
    output$map <- renderLeaflet({
        
        if (!input$run) {
        # default plot
        
        leaflet(resources_axis_sf) %>% 
                setView(lng = -120.3, lat = 34.53, zoom =10 ) %>%
                addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
                        color = "grey",
                        fillColor = ~pal(eems_synth)) %>% 
            addProviderTiles(providers$Stamen.Terrain) %>% 
            addLegend("bottomright", pal = pal, values = ~resources_axis_sf$eems_synth,
                      title = "Score",
                      opacity = 1)
        
    } else{
        
        # ggplot2::ggplot(data =  map_reactive()) +
        # geom_sf(aes(fill=norm_score),lwd = 0)
        
        # tm_shape(shp = map_reactive()) +
        #     tm_fill(col = 'norm_score',
        #             title = "Area of interest",
        #             style = 'cont',
        #             alpha= input$alpha) +
        #     tm_basemap(leaflet::providers$Stamen.Terrain) # when changing parameters it loss the  previous zoom and position
        
        map <-  leaflet(map_reactive()) %>% 
            setView(lng = -120.3, lat = 34.53, zoom =10 ) %>%
            addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
                        color = "grey",
                        fillColor = ~pallete_reactive()(norm_score)) %>% 
            addProviderTiles(providers$Stamen.Terrain) %>% 
            addLegend("bottomright", pal = pallete_reactive(), values = ~map_reactive()$norm_score,
                      title = "Score",
                      opacity = 1)
    }
    })
    
     
    
       
}
    
