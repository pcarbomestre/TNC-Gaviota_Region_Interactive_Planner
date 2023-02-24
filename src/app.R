#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(tmap)
library(sf)
library(here)
library(janitor)
library(caret)
library(shiny)
library(rmapshaper)
library(leaflet)
library(bslib)
library(shinyWidgets)
library(stars)


# Data input
# ----------------------

## Multi-benefit areas layer

# areas_sf <- st_read(here('data','SHP_Multi-benefit_Areas_Map',
#                         '0000Multi-benefit Areas Map (EEMS Synthesis b07 - Simple).shp')) %>%
#     clean_names() %>% 
#     ms_simplify(keep = 0.05, keep_shapes = TRUE)

areas_sf <- st_read(here('data','small_MB_area','small_MB_area.shp')) %>% 
    clean_names() %>% 
    st_transform("EPSG:4326")

areas_df <- st_drop_geometry(areas_sf)

## normalizing data using caret range method
range_norm <- function(new_score) {
    ss <- preProcess(as.data.frame(new_score), method=c("range"))
    gfg <- predict(ss, as.data.frame(new_score))
    return(gfg)
}

## Partner areas layer

jldp_sf <- st_read(here('data','jldp_boundary','jldp_boundary.shp')) %>%
    clean_names() %>% 
    st_transform("EPSG:4326")

jldp_df <- st_drop_geometry(jldp_sf)


mean_values <- st_intersection(areas_sf, jldp_sf) %>%
  st_drop_geometry() %>% 
  summarize(global_score = mean(eems_synth),
            agricultur_score = mean(agricultur),
            community_score = mean(community),
            bio_score = mean(flora_faun),
            water_score = mean(water_raw)) %>% 
  mutate(id=jldp_sf$global_id)



# Map setup
# ----------------------
tmap_mode(mode = "view")
tmap_options(check.and.fix = TRUE)

pal <- colorNumeric("Greens", areas_sf$eems_synth) # pallete for default leaflet

# Create a custom theme
# ----------------------
my_theme <- bs_theme(
    bg = "#FFFFFF",
    fg = " #598048 ",
    primary = "#EC255A",
    base_font = font_google('Avenir')
)

# Define UI for application that displays the map
# ----------------------
ui <- fluidPage(theme=my_theme,
                navbarPage("Interactive planner",
                           # Application title
                           tabPanel("Interactive planner",fluid = TRUE, icon = icon("map"),
                                    fluidRow(
                                        column(
                                        p("The Santa Barbara Countys Interactive Planner allows you to adjust weights given to each of the four 
                                        Conservation Goals to create an integrated assessment map that reflects your unique vision for the 
                                        region priorities. It may also be used to reveal areas for potential collaborations with partners 
                                        whose organizations may focus on different goals, or explore areas where funding for a given goal may be 
                                        leveraged to achieve conservation of other goals.",
                                          style="text-align:justify;color:black;padding:15px;border-radius:5px; width: 1350px; align: center"),
                                        width=4)
                                        ),
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                        sidebarPanel(
                                            setSliderColor(c("#629871", "#629871", "#629871", "#629871","#97af9e"), c(1,2,3,4,5)),
                                            sliderInput(inputId = "water_w",
                                                        label = "Water resources",
                                                        min = 0,
                                                        max = 10,
                                                        value = 5,
                                            ), # End sliderInput
                                            
                                            sliderInput(inputId = "agriculture_w",
                                                        label = "Agricultural use",
                                                        min = 0,
                                                        max = 10,
                                                        value = 5,
                                            ), # End sliderInput
                                            
                                            sliderInput(inputId = "biodiversity_w",
                                                        label = "Biodiversity",
                                                        min = 0,
                                                        max = 10,
                                                        value = 5,
                                            ), # End sliderInput
                                            
                                            sliderInput(inputId = "community_w",
                                                        label = "Community resources",
                                                        min = 0,
                                                        max = 10,
                                                        value = 5,
                                            ), # End sliderInput
                                            
                                            sliderInput(inputId = "alpha",
                                                        label = "Transparency",
                                                        min = 0,
                                                        max = 1,
                                                        value = 0.9,
                                            ), # End sliderInput
                                            
                                            checkboxInput("include_areas","Include areas", FALSE
                                            ), # end checkbox
                                            
                                            actionButton("run", "Apply weights")
                                            
                                        ), # End sidebarPanel
                                
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                                   # plotOutput("planner_map"))
                                                   leafletOutput("myMap"),
                                                   br(),
                                                   splitLayout(cellWidths = c("50%", "50%"),
                                                               tableOutput("mytable"),
                                                               plotOutput("plot")
                                                               # tmapOutput(outputId = "planner_map"), width = 8, height = 100)
                                                               )
                                                   )
                                        )
                                    ),
                                    tabPanel(title=HTML("</a></li><li><a href='https://sbcblueprint.databasin.org/articles/2d84c23b3dfb4912b073353cbec576db/'>Data information"
                                                       )
                                             )
                                    )
                )
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    weights_reactive <- eventReactive(input$run,{
        
       areas_df %>%
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
        
        left_join(areas_sf, weights_reactive(), by = 'objectid')
        
    }) # end map_reactive
    
   
 
    # output$planner_map <- renderTmap({
    # output$planner_map <- renderPlot({   
    output$myMap <- renderLeaflet({
        
        if (!input$run) {
            # default plot
            
            leaflet(areas_sf) %>% 
                addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
                            color = "grey",
                            fillColor = ~pal(eems_synth)) %>% 
                addProviderTiles(providers$Stamen.Terrain) %>% 
                addLegend("bottomright", pal = pal, values = ~areas_sf$eems_synth,
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
           addPolygons(stroke = FALSE, fillOpacity = input$alpha, smoothFactor = 0.5,
                        color = "grey",
                        fillColor = ~pallete_reactive()(norm_score)) %>% 
            addProviderTiles(providers$Stamen.Terrain) %>% 
            addLegend("bottomright", pal = pallete_reactive(), values = ~map_reactive()$norm_score,
                      title = "Score",
                      opacity = 1)
       
       if (input$include_areas) {
           map <- map %>% 
               addPolygons(data=jldp_sf,color = "#444444", 
                           weight = 0.5, smoothFactor = 0.5,
                           opacity = 0.8, fillOpacity = 0.3,
                           highlightOptions = highlightOptions(color = "white", 
                                                               weight = 1,
                                                               bringToFront = TRUE),
                           layerId = jldp_sf$global_id)
       }
       map
       
        }
    }) # end output$planner_map
    
    # reactive table content (shape details)
    
   
    
    table_reactive <- reactive({
        
        mean_weighted_score <-st_intersection(map_reactive(), jldp_sf) %>%
          st_drop_geometry() %>% 
          summarize(global_score = mean(norm_score))
        
        mean_original_values <-st_intersection(areas_sf, jldp_sf) %>%
          st_drop_geometry() %>% 
          summarize(agricultur_score = mean(agricultur),
                    community_score = mean(community),
                    bio_score = mean(flora_faun),
                    water_score = mean(water_raw)) %>% 
          mutate(global_id=jldp_sf$global_id)
        
        mean_values <- cbind(mean_weighted_score,
                             mean_original_values)
        t(mean_values)
        
    }) # end table_reactive
    
    
    observeEvent(input$myMap_shape_click, {
        
        #capture the info of the clicked polygon
        click <- input$myMap_shape_click
        
        #subset your table with the id of the clicked polygon 
        selected <- mean_values[mean_values$id == click$id,]
        
        #if click id isn't null render the table
        if(!is.null(click$id)){
            output$mytable = renderTable({
              selected <- as.data.frame(selected)
              rownames(selected) <- "Mean score"
              cbind(c("Multi-benefit",
                      "Agriculture",
                      "Community",
                      "Biodiversity",
                      "Water"), round(t(selected[-6]),3))
            }) 
        }
        if(!is.null(click$id)){
            output$plot = renderPlot({
                plot(st_intersection(areas_sf, jldp_sf)[,c(2,3,6,7)])
                })
        }
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)