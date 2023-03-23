## _____________________________
### File: ui.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## _____________________________
## Description:
##  Fluid page for the Interactive Planner app
## _____________________________

#Global displaying options
options(spinner.color="#bdbfbe",
        spinner.color.background = "#929493",
        spinner.type=8)

# Header ------------------------------------------------------------------
header<- dashboardHeader(title = HTML("Gaviota Region Interactive planner"),
                         disable = FALSE,
                         titleWidth = 500,
                         dropdownMenuCustom( type = 'message',
                                               customSentence = customSentence_share,
                                               icon = icon("share-alt"),
                                               messageItem(
                                                 from = 'Twitter',
                                                 message = "",
                                                 icon = icon("twitter")#,
                                                 #href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               ),
                                               messageItem(
                                                 from = 'Facebook',
                                                 message = "",
                                                 icon = icon("facebook")#,
                                                 #href = #"https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                               ),
                                               messageItem(
                                                 from = 'LinkedIn',
                                                 message = "",
                                                 icon = icon("linkedin")#,
                                                 #href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               ))
                         
                         )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.nature.org/en-us/about-us/where-we-work/united-states/california/stories-in-california/dangermond-preserve/',
                                             tags$img(src='/img/TNC_logo.png',style="width: 125px", align = 'left'),
                                             target = '_blank')


# Siderbar ------------------------------------------------------------------
siderbar<- dashboardSidebar(
  # introBox(data.step = 1, data.intro = intro$text[1], data.position="right", #  intro tour
           div(class="inlay",style = "height:100%;width:100%;background-color: #ecf0f5;"),
  sidebarMenu(
    menuItem("Resources Axis", tabName = "Initial", icon = icon("fa-brands fa-pagelines"),
             menuSubItem("Natural Resources",tabName = "natural_resources_map", icon = icon("map")),
             menuSubItem("Stakeholders priorities",tabName = "environmental_stake_map", icon = icon("fa-solid fa-layer-group")),
             menuSubItem("Data Information",tabName = "data_information_resources", icon = icon("info-circle"))),
    menuItem("Threats Axis",tabName = "landings", icon = icon("fa-regular fa-fire"),
             menuSubItem("Environmental Threats",tabName = "environmental_threats_map", icon = icon("map")),
             menuSubItem("Data Information",tabName = "data_information_threats", icon = icon("info-circle"))),
    menuItem("DEI/EJ Axis",tabName = "Assessment", icon = icon("fa-duotone fa-people-arrows"),
             menuSubItem("DEI/EJ Issues",tabName = "equity_issues_map", icon = icon("map")),
             menuSubItem("Data Information",tabName = "data_information_equity", icon = icon("info-circle"))),
    menuItem("Other Information",tabName = "other_information", icon = icon("info-circle"))
    )
  #)
  )

# Body ------------------------------------------------------------------
body <- dashboardBody(
  # 
  # tags$head( # must include css
  #   tags$style(HTML("
  #       .img-local {
  #       }
  #       
  #       .small-box .img-local {
  #       position: absolute;
  #       top: auto;
  #       bottom: -25px;
  #       right: 5px;
  #       z-index: 0;
  #       font-size: 70px;
  #       color: rgba(0, 0, 0, 0.15);
  #       }"
  #   ))
  # ),
  
  useShinyjs(),
  introjsUI(),  
  
  tabItems(
    ## NATURAL RESOURCES ------------------------------------------------------------------
    tabItem(tabName = "natural_resources_map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map", height='60vh')),
                     ### Transparency panel ----
                     absolutePanel(id = "transparency_control",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 50,
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",
                                   setSliderColor("#97af9e",1),

                                   sliderInput(inputId = "alpha",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 1,
                                               value = 0.9), # End sliderInput
                     ),

                     absolutePanel(id = "transparency_control_title",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 20,
                                   style="background-color: rgba(0,0,0,0);
                                          border-color: rgba(0,0,0,0);
                                          border-style: hidden;
                                          box-shadow: 0px;
                                          padding-left: 20px;
                                          padding-bottom: 20px;
                                          padding-top: 5px;
                                          color: #000000;
                                          font-weight: bold;",
                                   tags$p("Transparency",
                                          style="text-align: justify")
                     ),

                     ### Sliders panel ----
                     absolutePanel(id = "controls",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = TRUE,
                                   top = 140, left = 40,
                                   right = "auto", bottom = "auto",
                                   width = 250, height = "auto",
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",

                       tags$head(
                       # Using ionRangeSlider's javascript options you can hide/show selector labels and min/max labels
                       HTML("
                                     <script>
                                      $(document).ready(function(){
                                          $(\".js-range-slider\").ionRangeSlider({
                                          hide_min_max: false,
                                          hide_from_to: true
                                          });

                                      });

                                      </script>
                                      "),
                       # Avoid scroll THIS SHOULD BE TEMPORARY, REMOVE FOR THE FINAL VERSION
                       # tags$style("body {overflow-y: hidden;}")
                     ),

                     # This CSS hack first hides the text within the span tags of the specified classes
                     # Then it adds desired text and CSS properties. !important parameter is to override
                     # inline css parameters.
                     tags$style(HTML(
                                ".irs-min {visibility:hidden !important;}
                                    .irs-max {visibility:hidden !important;}
                                    .js-irs-1 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-1 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-2 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-2 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-3 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-3 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-4 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-4 .irs .irs-max:after {content:'Highest' !important;}
                                    .irs-min:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        left: 0;
                                        position: absolute;}

                                    .irs-max:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        right: 0;
                                        position: absolute;}

                                ")),



                                     tags$style(HTML("
                                     a.action-button {color: #000000;font-weight: bold;}
                                     #checkbox1{padding-left: 25px;}
                                                     ")),
                                   # Set slides color
                                   setSliderColor(rep("#629871", 5), c(2:5)),

                     #### Sliders and checkbox set up ----
                     fluidRow(
                       column(10,

                                   actionLink("checkbox_water", label = "Water resources",
                                              style="display: block;padding-top: 15px;}"),
                                   sliderInput(inputId = "water_w",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 100,
                                               value = 25), # End sliderInput
                               ),
                               column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                      br(),
                                      textOutput("water_agg_pref")
                                      )
                       ),

                     fluidRow(
                       column(10,
                                   actionLink("checkbox_agri", label = "Soil"),
                                   sliderInput(inputId = "soil_w",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 100,
                                               value = 25), # End sliderInput
                       ),
                       column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                              br(),
                              textOutput("soil_agg_pref")
                       )
                     ),
                     fluidRow(
                       column(10,
                                   actionLink("checkbox_bio", label = "Biodiversity"),
                                   sliderInput(inputId = "biodiversity_w",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 100,
                                               value = 25), # End sliderInput
                       ),
                       column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                              br(),
                              textOutput("biodiversity_agg_pref")
                       )
                     ),
                     fluidRow(
                       column(10,
                                   actionLink("checkbox_com", label = "Resilience"),
                                   sliderInput(inputId = "resil_w",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 100,
                                               value = 25), # End sliderInput
                                   ),
                       column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                              br(),
                              textOutput("resilience_agg_pref")
                       )
                     )
                     ),

                     ### Remove shapes button ----
                     absolutePanel(id = "removeShapes",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 125, left = "auto",
                                   right = 25, bottom = "auto",
                                   actionButton("removeShapes","", icon = icon("fa-regular fa-trash"))
                     ),


                     ### Select Picker and planner use note ----
                     
                     column(5,offset = 0, style='padding-left:30px;padding-right:30px;',
                            br(),
                            selectInput("stakeholder_w", label = "Select stakeholder's weights:",
                                        choices = c("None",ahp_weights$group),
                                        selected = NULL),
                            ### Text Information ----
                            tags$div(
                              style = "display: flex; align-items: center; margin-top: 5px; margin-bottom: 5px;",
                              tags$h4(
                                style = "text-align: left; margin-right: 10px; margin-bottom: 0;",
                                tags$mark("Natural resources map",style = "color:#ffffff;
                            background-color: #629871;
                            border-radius: 0px;
                            padding: 3px 10px;
                            font-weight: bold;
                            position:relative;
                            filter:url(#marker-shape);
                            width:100%;
                            height:1em;
                            left:-0.25em;
                            top:0.1em;
                            padding:0 0.25em;
                            font-family: 'Chronicle Text G2 A', 'Chronicle Text G2 B', 
                            'Chronicle Text G2', Georgia, sans-serif")
                              ),
                              tags$h4("How to use it?", style = "font-weight: bold ;margin-bottom: 0; margin-top: 2;")
                            ),
                            tags$p("The Gaviota Region Interactive Planner maps the degree of overlap of natural resources. 
                                   Darker areas have more resources. 
                                   Use the sliders to adjust the relative weight of each item. 
                                   Select higher values for the resources you prioritize more.", 
                                   style="text-align: justify; margin-top: 15px; margin-bottom: 5px;")
                     )
                     
                     ,
                  ### Data tab ----
               column(7,offset = 0,
                      tabsetPanel(id="tabs",
                        tags$style(HTML(".tabbable > .nav > li > a {margin-top:5px;float:right;display:inline-block;}")),
                        tags$style(HTML(
                          ".tabbable ul li:nth-child(4) { float: right;}
                          .tabbable ul li:nth-child(3) { float: right;}
                          .tabbable ul li:nth-child(5) { float: right;}"
                        )),

                        #### About tab ----
                        tabPanel(value = "about",title="About",
                                 tags$style("code {
                        color:#303e52;
                                   background-color: #c1d7f5;
                                     border-radius: 3px;
                                   padding: 0 3px;
                                 }"),
                                 h3("Area statistics:"),
                                 HTML("<h4 style=text-align: left>To extract statistics from your area of interest, draw a shape on the map.</h4>"),
                                 HTML('<p align= "justify">The <code>Summary</code> tab displays the average score of each available resource, along with the combined score calculated by applying weights.
                                      The <code>Plot</code> tab shows the actual scores of each resource graphically and offers insights into the data distribution within the selected area. 
                                      When no area is selected, the values for the entire region of interest are displayed.</p>')
                        ),
                        #### Summary tab ----
                        tabPanel(value = "summary", title ="Summary",
                                 textOutput("data_displayed_note_summary"),
                                        tags$head(tags$style("#data_displayed_note_summary{margin-top: -1.6em;
                                           margin-left: 1em;
                                          margin-bottom: 0.1em;
                                           color: #808080;
                                           font-size:12px;
                                           font-style: italic;}"
                                        )),
                                 fluidRow(column(7,
                                                 gt_output("mytable")
                                                 ),
                                          column(5,
                                                 h4("Aggregated Score:",
                                                    style="color: #808080;
                                                    padding-top: 5px;"),
                                                 br(),
                                                 gaugeOutput("gauge")),
                                 )),
                        #### Plot tab ----
                        tabPanel("Plot",
                                 textOutput("data_displayed_note_plot"),
                                 tags$head(tags$style("#data_displayed_note_plot{margin-top: -1.6em;
                                           margin-left: 1em;
                                          margin-bottom: 0.1em;
                                           color: #808080;
                                           font-size:12px;
                                           font-style: italic;}"
                                 )),
                                 fluidRow(column(7,
                                                 plotOutput("boxplot",inline=T, height = 210)
                                                 ),
                                          column(5,
                                                 br(),
                                                 plotlyOutput("radar_graph", inline=T, height = 190),
                                                 align="right")
                                 ))
                        )),
              )
              )
            ),
    
    ### STAKE HOLDERS ----
    tabItem(tabName = "environmental_stake_map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map_stake", height='60vh')),
                     ### Transparency panel ----
                     absolutePanel(id = "transparency_control_stake",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 30, bottom = "auto",
                                   width = 200, height = 50,
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",
                                   setSliderColor("#97af9e",5),
                                   sliderInput(inputId = "alpha_stake",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 1,
                                               value = 0.9), # End sliderInput
                     ),
                     
                     absolutePanel(id = "transparency_control_title_stake",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 30, bottom = "auto",
                                   width = 200, height = 20,
                                   style="background-color: rgba(0,0,0,0);
                                          border-color: rgba(0,0,0,0);
                                          border-style: hidden;
                                          box-shadow: 0px;
                                          padding-left: 20px;
                                          padding-bottom: 20px;
                                          padding-top: 5px;
                                          color: #000000;
                                          font-weight: bold;",
                                   tags$p("Transparency",
                                          style="text-align: justify")
                     ),

                     
                     
                     ### Text Information ----
                     column(7,offset = 0, style='padding-left:30px;padding-right:30px;padding-top:15px',
                            br(),
                            tags$div(
                              style = "display: flex; align-items: center; margin-top: 5px; margin-bottom: 5px;",
                              tags$h4(
                                style = "text-align: left; margin-right: 10px; margin-bottom: 0;",
                                tags$mark("Comparison of Resource Priorities",style = "color:#ffffff;
                            background-color: #054f96;
                            border-radius: 0px;
                            padding: 3px 10px;
                            font-weight: bold;
                            position:relative;
                            filter:url(#marker-shape);
                            width:100%;
                            height:1em;
                            left:-0.25em;
                            top:0.1em;
                            padding:0 0.25em;
                            font-family: 'Chronicle Text G2 A', 'Chronicle Text G2 B', 
                            'Chronicle Text G2', Georgia, sans-serif")
                              ),
                              tags$h4("What does this map represent?", style = "font-weight: bold ;margin-bottom: 0; margin-top: 1;")
                            ),
                            tags$p("This map allows for the spatial comparison of resource priorities for various stakeholder or rightsholder groups. 
                            Users can select two stakeholder/rightsholder groups and the map will display the Gaviota Region colored by relative preference between those groups. 
                            In addition, the user can compare their own resource preference values, selected in the previous tab, to a group of their choice. 
                            Select the groups in the dropdown menus to compare custom resource values. 
", 
                                   style="text-align: justify; margin-top: 15px; margin-bottom: 5px;")
                     ),
                     ### Data tab ----
                     column(5,offset = 0, align="center",
                            style='padding-left:30px;padding-right:30px;padding-top:20px;margin-top:10px;',
                            fluidRow(
                              column(3,
                                     "Compare", style= "font-weight: bold; font-size:15pt; margin-left:50px;"
                                     ),
                              column(4,align="left",
                                selectInput("group_1", label = NULL,
                                            choices = c("My",unique(ahp_weights$group)),
                                            selected = unique(ahp_weights$group)[5]),
                                style ="margin-left:-20px;"
                              ),
                              column(2,
                                    "preferences",
                                    style= "font-weight: bold; font-size:15pt; margin-left:-15px;"
                              )),
                            fluidRow(
                              column(5,
                                     "with those from",
                                     style= "font-weight: bold; font-size:15pt;margin-top:-10px;margin-left:70px;margin-bottom:-10px"
                              ),
                              column(1, align="left",
                                     selectInput("group_2", label = NULL,
                                                 choices = c("My",unique(ahp_weights$group)),
                                                 selected = unique(ahp_weights$group)[4]),
                                     style= "margin-top:-10px;margin-left:-25px; margin-bottom:-10px"
                              ),
                              
                              tags$head(tags$style(HTML('.selectize-input {white-space: nowrap}
                                                                     #group_1+ div>.selectize-input{
                                                                     width: 130px;
                                                                     line-height: 10px;
                                                                     min-height: 25px;
                                                                     border:0;
                                                                     border-radius: 0;
                                                                     padding: 6px 5px 5px 5px;
                                                                     background-color: #054f96;
                                                                     color: white; !important}
                                                                     #group_2+ div>.selectize-input{
                                                                     width: 130px;
                                                                     line-height: 10px;
                                                                     min-height: 25px;
                                                                     border:0;
                                                                     border-radius: 0;
                                                                     padding: 6px 5px 5px 5px;
                                                                     background-color: #a80d0d;
                                                                     color: white; !important}')))
                            ),
                            tags$p("Interpretation:",style="text-align: justify; margin-top: 5px; margin-bottom: -5px; font-weight:bold;"),
                            tags$p("White represents areas that are equally important to the two selected groups. 
                            Colors at each end of the spectrum represent areas that are relatively much more important to one selected group compared to the other selected group.", 
                                   style="text-align: justify; margin-top: 5px; margin-bottom: 5px;")
                     )
              )
            )
    ),
    
    ### DATA INFORMATION ----
    tabItem(tabName = "data_information_resources",
            fluidPage(
              htmltools::tags$iframe(src = "data_information_resources.html", width = '100%',  height = 1000,  style = "border:none;"))
    ),
    
    
   
    
    
    ## ENVIRONMENTAL THREATS ------------------------------------------------------------------
    tabItem(tabName = "environmental_threats_map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map_threats", height='60vh')),
                     ### Transparency panel ----
                     absolutePanel(id = "transparency_control_threats",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 50,
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",

                                   sliderInput(inputId = "alpha_threats",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 1,
                                               value = 0.9), # End sliderInput
                     ),

                     absolutePanel(id = "transparency_control_title_threats",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 20,
                                   style="background-color: rgba(0,0,0,0);
                                          border-color: rgba(0,0,0,0);
                                          border-style: hidden;
                                          box-shadow: 0px;
                                          padding-left: 20px;
                                          padding-bottom: 20px;
                                          padding-top: 5px;
                                          color: #000000;
                                          font-weight: bold;",
                                   tags$p("Transparency",
                                          style="text-align: justify")
                     ),

                     ### Sliders panel ----
                     absolutePanel(id = "controls_threats",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = TRUE,
                                   top = 140, left = 40,
                                   right = "auto", bottom = "auto",
                                   width = 250, height = "auto",
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",

                                   tags$head(
                                     # Using ionRangeSlider's javascript options you can hide/show selector labels and min/max labels
                                     HTML("
                                     <script>
                                      $(document).ready(function(){
                                          $(\".js-range-slider\").ionRangeSlider({
                                          hide_min_max: false,
                                          hide_from_to: true
                                          });

                                      });

                                      </script>
                                      "),
                                     # Avoid scroll THIS SHOULD BE TEMPORARY, REMOVE FOR THE FINAL VERSION
                                     # tags$style("body {overflow-y: hidden;}")
                                   ),

                                   # This CSS hack first hides the text within the span tags of the specified classes
                                   # Then it adds desired text and CSS properties. !important parameter is to override
                                   # inline css parameters.
                                   tags$style(HTML(
                                     ".irs-min {visibility:hidden !important;}
                                    .irs-max {visibility:hidden !important;}
                                    .js-irs-7 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-7 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-8 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-8 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-9 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-9 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-10 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-1o .irs .irs-max:after {content:'Highest' !important;}
                                    .irs-min:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        left: 0;
                                        position: absolute;}

                                    .irs-max:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        right: 0;
                                        position: absolute;}

                                ")),



                                   tags$style(HTML("
                                     a.action-button {color: #000000;font-weight: bold;}
                                     #checkbox1{padding-left: 25px;}
                                                     ")),
                                  # Set slides color
                                  setSliderColor(rep("#f7a540", 11), c(7:111)),
                                   #### Sliders and checkbox set up ----
                                   fluidRow(
                                     column(10,
                                            actionLink("checkbox_climate", label = "Climate exposure",
                                                       style="display: block;padding-top: 15px;}"),
                                            sliderInput(inputId = "cli_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 25), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("cli_agg_pref")
                                     )
                                   ),

                                   fluidRow(
                                     column(10,
                                            fluidRow(
                                              column(4,
                                            actionLink("checkbox_droughts", label = "Droughts"),
                                            style = "margin-top:0px;margin-bottom:-12px"),
                                            column(7,
                                            selectInput("model_drgh",label=NULL,
                                                        choices = c("(Ensemble)","(MIROC-esm)","(CCSM4)"),
                                                        selected = NULL),
                                           tags$head(tags$style(HTML('.selectize-input {white-space: nowrap}
                                                                     #model_drgh+ div>.selectize-input{
                                                                     width: 90px;
                                                                     font-size: 8pt;
                                                                     line-height: 10px;
                                                                     min-height: 5px;
                                                                     border:0;
                                                                     padding: 0px 0px 0px 0px; !important}
                                                                     #model_drgh+ div>.selectize-dropdown{font-size: 8pt;  !important}'))),
                                           style = "margin-top:0px;margin-bottom:-12px"),
                                           ),
                                            sliderInput(inputId = "drgh_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 25), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("drgh_agg_pref")
                                     )
                                   ),
                                   fluidRow(
                                     column(10,
                                            fluidRow(
                                              column(3,
                                                     actionLink("checkbox_floods", label = "Floods"),
                                                     style = "margin-top:0px;margin-bottom:-12px"),
                                              column(7,
                                                     selectInput("model_fld",label=NULL,
                                                                 choices = c("(Ensemble)","(MIROC-esm)","(CCSM4)"),
                                                                 selected = NULL),
                                                     tags$head(tags$style(HTML('.selectize-input {white-space: nowrap}
                                                                     #model_fld+ div>.selectize-input{
                                                                     width: 90px;
                                                                     font-size: 8pt;
                                                                     line-height: 10px;
                                                                     min-height: 5px;
                                                                     border:0;
                                                                     padding: 0px 0px 0px 0px; !important}
                                                                     #model_fld+ div>.selectize-dropdown{font-size: 8pt;  !important}'))),
                                                     style = "margin-top:0px;margin-bottom:-12px"),
                                            ),
                                            sliderInput(inputId = "fld_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 25), # End sliderInput
                                            ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("fld_agg_pref")
                                     )
                                   ),
                                   fluidRow(
                                     column(10,
                                            fluidRow(
                                              column(4,
                                                     actionLink("checkbox_wildfires", label = "Wildfires"),
                                                     style = "margin-top:0px;margin-bottom:-12px"),
                                              column(7,
                                                     selectInput("model_wf",label=NULL,
                                                                 choices = c("(Ensemble)","(MIROC5)","(CNRM-CM5)"),
                                                                 selected = NULL),
                                                     tags$head(tags$style(HTML('.selectize-input {white-space: nowrap}
                                                                     #model_wf+ div>.selectize-input{
                                                                     width: 90px;
                                                                     font-size: 8pt;
                                                                     line-height: 10px;
                                                                     min-height: 5px;
                                                                     border:0;
                                                                     padding: 0px 0px 0px 0px; !important}
                                                                     #model_wf+ div>.selectize-dropdown{font-size: 8pt;  !important}'))),
                                                     style = "margin-top:0px;margin-bottom:-12px"),
                                            ),
                                            sliderInput(inputId = "wf_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 25), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("wf_agg_pref")
                                     )
                                   )
                     ),

                     ### Remove shapes button ----
                     absolutePanel(id = "removeShapes_threats",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 125, left = "auto",
                                   right = 25, bottom = "auto",
                                   actionButton("removeShapes_threats","", icon = icon("fa-regular fa-trash"))
                     ),


                     ### Text Information ----
                     column(5,offset = 0, style='padding-left:30px;padding-right:30px;padding-top:40px',
                            br(),
                            tags$div(
                              style = "display: flex; align-items: center; margin-top: 5px; margin-bottom: 5px;",
                              tags$h4(
                                style = "text-align: left; margin-right: 10px; margin-bottom: 0;",
                                tags$mark("Environmental threats map",style = "color:#ffffff;
                            background-color: #f78800;
                            border-radius: 0px;
                            padding: 3px 10px;
                            font-weight: bold;
                            position:relative;
                            filter:url(#marker-shape);
                            width:100%;
                            height:1em;
                            left:-0.25em;
                            top:0.1em;
                            padding:0 0.25em;
                            font-family: 'Chronicle Text G2 A', 'Chronicle Text G2 B', 
                            'Chronicle Text G2', Georgia, sans-serif")
                              ),
                              tags$h4("How to use it?", style = "font-weight: bold ;margin-bottom: 0; margin-top: 1;")
                            ),
                            tags$p("This map shows the degree of overlap of environmental threats. 
                            Each category represents a projection estimate for the next 20-30 years. 
                            Areas where threats concentrate are shown darker, suggesting that they are currently or will be in the future more susceptible to these hazards. 
                            You can use the sliders to adjust the relative weight of each item that you want to display.", 
                                   style="text-align: justify; margin-top: 15px; margin-bottom: 5px;")
                     ),
                     ### Data tab ----
                     column(7,
                            tabsetPanel(id="tabs_threats",
                                        tags$style(HTML(".tabbable > .nav > li > a {margin-top:5px;float:right;display:inline-block;}")),
                                        tags$style(HTML(
                                          ".tabbable ul li:nth-child(4) { float: right;}
                          .tabbable ul li:nth-child(3) { float: right;}
                          .tabbable ul li:nth-child(5) { float: right;}"
                                        )),

                                        #### About tab ----
                                        tabPanel(value = "about_threats",title="About",
                                                 tags$style("code {
                                                 color:#303e52;
                                                 background-color: #c1d7f5;
                                                 border-radius: 3px;
                                                padding: 0 3px;}"),
                                                 h3("Area statistics:"),
                                                 HTML("<h4 style=text-align: left>To extract statistics from your area of interest, draw a shape on the map.</h4>"),
                                                 HTML('<p align= "justify">The <code>Summary</code> tab displays the average score of each available resource, along with the combined score calculated by applying weights.
                                                 The <code>Plot</code> tab shows the actual scores of each resource graphically and offers insights into the data distribution within the selected area. 
                                                      When no area is selected, the values for the entire region of interest are displayed.</p>')
                                        ),
                                        #### Summary tab ----
                                        tabPanel(value = "summary_threats", title ="Summary",
                                                 textOutput("data_displayed_note_summary_threats"),
                                                 tags$head(tags$style("#data_displayed_note_summary_threats{margin-top: -1.6em;
                                           margin-left: 1em;
                                          margin-bottom: 0.1em;
                                           color: #808080;
                                           font-size:12px;
                                           font-style: italic;}"
                                                 )),
                                                 fluidRow(column(7,
                                                                 gt_output("mytable_threats")
                                                 ),
                                                 column(5,
                                                        h4("Aggregated Score:",
                                                           style="color: #808080;
                                                    padding-top: 5px;"),
                                                        br(),
                                                        gaugeOutput("gauge_threats")),
                                                 )),
                                        #### Plot tab ----
                                        tabPanel(value = "plot_threats", title ="Plot",
                                                 textOutput("data_displayed_note_plot_threats"),
                                                 tags$head(tags$style("#data_displayed_note_plot_threats{
                                                 margin-top: -1.6em;
                                                 margin-left: 1em;
                                                 margin-bottom: 0.1em;
                                                 color: #808080;
                                                 font-size:12px;
                                                 font-style: italic;}"
                                                 )),
                                                 fluidRow(column(6,
                                                                 plotOutput("boxplot_threats",inline=T, height = 210)
                                                 ),
                                                 column(6,
                                                        br(),
                                                        plotlyOutput("radar_graph_threats", inline=T, height = 190),
                                                        align="right")
                                                 ))
                            )),
              )
            )
    ),

    ### DATA INFORMATION ----
    tabItem(tabName = "data_information_threats",
            fluidPage(
              htmltools::tags$iframe(src = "data_information_threats.html", width = '100%',  height = 1000,  style = "border:none;"))
    ),

    ## EQUITY ISSUES ------------------------------------------------------------------
    tabItem(tabName = "equity_issues_map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map_equity", height='60vh')),
                     ### Transparency panel ----
                     absolutePanel(id = "transparency_control_equity",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 50,
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",
                                   
                                   sliderInput(inputId = "alpha_equity",
                                               label = NULL,
                                               ticks = FALSE,
                                               min = 0,
                                               max = 1,
                                               value = 0.9), # End sliderInput
                     ),
                     
                     absolutePanel(id = "transparency_control_title_equity",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 12, left = "auto",
                                   right = 70, bottom = "auto",
                                   width = 200, height = 20,
                                   style="background-color: rgba(0,0,0,0);
                                          border-color: rgba(0,0,0,0);
                                          border-style: hidden;
                                          box-shadow: 0px;
                                          padding-left: 20px;
                                          padding-bottom: 20px;
                                          padding-top: 5px;
                                          color: #000000;
                                          font-weight: bold;",
                                   tags$p("Transparency",
                                          style="text-align: justify")
                     ),
                     
                     ### Sliders panel ----
                     absolutePanel(id = "controls_equity",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = TRUE,
                                   top = 210, left = 40,
                                   right = "auto", bottom = "auto",
                                   width = 250, height = "auto",
                                   style="background-color: white;
                                          opacity: 0.95;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",
                                   
                                   tags$head(
                                     # Using ionRangeSlider's javascript options you can hide/show selector labels and min/max labels
                                     HTML("
                                     <script>
                                      $(document).ready(function(){
                                          $(\".js-range-slider\").ionRangeSlider({
                                          hide_min_max: false,
                                          hide_from_to: true
                                          });

                                      });

                                      </script>
                                      "),
                                     # Avoid scroll THIS SHOULD BE TEMPORARY, REMOVE FOR THE FINAL VERSION
                                     # tags$style("body {overflow-y: hidden;}")
                                   ),
                                   
                                   # This CSS hack first hides the text within the span tags of the specified classes
                                   # Then it adds desired text and CSS properties. !important parameter is to override
                                   # inline css parameters.
                                   tags$style(HTML(
                                     ".irs-min {visibility:hidden !important;}
                                    .irs-max {visibility:hidden !important;}
                                    .js-irs-12 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-12 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-13 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-13 .irs .irs-max:after {content:'Highest' !important;}
                                    .js-irs-14 .irs .irs-min:after {content:'Lowest' !important;}
                                    .js-irs-14 .irs .irs-max:after {content:'Highest' !important;}
                                    .irs-min:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        left: 0;
                                        position: absolute;}

                                    .irs-max:after {
                                        visibility: visible !important;
                                        display: block;
                                        background: rgba(0, 0, 0, 0.1) none repeat scroll 0 0;
                                        border-radius: 3px;
                                        color: #333;
                                        font-size: 10px;
                                        line-height: 1.333;
                                        padding: 1px 3px;
                                        text-shadow: none;
                                        top: 0;
                                        cursor: default;
                                        display: block;
                                        right: 0;
                                        position: absolute;}

                                ")),
                                   
                                   
                                   
                                   tags$style(HTML("
                                     a.action-button {color: #000000;font-weight: bold;}
                                     #checkbox1{padding-left: 25px;}
                                                     ")),
                                   # Set slides color
                                   setSliderColor(rep("#9760a8", 15), c(12:15)),
                                   #### Sliders and checkbox set up ----
                                   fluidRow(
                                     column(10,
                                            actionLink("checkbox_pollution", label = "Pollution",
                                                       style="display: block;padding-top: 15px;}"),
                                            sliderInput(inputId = "pol_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 33), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("pol_agg_pref")
                                     )
                                   ),
                                   
                                   fluidRow(
                                     column(10,
                                            actionLink("checkbox_demographics", label = "Demographics"),
                                            sliderInput(inputId = "demo_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 33), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("demo_agg_pref")
                                     )
                                   ),
                                   fluidRow(
                                     column(10,
                                            actionLink("checkbox_access", label = "Isolation from Nature"),
                                            sliderInput(inputId = "access_w",
                                                        label = NULL,
                                                        ticks = FALSE,
                                                        min = 0,
                                                        max = 100,
                                                        value = 33), # End sliderInput
                                     ),
                                     column(1,offset = 0, style='padding: 15px 10px 0px 0px;',
                                            br(),
                                            textOutput("access_agg_pref")
                                     )
                                   )
                     ),
                     
                     ### Remove shapes button ----
                     absolutePanel(id = "removeShapes_equity",
                                   class = "panel panel-default",
                                   fixed = FALSE,
                                   draggable = FALSE,
                                   top = 125, left = "auto",
                                   right = 25, bottom = "auto",
                                   actionButton("removeShapes_equity","", icon = icon("fa-regular fa-trash"))
                     ),
                     
                     ### Text Information ----
                     column(5,offset = 0, style='padding-left:30px;padding-right:30px;padding-top:40px',
                            br(),
                            tags$div(
                              style = "display: flex; align-items: center; margin-top: 5px; margin-bottom: 5px;",
                              tags$h4(
                                style = "text-align: left; margin-right: 10px; margin-bottom: 0;",
                                tags$mark("Equity issues map",style = "color:#ffffff;
                            background-color: #9760a8;
                            border-radius: 0px;
                            padding: 3px 10px;
                            font-weight: bold;
                            position:relative;
                            filter:url(#marker-shape);
                            width:100%;
                            height:1em;
                            left:-0.25em;
                            top:0.1em;
                            padding:0 0.25em;
                            font-family: 'Chronicle Text G2 A', 'Chronicle Text G2 B', 
                            'Chronicle Text G2', Georgia, sans-serif")
                              ),
                              tags$h4("How to use it?", style = "font-weight: bold ;margin-bottom: 0; margin-top: 1;")
                            ),
                            tags$p("This map illustrates the degree of overlap among different environmental justice, diversity, equity, and inclusion issues. 
                                   Darker areas represent a higher concentration of the selected factors, indicating that certain communities in those areas may be disproportionately affected and in need of mitigating actions. 
                                   You can adjust the sliders to modify the relative weight of each item that you wish to display.", 
                                   style="text-align: justify; margin-top: 15px; margin-bottom: 5px;")
                     ),
                     ### Data tab ----
                     column(7,
                            tabsetPanel(id="tabs_equity",
                                        tags$style(HTML(".tabbable > .nav > li > a {margin-top:5px;float:right;display:inline-block;}")),
                                        tags$style(HTML(
                                          ".tabbable ul li:nth-child(4) { float: right;}
                          .tabbable ul li:nth-child(3) { float: right;}
                          .tabbable ul li:nth-child(5) { float: right;}"
                                        )),
                                        
                                        #### About tab ----
                                        tabPanel(value = "about_equity",title="About",
                                                 tags$style("code {
                                                 color:#303e52;
                                                 background-color: #c1d7f5;
                                                 border-radius: 3px;
                                                padding: 0 3px;}"),
                                                 h3("Area statistics:"),
                                                 HTML("<h4 style=text-align: left>To extract statistics from your area of interest, draw a shape on the map.</h4>"),
                                                 HTML('<p align= "justify">The <code>Summary</code> tab displays the average score of each available resource, along with the combined score calculated by applying weights. 
                                                 The <code>Plot</code> tab shows the actual scores of each resource graphically and offers insights into the data distribution within the selected area. 
                                                      When no area is selected, the values for the entire region of interest are displayed.</p>')
                                        ),
                                        #### Summary tab ----
                                        tabPanel(value = "summary_equity", title ="Summary",
                                                 textOutput("data_displayed_note_summary_equity"),
                                                 tags$head(tags$style("#data_displayed_note_summary_equity{margin-top: -1.6em;
                                           margin-left: 1em;
                                          margin-bottom: 0.1em;
                                           color: #808080;
                                           font-size:12px;
                                           font-style: italic;}"
                                                 )),
                                                 fluidRow(column(7,
                                                                 gt_output("mytable_equity")
                                                 ),
                                                 column(5,
                                                        h4("Aggregated Score:",
                                                           style="color: #808080;
                                                    padding-top: 5px;"),
                                                        br(),
                                                        gaugeOutput("gauge_equity")),
                                                 )),
                                        #### Plot tab ----
                                        tabPanel(value = "plot_equity", title ="Plot",
                                                 textOutput("data_displayed_note_plot_equity"),
                                                 tags$head(tags$style("#data_displayed_note_plot_equity{
                                                 margin-top: -1.6em;
                                                 margin-left: 1em;
                                                 margin-bottom: 0.1em;
                                                 color: #808080;
                                                 font-size:12px;
                                                 font-style: italic;}"
                                                 )),
                                                 fluidRow(column(6,
                                                                 plotOutput("boxplot_equity",inline=T, height = 210)
                                                 ),
                                                 column(6,
                                                        br(),
                                                        plotlyOutput("radar_graph_equity", inline=T, height = 190),
                                                        align="right")
                                                 ))
                            )),
              )
            )
    ),

    ### DATA INFORMATION ----
    tabItem(tabName = "data_information_equity",
            fluidPage(
              htmltools::tags$iframe(src = "data_information_equity.html", width = '100%',  height = 1000,  style = "border:none;"))
    ),
    
    
    ## OTHER INFORMATION ----
    tabItem(tabName = "other_information",
            fluidPage(
              htmltools::tags$iframe(src = "data_information_other.html", width = '100%',  height = 1000,  style = "border:none;"))
    )

  
    )
  )
     

ui <- dashboardPage(header, siderbar, body , skin = "black",
                    tags$head(tags$style(HTML('* {font-family: "Whitney A", "Whitney B", Whitney, "Trebuchet MS", sans-serif;')))
                    )



