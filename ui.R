## ---------------------------
### File: ui.R
##
## Date created: 2023-02-23
## Author: Pol Carbó Mestre
## Contact: pcarbomestre@bren.ucsb.edu
##
## ---------------------------
## Description:
##  Fluid page for the Interactive Planner app
## ---------------------------
##

#Global displaying options
options(spinner.color="#629871",
        spinner.color.background = "#629871",
        spinner.type=8)

# Header ------------------------------------------------------------------
header<- dashboardHeader(title = HTML("Interactive planner"),
                         disable = FALSE,
                         titleWidth = 350,
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
  #introBox(data.step = 1, data.intro = intro$text[1], data.position="right", #  intro tour
           div(class="inlay",style = "height:100%;width:100%;background-color: #ecf0f5;"),
  sidebarMenu(
    menuItem("Resources Axis", tabName = "Initial", icon = icon("fa-brands fa-pagelines"),
             menuSubItem("Natural Resources",tabName = "Map", icon = icon("map")),
             menuSubItem("Stakeholders priorities",tabName = "Map", icon = icon("fa-solid fa-layer-group")),
             menuSubItem("Data Information",tabName = "Details", icon = icon("info-circle"))),
    menuItem("Threats Axis",tabName = "landings", icon = icon("fa-regular fa-fire"),
             menuSubItem("Environmental Threats",tabName = "Map", icon = icon("map")),
             menuSubItem("Data Information",tabName = "Details", icon = icon("info-circle"))),
    menuItem("Equity Axis",tabName = "Assessment", icon = icon("fa-duotone fa-people-arrows"),
             menuSubItem("Equity Issues",tabName = "Map", icon = icon("map")),
             menuSubItem("Data Information",tabName = "Details", icon = icon("info-circle"))),
    menuItem("Other Information",tabName = "Oinfo", icon = icon("info-circle"))
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
    # Natural Resources ------------------------------------------------------------------
    tabItem(tabName = "Map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map", height='60vh')),
                     
                     absolutePanel(id = "transparency_control", 
                                   class = "panel panel-default", 
                                   fixed = TRUE,
                                   draggable = FALSE, 
                                   top = 80, left = "auto", 
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
                                   fixed = TRUE,
                                   draggable = FALSE, 
                                   top = 80, left = "auto", 
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
                     
                     absolutePanel(id = "controls", 
                                   class = "panel panel-default", 
                                   fixed = TRUE,
                                   draggable = TRUE, 
                                   top = 250, left = "auto", 
                                   right = 920, bottom = "auto",
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
                       #Using ionRangeSlider's javascript options you can hide/show selector labels and min/max labels
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
                       tags$style(
                                        "body {overflow-y: hidden;}"
                                      )
                     ),
                     
                     #This CSS hack first hides the text within the span tags of the specified classes
                     #Then it adds desired text and CSS properties. !important parameter is to override
                     #inline css parameters.
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
                                   
                                   setSliderColor(c("#629871", "#629871", "#629871", "#629871","#629871"), c(1,2,3,4,5)),
                     fluidRow(
                       column(10,
                                   actionLink("checkbox_water", label = "Water resources"),
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
                     
                     absolutePanel(id = "printShapes", 
                                   class = "panel panel-default", 
                                   fixed = TRUE,
                                   draggable = FALSE, 
                                   top = 140, left = "auto", 
                                   right = 70, bottom = "auto",
                                   width = "auto", height = "auto",
                                   style="background-color: white;
                                          opacity: 0.95;
                                          margin: auto;
                                          padding-bottom: 0mm;
                                          padding-top: 0mm;",
                                   actionButton("printShapes", h5(strong("Generate Stats")))
                     ),
                     
              column(4,
                     br(),
                     selectInput("stakeholder_w", label = "Select stakeholder's weights:", 
                                 choices = c("None",ahp_weights$group), 
                                 selected = NULL),
                     #uiOutput("statsBut"),
                     
                     h2("How to use this map?"),
                     tags$p("The Santa Barbara County Interactive Planner maps 
                     the degree of overlap of natural resources. Darker areas have more resources. 
                     Use the sliders to adjust the relative influence of each item.",
                            style="text-align: justify")
                  ),
             
              column(1),
               column(7,
                      tabsetPanel(
                        tags$style(HTML(".tabbable > .nav > li > a {margin-top:5px;float:right;display:inline-block;}")),
                        tags$style(HTML(
                          ".tabbable ul li:nth-child(4) { float: right;} 
                          .tabbable ul li:nth-child(3) { float: right;}
                          .tabbable ul li:nth-child(5) { float: right;}"
                        )),
                        tabPanel("About",
                                 h3("Area statistics:"),
                                 br(),
                                 h4("To extract statistics from your area of interest, simply create a polygon on the map and then click on the Generate Stats button.",
                                    style="text-align: center")
                                 ),
                        tabPanel("Summary",
                                 fluidRow(column(5,
                                                 gt_output("mytable")
                                                 ),
                                          column(6,
                                                 br(),
                                                 h4("Aggregated Score:"),
                                                 gaugeOutput("gauge"))
                                          )
                                 ),

                        tabPanel("Plot",
                                 fluidRow(column(7,
                                                 plotOutput("boxplot",inline=T, height = 210)),
                                          column(5,
                                                 br(),
                                                 plotlyOutput("radar_graph", inline=T, height = 190),
                                                 align="right"))
                                 )
                                 
                                 
                                 
                                 ,
                      
                        )
                ),

              # column(1,
              #        br(),
              #        br(),
              #        tableOutput("mytable")),
              # column(5,
              #        br(),
              #        br(),
              #        plotlyOutput("radar_graph", inline=T,height = 210))
              
              )
              )
            ),
    
    tabItem(tabName = "Details",
            br(),
            tags$p("This tab provides a brief description of the different data sources relevant to Shellfish Stocks 
                   around Ireland. Not all data sources are currently included in the app, as work is ongoing"),
            br(),
            tags$b("Shellfish Surveys"),
            HTML('<p align= "justify">Every year, the Marine Institute in collaboration with the fishing industry carry a number of scientific surveys 
            around Ireland, mainly focusing on <i>Cerastoderma edule</i> (Cockles), <i>Ensis siliqua</i> 
            (Razor clams) and <i>Ostrea edulis</i> (Native Oysters). The data collected during these surveys 
            is assessed using a geostatistical model which provides important information about the biomass 
            and size profile of the stocks. The outputs from this model, are directly reported to the industry. More information about 
                 the different surveys carried annually is found in the "Assessment and Advice" tab</p>'),
            br(),
            tags$b("Sentinel Vessel Programme"),
            HTML('<p align="justify">Since 2013 a total of approximately 80 Inshore fishing vessels 
            around Ireland have been providing self-sample information about their daily fishing 
            operations. The programme is administered by BIM as a pilot project funded 
            by the Data Collection Framework. Vessels are chosen from different length 
            and gear categories representative of fishing activities by vessels under 12 m 
            around the Irish coast. BIM send hard copies of SVP logbooks to the Marine Institute who extract and 
            manage the data from the SVP books, uploading it to the FEAS Inshore Database. 
            A small number of Skippers in recent years have submitted data via a phone app 
            directly to the Marine Institute (so called eSVP). The data recorded in the SVP logbooks includes the 
            catches, landings and discards of several species, i.e. <i>Homarus gammarus</i> 
            (Lobster), <i>Cancer pagurus</i> (Brown Crab), <i>Maja brachydactyla</i> (Spider Crab), 
            <i>Necora puber</i> (Velvet Crab), <i>Buccinum undatum</i> (Whelk), <i>Ensis sp</i>. (Razor clams), 
            <i>Cerastoderma edule</i> (Cockle) and various finfish species.  The fishing location 
            is recorded at either ICES Statistical Rectangle or Inshore Grid Resolution and 
            additional details such as the type and amount of bait used or vessel operating 
            costs (i.e. fuel consumption, number of crew, hours worked.). Additionally, although 
            to a lesser extent (every five fishing days), length frequency data for lobsters and 
            crabs may be included.</p>'),
            br(),
            tags$b("Observer programme"),
            HTML('<p align="justify">Each year since 2009, Marine Institute staff and contractors go to sea 
                   on inshore fishing vessels to observe and record fishing activity. 
                   About 50-80 day trips are completed annually, although this varies year 
                   on year and was lower earlier in the time series.  The data recorded in 
                   observer trips includes the catches, landings and discards of several species 
                   such as <i>Homarus gammarus</i> (Lobster), <i>Cancer pagurus</i> (Brown Crab), <i>Maja brachydactyla</i> 
                   (Spider Crab), <i>Necora puber</i> (Velvet Crab), <i>Buccinum undatum</i> (Whelk), and the bycatch 
                   associated with these fishing events. Furthermore, all individuals or a sample 
                   (depending on catch volume) of the target species captured  are measured to the nearest 
                   mm and their sex is determined, providing a significant amount of valuable biological 
                   information for these species. The observer programme provides data at the level of individual 
                   fishing operations in contrast to fishery dependent data collection programmes which report aggregated 
                   data. The sampling levels of 50-80 trips per year is low relative to the thousands of trips undertaken by the 
                   Inshore fishing fleet annually. Furthermore there is high variance between vessels (related to location of fishing). 
                   The low sampling level and high variance reduces precision and even accuracy in these data sets especially when reported 
                   to local level where the data supports are diluted.</p>'),
            br(),
            tags$b("Skipper Self-Sampling programme"),
            HTML('<p align="justify">Since 2021, a number of commercial inshore boats around the Irish coast report daily information on a haul by haul level on 
            catches, landings and discards,as well as biological information of several species. This programme is administered fully by the MI 
                   and the information provided enhances both the resolution (haul-by haul), and quantity (number of trips) of the SVP and observer programme respectively.
                   As the time-series of this programme is still short, data from this programme is not yet included in the Shellfish Fisheries app.</p>'),
            br(),
            tags$b("Port Sampling"),
            HTML('<p align= "justify">As part of the European Union (EU) Data Collection Framework (DCF) (or previous versions of it), since the early 90s, the Marine Institute has been requested to report catch sampling 
            information to aid in the assessment of fish stocks.This generally include both commercial at sea sampling and port sampling. In the case of Shellfish stocks, port samples 
                   provide valuable data related to the size distribution and biological information of landings and the area (at ICES Rectangle level) where fishing occured. Some of the species routinely sampled around Irish harbours include <i>Pecten maximus</i> (Scallops), 
                   <i>Buccinum undatum</i> (whelk) or <i>Cancer pagurus</i> (Brown crab) among others.</p>')
    ),
    
    tabItem(tabName = "Oinfo",
            br(),
            tags$p("This tab provides a series of usefull links to other sources of information relevant to Irish Fisheries"),
            br(),
            tags$b("Shellfish Stocks and Fisheries Review 2021: an assessment of selected stocks"),
            br(),
            tags$a(
              "https://oar.marine.ie/handle/10793/1744",
              target = "_blank",
              href = "https://oar.marine.ie/handle/10793/1744"),
            br(),
            br(),
            tags$b("Atlas: Commercial fisheries for shellfish around Ireland"),
            br(),
            tags$a(
              "https://oar.marine.ie/handle/10793/1243",
              target = "_blank",
              href = "https://oar.marine.ie/handle/10793/1243"),
            br(),
            br(),
            tags$b("Natura 2000 Network maps"),
            br(),
            tags$a(
              "https://natura2000.eea.europa.eu/",
              target = "_blank",
              href = "https://natura2000.eea.europa.eu/")
            )
    )
  )
     

ui <- dashboardPage(header, siderbar, body , skin = "black")


