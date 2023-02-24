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
  
  removeUI( selector = '#main_wait_message' )
  
  # UI General
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Alright. Let's go"))
  )
  
  # Data Outputs
  
  output$Nvessels_SVP <- renderValueBox({
    
    Nvessels_SVP_Y <- dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("SVP",SampleType,ignore.case = T)) %>%
      summarise(Nvessels=n_distinct(VesselID)) %>%
      ungroup() %>%
      select(Nvessels) %>%
      as.numeric()
    
    valueBox(value=Nvessels_SVP_Y,
             subtitle = tags$p("Number of boats in SVP programme",
                               style = "font-size: 100%;"),
             icon = icon('ship'), 
             color = "green"
    )
  })
  
  output$NDaysSVP <- renderValueBox({
    
    NDaysSVP<- dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("SVP",SampleType,ignore.case = T)) %>%
      group_by(VesselID) %>%
      summarise(Days=n_distinct(EventStartDate)) %>%
      ungroup() %>%
      summarise(Tot=sum(Days)) %>% 
      select(Tot) %>%
      as.numeric()
    
    valueBox(value=NDaysSVP, 
             subtitle = tags$p("Number of day trips sampled in the SVP",
                               style = "font-size: 100%;"),
             icon = icon('hourglass'), 
             color = "light-blue"
    )
  })
  
  output$plotH1<-renderPlot({
    dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("SVP",SampleType,ignore.case = T)) %>%
      
      group_by(COUNTY) %>%
      summarise(Nvessels=n_distinct(VesselID)) %>%
      ungroup () %>%
      mutate(csum = rev(cumsum(rev(Nvessels))), 
             pos = Nvessels/2 + lead(csum, 1),
             pos = if_else(is.na(pos), Nvessels/2, pos)) %>%
      arrange(-Nvessels) %>%
      ggplot(data=.,
             aes(x="",y=Nvessels,
                 fill=COUNTY))+
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_label_repel(aes(y = pos, label = Nvessels),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
      labs(y="SVP boats by County") +
      scale_fill_npg()+
      theme_void()+
      theme(
        legend.title=element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_text(size=rel(1.4), face="bold",
                                    margin = margin(t = 25, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
  },bg="transparent")
  
  output$plotH2<-renderPlot({
    dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("obs",SampleType,ignore.case = T)) %>%
      
      group_by(COUNTY) %>%
      summarise(NTrips=n_distinct(FileName)) %>%
      ungroup () %>%
      mutate(csum = rev(cumsum(rev(NTrips))), 
             pos = NTrips/2 + lead(csum, 1),
             pos = if_else(is.na(pos), NTrips/2, pos)) %>%
      arrange(-NTrips) %>%
      ggplot(data=.,
             aes(x="",y=NTrips,
                 fill=COUNTY))+
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_label_repel(aes(y = pos, label = NTrips),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      labs(y="Observer trips by County") +
      scale_fill_npg()+
      theme_void()+
      theme(
        legend.title=element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_text(size=rel(1.4), face="bold",
                                    margin = margin(t = 25, r = 0, b = 0, l = 0)),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
  },bg="transparent")
  
  
  output$NTrips_Obs_Y <- renderValueBox({
    
    NTrips_Obs_Y <- dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("obs",SampleType,ignore.case = T)) %>%
      summarise(NTrips=n_distinct(FileName)) %>%
      ungroup() %>%
      select(NTrips) %>%
      as.numeric()
    
    valueBox(value=NTrips_Obs_Y,
             subtitle = tags$p("Number of observer trips",
                               style = "font-size: 100%;"),
             icon = icon('eye'), 
             color = "green"
    )
  })
  
  output$NHauls_Obs_Y <- renderValueBox({
    
    NHauls_Obs_Y <- dat_sta %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      filter(grepl("obs",SampleType,ignore.case = T)) %>%
      group_by(FileName) %>%
      summarise(NHauls=n_distinct(EventID)) %>%
      ungroup() %>%
      summarise(Tot=sum(NHauls)) %>% 
      select(Tot) %>%
      as.numeric()
    
    valueBox(value=NHauls_Obs_Y, 
             subtitle = tags$p("Number of hauls sampled by observers",
                               style = "font-size: 100%;"),
             icon = icon('hourglass'), 
             color = "light-blue"
    )
  })
  
  output$CREMeasured <- renderValueBox({
    
    CREMeas <- bio %>%
      filter(CommonName %in% "EDIBLE CRAB UNSEXED") %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      group_by(CommonName) %>%
      summarise(Nind=n()) 
      as.numeric()
    
    valueBox(value=CREMeas$Nind, 
             icon = icon(list(src="species/Edible crab.png", width="150px"), 
                         lib="local"),
             subtitle = tags$p("Number of crabs measured by observers",
                               style = "font-size: 100%;"),
             color = "green"
    )
  })
  
  output$LBEMeasured <- renderValueBox({
    
    LBEMeas <- bio %>%
      filter(CommonName %in% "EUROPEAN LOBSTER") %>%
      filter(.$Year >= input$Year[1]) %>%
      filter(.$Year <= input$Year[2]) %>%
      group_by(CommonName) %>%
      summarise(Nind=n()) 
    as.numeric()
    
    valueBox(value=LBEMeas$Nind, 
             subtitle = tags$p("Number of lobsters measured by observers",
                               style = "font-size: 100%;"),
             icon = icon(list(src="species/European lobster.png", width="150px"), 
                         lib="local"), 
             color = "green"
    )
  })
  
  output$landingsPlot<-renderPlot({
    
    tmp1<-landings%>%
      filter(.$SpeciesName %in% input$SpIDL) %>%
      filter(.$Year >= input$YearL[1]) %>%
      filter(.$Year <= input$YearL[2])
    
    ggplot(data=tmp1,
           aes(x=Year,y=Landings,group=1))+
      geom_bar(stat="identity",colour="black",fill="grey")+
      facet_wrap(.~SpeciesName,scales = "free_y",ncol = 1)+
      #scale_colour_lancet()+
      #scale_fill_lancet()+
      #scale_y_continuous(limits = c(0,max(tmp$Mean_rate))) +
      #facet_wrap(.~SampleType,ncol = 2)+
      labs(x="Year",y="Tonnes")+
      theme_bw()+
      theme(
        axis.title.x = element_text(size=rel(1.4), face="bold",
                                    margin = margin(t = 25, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=rel(1.4), face="bold",
                                    margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.title = element_text(size = rel(1.4)),
        strip.text = element_text(size=rel(1.4)),
        panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key=element_blank())
    
  },bg="transparent")
  
  output$plot1<-renderPlot({
    
    tmp2<-catch_plot%>%
      filter(.$CommonName %in% input$SpID) %>%
      filter(.$SampleType %in% c(input$SP)) %>%
      filter(.$Year >= input$YearC[1]) %>%
      filter(.$Year <= input$YearC[2]) %>%
      filter(.$Catch_type %in% c(input$CatchID)) #%>%
      #mutate(maxC=max(Mean_rate,na.rm = T),
      #       minC=0) %>%
     
     ggplot(data=tmp2,
             aes(x=Year,y=Mean_rate,shape=Catch_type,
                        linetype = Catch_type,
                 colour=Catch_type,
                 fill=Catch_type))+
      geom_point(size=3)+
      geom_line(size=.5)+
      scale_colour_lancet()+
      scale_fill_lancet()+
      scale_y_continuous(limits = c(0,max(tmp2$Mean_rate))) +
      facet_wrap(.~SampleType,ncol = 2)+
      labs(x="Year",y=tmp2$Units,shape="Catch Type",linetype="Catch Type",
           fill="Catch Type",colour="Catch Type")+
      theme_bw()+
      theme(
            axis.title.x = element_text(size=rel(1.4), face="bold",
                                        margin = margin(t = 25, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size=rel(1.4), face="bold",
                                        margin = margin(t = 0, r = 25, b = 0, l = 0)),
            legend.title = element_text(size = rel(1.4)),
            strip.text = element_text(size=rel(1.4)),
            panel.background = element_rect(fill = "aliceblue"),
            plot.background = element_blank(),
            legend.background = element_blank(),
            legend.key=element_blank())
        
  },bg="transparent")
  
  output$plot2<-renderPlot({
    bio%>%
      filter(.$CommonName %in% input$SpID2) %>%
      filter(.$Year >= input$Year2[1]) %>%
      filter(.$Year <= input$Year2[2]) %>%
      filter(.$Size >= input$Size[1]) %>%
      filter(.$Size <= input$Size[2]) %>%
      filter(.$SexID %in% c(input$SexID)) %>%
      filter(.$DiscardedYN %in% c(input$DiscardedYN)) %>%
      filter(.$VNotchedYN %in% c(input$VNotchedYN)) %>%
    ggplot(.,
           aes(x=Size,fill=DiscardedYN))+
      geom_histogram(binwidth = 1,position = "identity",alpha=.7,colour="black")+
      geom_density(aes(y=1 * ..count..),alpha=.8,trim=T)+
      geom_vline(aes(xintercept=MLS),colour="green",linetype="dashed",size=1.5)+
      geom_vline(aes(xintercept=MaxLS),colour="green",linetype="dashed",size=1.5)+
      #geom_freqpoly(binwidth = 1,size=1.5)+
      facet_wrap(.~Year,ncol=1)+
      scale_colour_lancet()+
      scale_fill_lancet()+
      expand_limits(y=c(0,0))+
      ylab("Number of Individuals")+xlab("Size (mm)")+
      theme_bw()+
      theme(axis.title.x = element_text(size=17.5, face="bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size=17.5, face="bold",margin = margin(t = 0, r = 10, b = 0, l = 0)),
            axis.text.x = element_text(face="bold",size=18),
            axis.text.y = element_text(face="bold",size=18),
            legend.title = element_blank(),
            legend.text = element_text(size=14.5, face="bold"),
            legend.position="bottom",
            strip.text = element_text(size=12,face="bold"),
            panel.grid.minor = element_blank(),
            #panel.grid.major.y = element_line(colour = "black",linetype="dashed"),
            panel.border = element_rect(colour = "black", fill=NA, size=2),
            axis.line = element_line(colour = "black"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank())
  },bg="transparent")
  
  
  #Leaflet section
 # pal<-reactiveValues()
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = ICES_LPUE$slp_std,
    na.color = "transparent")
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -6.2, lat = 53.3, zoom =5.5 ) %>%
      addProviderTiles(providers$Esri.WorldTerrain) %>% #Esri.OceanBasemap
      addLegend(pal = pal, 
                values = ICES_LPUE$slp_std, 
                opacity = 1, 
                title = "LPUE trend")
    })

  
  observeEvent(input$SpID5, {
    
    if(input$SpID5 != "")
    {
      
      tmp <- ICES_LPUE[ICES_LPUE$CommnNm == input$SpID5, ]
      
      NamePop<-paste0("Rectangle name:",tmp$ICESNAME)
      
      leafletProxy("map") %>% 
        clearShapes() %>%
        addPolygons(data = tmp,
                    group = "LPUE", 
                    weight = 1,                              
                    opacity = 1,
                    color = "black", 
                    fillColor = ~pal(slp_std),
                    fillOpacity = .8,
                    layerId = ~ICESNAME,
                    popup = NamePop)
    }
  }) 
  
  # A reactive expression that returns the set of recs that are
  # in bounds right now
  RecInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(ICES_LPUE[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
  
    subset(ICES_dat, CommonName %in% input$SpID5 &
           NORTH  >= latRng[1] & SOUTH  <= latRng[2] &
             EAST   >= lngRng[1] & WEST   <= lngRng[2])
  })
  
  output$scatter_plot <- renderPlotly({
    # If no rectangles are in view, don't plot
    if (nrow(RecInBounds()) == 0)
      return(NULL)
    
    p<-ggplot(RecInBounds(),aes(x=Year,y=LPUE))+
      geom_jitter(size = 0.1)+
      geom_smooth(method = "lm")+
      scale_x_continuous(breaks = seq(minY,maxY,1))+
      labs(y=paste0("LPUE","(",unique(RecInBounds()$units),")"))+
      theme_bw()+
      theme(
        axis.title.x = element_text(size=rel(1), face="bold",
                                    margin = margin(t = 25, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size=rel(1), face="bold",
                                    margin = margin(t = 0, r = 25, b = 0, l = 0)),
        legend.title = element_text(size = rel(1)),
        strip.text = element_text(size=rel(1)),
        panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key=element_blank())
    
    fig<-ggplotly(p)
    fig
  })
  
 #Assessment Outputs
 
  #Reactive selectInput
  observe({
    
    tmp<-a_a$Specie[a_a$Year == input$SpY]
    
    updateSelectInput(session, 
                      "SpIDA",
                      "Species:",
                      choices = c("SELECT SPECIES",unique(tmp)),
                      selected = "SELECT SPECIES")
  })
  
  observe({
    tmpY<-a_a[a_a$Year == input$SpY,]
    tmpS<-tmpY$Area[tmpY$Specie == input$SpIDA]
    updateSelectInput(session, 
                      "SpArea",
                      "Area:",
                      choices = c("SELECT AREA",unique(tmpS)),
                      selected = "SELECT AREA")
  })
  
  # Text output
  output$stock_text <- renderText({
    paste0(a_a[a_a$Year == input$SpY &
                 a_a$Specie == input$SpIDA &
                 a_a$Area == input$SpArea,"fishery"])
    })
  
  output$advice_text<- renderText({
    paste0(a_a[a_a$Year == input$SpY &
                 a_a$Specie == input$SpIDA &
                 a_a$Area == input$SpArea,"ManagementAdvice"])
  })
  
  
  output$survey_text <- renderText({
    paste0(a_a[a_a$Year == input$SpY &
                 a_a$Specie == input$SpIDA &
                 a_a$Area == input$SpArea,"Assessment"])
  })
  
  output$Aoutput_text <- renderText({
    paste0(a_a[a_a$Year == input$SpY &
                 a_a$Specie == input$SpIDA &
                 a_a$Area == input$SpArea,"Outputs"])
  })
  
  
  #Directory to look for images:
  #stock<-reactive({
  #  tmp<-a_a[a_a$Specie == input$SpIDA,]
  #  tmp2<-a_a[a_a$Area == input$SpArea,]
  #  
  #  stock<-paste(tmp2$Specie,tmp2$Area,sep=" ")
  #  
  #  return(stock)
  #  
  #})
  
  #stock <- reactive ({
  #  if (!is.null(input$SpIDA) & !is.null(input$SpArea)) {
  #    tmp<-paste(unique(input$SpIDA), unique(input$SpArea), sep=" ")
  #    return(tmp)
  #  }
  #  else {
  #    return()
  #  }
  #  })

  #output$species <- renderImage({
  #  image_file <- paste0("www/species/",input$SpIDA, 
  #                       "/Survey_zones.png")
  #  return(list(src = image_file, filetype = "image/png",width = 300))
  #}, deleteFile = FALSE)
  
  
  output$survey.zones <- renderImage({
    
    image_path1<-file.path("www/Assessment and advice",
                          paste(input$SpIDA,input$SpArea,input$SpY,sep= "/"), 
                          "Survey_zones.png")
    return(list(src = image_path1, filetype = "image/png",width = 300))
  }, deleteFile = FALSE)
  
  output$display.assessment <- renderImage({
    image_path2<-file.path("www/Assessment and advice",
                          paste(input$SpIDA,input$SpArea,input$SpY,sep= "/"), 
                          "BiomassMap_from_AbundanceDensityLW.png")
    return(list(src = image_path2, filetype = "image/png",width = 300))
  }, deleteFile = FALSE)
  
  output$display.size <- renderImage({
    image_path3<-file.path("www/Assessment and advice",
                          paste(input$SpIDA,input$SpArea,input$SpY,sep= "/"), 
                          "Size_distribution_abDensity.png")
    return(list(src = image_path3, filetype = "image/png",width = 500))
  }, deleteFile = FALSE)
  
  
}

