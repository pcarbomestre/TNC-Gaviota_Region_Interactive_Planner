ui <- fluidPage(
  
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
    ")
  ),
  
  #This CSS hack first hides the text within the span tags of the specified classes
  #Then it adds desired text and CSS properties. !important parameter is to override
  #inline css parameters.
  tags$style(type = "text/css", "
    .irs-min {visibility:hidden !important;}
    .irs-max {visibility:hidden !important;}
    .js-irs-0 .irs .irs-max:after {content:'highest' !important;}
    .js-irs-0 .irs .irs-min:after {content:'lowest' !important;}
    .js-irs-1 .irs .irs-max:after {content:'highest' !important;}
    .js-irs-1 .irs .irs-min:after {content:'lowest' !important;}
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

"),
  
  sliderInput(inputId = "water_w",
              label = NULL,
              ticks = FALSE,
              min = 0,
              max = 1,
              value = 1),
  sliderInput(inputId = "water_22",
              label = NULL,
              ticks = FALSE,
              min = 0,
              max = 1,
              value = 1)
  
)

server <- function(input, output, session){
  
}

shinyApp(ui = ui, server=server)