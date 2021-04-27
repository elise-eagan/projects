


# Calculator --------------------------------------------

wh_panel <- tabPanel(
  "WH Connected Calculator",
  
  sidebarPanel(
    helpText("Select your tool, experiment groups, and KPI"),
    
    radioButtons("tool_used", "Implementation:"
                 , choices = list("Pendo", "Optimizely")),
    
    numericInput("variants", "Variants:", 2, min = 2, max = 5, width = '80px'),
    
    # add dynamic UI for > 2 groups: https://shiny.rstudio.com/articles/dynamic-ui.html
    uiOutput("ctrlGroupControls"),
    uiOutput("testGroupControls"),
    
    selectizeInput("kpi", "Primary KPI:"
                   , choices = list("Nominate"="nominate", "Congratulate"="congrats", "View Cart"="cart", "Redeem"="redeem"

                   )
                   , options = list(placeholder = 'Select a success metric', onInitialize = I('function() { this.setValue("a"); }'))),
    
    radioButtons("timeframe", "KPI Conversion Timeframe:", choices = list("Same Sesssion"="ss", "Same Day"="d1", "Within 7 Days"="d7", "Within 14 Days"="d14")),
    sliderInput("confidence_level", "Confidence Level:",min = 90,max = 99,value = 90, post = "%"),
    
    br(),
    
    tags$h4("Additional Options:"),

    checkboxInput("bonferroni", "Apply Bonferroni Correction", TRUE, width = NULL), # what about Dunnet's Correction?
    #helpText("(The Bonferroni Correction reduces the change of a false positive when you have more variants.)"),
    #br(),
    sliderInput("mde", "Minimum Detectable Effect:",min = 10,max = 60,value = 30, post = "%"),
    
    br()
    
    
  ), # end sidebar panel
  
  
  mainPanel(
    

    conditionalPanel(
      condition = "!is.null(input.kpi)",
      uiOutput("txtout_result")
      
    ),
    
    tableOutput("describe_data"),
    br(),
    tableOutput("results_data"),
    
    conditionalPanel(
      condition = "!is.null(input.kpi)",
      plotOutput("dist_plot", height = "450")
      
    ),
    
    
    br()
    

    
  )
  
  
)






# User Interface ----------------------------------------------

ui <- shinyUI(
  navbarPage(
    title = "Statistical Significance Calculator",
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
    wh_panel

  )
)