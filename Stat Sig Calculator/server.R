library(dplyr)

## for loading from s3
#S3_LINK <- "s3://wh-redshift-poc/wari_poc/elise.eagan/experiment_history/"
#data_file <- "experiment_history.csv000"

# check to make sure the file name is linked here correctly. 
#data.exists <- aws.s3::object_exists(paste0(S3_LINK, data_file))

# load data if exists
#if(data.exists == TRUE){
#  data <- aws.s3::s3read_using(read.csv, object = paste0(S3_LINK, data_file), header=TRUE)
#} 

data <- read.csv("data/experiment_history.csv")


# Define server function  
server <- function(input, output) {
  
  
  # WH Connected Calculations
  
  Pendo <- data %>% dplyr::filter(implementation == "Pendo")
  Optly <- data %>% dplyr::filter(implementation == "Optimizely")
  Marketo <- data %>% dplyr::filter(implementation == "Marketo")
  
  datasetTool <- reactive({
    switch(input$tool_used,
           "Pendo" = Pendo,
           "Optimizely" = Optly,
           "Marketo" = Marketo
    )
  })
  
  #change UI control group options based on impelentation tool selected
  output$ctrlGroupControls <- renderUI({
    groups <- datasetTool()
    selectizeInput("ctrl_group", "Control Group:"
                   , choices = datasetTool() %>% arrange(test_group) %>% pull(test_group)
                   , options = list(placeholder = 'Select a control group'))
  })
  
  #change UI test group options based on impelentation tool selected
  output$testGroupControls <- renderUI({
    groups <- datasetTool()
    selectizeInput("test_group", "Test Group:"
                   , choices = datasetTool() %>% arrange(test_group) %>% pull(test_group)
                   , options = list(placeholder = 'Select a test group'))
  })
  
  
  
  
  # control group data
  controldf = reactive({
    ctrl_df <- data %>% dplyr::filter(test_group == input$ctrl_group)
    return(list(ctrl_df = ctrl_df))
  }) 
  
  # test group data
  testdf = reactive({
    test_df <- data %>% dplyr::filter(test_group == input$test_group)
    return(list(test_df = test_df))
  }) 
  
  
  # describe data
  describe = reactive({
    time_window <- input$timeframe
    kpi <- input$kpi
    column_name <- paste(time_window, kpi, sep="_")
    
    ctrl_conv <- round(controldf()$ctrl_df %>% pull(column_name), digits=0)
    ctrl_sample <- round(controldf()$ctrl_df %>% pull("exposed_users"), digits=0)
    ctrl_conv_rate <- ctrl_conv/ctrl_sample
    ctrl_variance <- ((1-ctrl_conv_rate)*ctrl_conv_rate)
    ctrl_std_err <- sqrt(ctrl_variance/ctrl_sample)
    ctrl_std_dev <- sqrt(ctrl_variance)
    
    test_conv <- round(testdf()$test_df %>% pull(column_name), digits=0)
    test_sample <- round(testdf()$test_df %>% pull("exposed_users"), digits=0)
    test_conv_rate <- test_conv/test_sample
    test_variance <- ((1-test_conv_rate)*test_conv_rate)
    test_std_err <- sqrt(test_variance/test_sample)
    test_std_dev <- sqrt(test_variance)
    
    Control <- c(round(ctrl_sample,digits=0), round(ctrl_conv, digits=0), round(ctrl_conv_rate*100, digits=2), ctrl_std_dev)
    Test <- c(round(test_sample, digits=0), round(test_conv,digits=0), round(test_conv_rate*100, digits=2), test_std_dev)
    
    df <-data.frame(Control, Test)
    row.names(df) <- c("Sample Size","Conversion Size", "Conversion Rate (%)", "Standard Deviation")
    
    return(list(df=df))
    
  })
  
  # significance calculations
  significance = reactive({
    
    
    ## add confidence interval and margin of error
    ### we are 95% confident conv rate is X% +/- Y% moe
    ### if CIs overlap, keep testing.
    
    time_window <- input$timeframe
    kpi <- input$kpi
    column_name <- paste(time_window, kpi, sep="_")
    
    # there has to be a better way than recalculating all of this
    ctrl_conv <- controldf()$ctrl_df %>% pull(column_name)
    ctrl_sample <- controldf()$ctrl_df %>% pull("exposed_users")
    ctrl_conv_rate <- ctrl_conv/ctrl_sample
    ctrl_variance <- ((1-ctrl_conv_rate)*ctrl_conv_rate)
    ctrl_std_err <- sqrt(ctrl_variance/ctrl_sample)
    
    test_conv <- testdf()$test_df %>% pull(column_name)
    test_sample <- testdf()$test_df %>% pull("exposed_users")
    test_conv_rate <- test_conv/test_sample
    test_variance <- ((1-test_conv_rate)*test_conv_rate)
    test_std_err <- sqrt(test_variance/test_sample)
    
    conv_diff <- test_conv_rate - ctrl_conv_rate
    lift <- conv_diff / ctrl_conv_rate
    z_score <- conv_diff / sqrt(ctrl_std_err**2 + test_std_err**2)
    p_value <-pnorm(q = -z_score, mean = 0, sd = 1, lower.tail = TRUE)*2
    
    Results <- c(round(lift*100,digits=1), z_score, p_value)
    results_df <- data.frame(Results)
    row.names(results_df) <- c("Lift (%)", "Z Score", "P Value")
    
    return(list(results_df=results_df))
    
  })
  
  
  
  
  ### main panel output
  
  output$describe_data <- renderTable({
    validate(need(input$kpi != "", ""))
    
    if(is.null(describe()$df)) return()
    describe()$df
  }, 'include.colnames' = TRUE, 'include.rownames' = TRUE)
  
  output$results_data <- renderTable({
    validate(need(input$kpi != "", ""))
    if(is.null(significance()$results_df)) return()
    significance()$results_df
  }, 'include.colnames' = TRUE, 'include.rownames' = TRUE)
  
  
  output$txtout_result <- renderText({
    validate(need(input$kpi != "", ""))
    
    lift <- round(unlist(significance()$results_df)[1],digits=0)
    p <- unlist(significance()$results_df)[3]
    
    if (input$bonferroni == TRUE) {siglevel <- (1-input$confidence_level/100)/input$variants}
    else if (input$bonferroni == FALSE) {siglevel <- (1-input$confidence_level/100)}   
    
    
    # is there enough sample?
    daily_traffic <- (controldf()$ctrl_df %>% pull("exposed_users") + testdf()$test_df %>% pull("exposed_users")) / testdf()$test_df %>% pull("duration")
    baseline_conv <- unlist(describe()$df)[3]/100
    mde <- if(input$mde < 30) {input$mde/100} else {.3}
    min_test_conv <- baseline_conv + baseline_conv*mde
    
    if (baseline_conv == 0){req_sample <- 0}
    else {req_sample <- (baseline_conv*(1-baseline_conv) + min_test_conv*(1-min_test_conv)) / ((baseline_conv-min_test_conv)**2)*(1.96+.84)**2}
    
    
    if (req_sample == 0){meets_sample <- FALSE}
    else if (controldf()$ctrl_df %>% pull("exposed_users") > req_sample && testdf()$test_df %>% pull("exposed_users") > req_sample){
      meets_sample <- TRUE} 
    else {meets_sample <- FALSE}
    
    
    
    if(is.null(p)) {return()}
    else if (isTRUE(meets_sample) == FALSE){
      Out <- HTML(paste(
        tags$h3("These results are ", tags$strong("INCONCLUSIVE."), sep=""),
        br(),
        tags$h4(tags$strong("What does this mean? ")),
        tags$h4("More sample is needed to determine significance. This test needs at least ", round(req_sample, digits=0), "users per group."),
        br(),
        tags$h5(strong("Control Group: "),input$ctrl_group),
        tags$h5(strong("Test Group: "),input$test_group),
        tags$h5(strong("Days Running: "),testdf()$test_df %>% pull("duration")),
        tags$h5(strong("Data available up to: "),testdf()$test_df %>% pull("last_day")),
        tags$h5(strong("Primary Success Metric: "),input$kpi)
      ))
    } else if (p <= siglevel){
      Out <- HTML(paste(
        tags$h3("These results are ", tags$strong(tags$span(style="color:green","statistically significant")), "!",sep=""),
        br(),
        tags$h4(tags$strong("What does this mean? ")),
        tags$h4("We are ", input$confidence_level," % confident that there IS a difference in conversion between the test and control groups, with a "
                ,tags$strong(lift,"% lift"), "in conversion rate. This increase is not guaranteed to hold when exposing additional users to the changes, but we can expect to see the same directionality."),
        br(),
        tags$h5(strong("Control Group: "),input$ctrl_group),
        tags$h5(strong("Test Group: "),input$test_group),
        tags$h5(strong("Days Running: "),testdf()$test_df %>% pull("duration")),
        tags$h5(strong("Data available up to: "),testdf()$test_df %>% pull("last_day")),
        tags$h5(strong("Primary Success Metric: "),input$kpi)
        
      ))
    } else if (p > siglevel){
      Out <- HTML(paste(
        tags$h3("These results are ", tags$strong(tags$span(style="color:red","NOT statistically significant")), ".", sep=""),
        br(),
        tags$h4(tags$strong("What does this mean? ")),
        tags$h4("This experiment is ",tags$strong("invalidated."),"We are unable to determine that there is
                a difference in conversion between the test and control groups. If there is a difference, it is too small to be detected."),
        br(),
        tags$h5(strong("Control Group: "),input$ctrl_group),
        tags$h5(strong("Test Group: "),input$test_group),
        tags$h5(strong("Days Running: "),testdf()$test_df %>% pull("duration")),
        tags$h5(strong("Data available up to: "),testdf()$test_df %>% pull("last_day")),
        tags$h5(strong("Primary Success Metric: "),input$kpi)
      ))
    }
    Out
  })
  
  
  
  
  output$dist_plot <- renderPlot({
    validate(need(input$kpi != "", ""))
    
    cm <- unlist(describe()$df)[3]
    csd <- unlist(describe()$df)[4]*100
    tm <- unlist(describe()$df)[7]
    tsd <- unlist(describe()$df)[8]*100
    
    ctrl <- function(x) dnorm(x,cm,csd)
    test <- function(y) dnorm(y,tm,tsd)
    
    plot(ctrl,-150,150, type = "h", col = "darkgoldenrod", main = "Conversion Rate (%) Distribution"
         ,xlab = NULL, ylab = NULL, yaxt="none")
    plot(test,-150,150, type = "o", col = "gray50", add=TRUE)
    axis(1, seq(-150,150,10))
    legend("topleft", inset=.02, title="Variant",
           legend=c("Control", "Test"), col=c("darkgoldenrod","gray50"),
           lty=1:2, cex=1.2)
    
    
    
  })  
  
  
  
}






