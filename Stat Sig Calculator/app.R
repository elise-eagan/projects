# Load data and libraries ---------------
library(dplyr)
library(shiny)
library(shinythemes)
#library(aws.s3)
#library(aws.ec2metadata)
#library(aws.signature)



source("ui.R")
source("server.R")






# Create shiny application --------------
shinyApp(ui = ui, server = server)