#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(azmetr)
library(gt)
library(pointblank)
library(lubridate)
# library(pins)
# library(arrow)

# Define UI for application that draws a histogram
ui <- navbarPage(
  # Application title
  title = "AZMET QA",
  id = "navbar",
  tabPanel(
    title = "Daily",
    uiOutput("date_range"),
    gt_output(outputId = "check_daily"),
    
  ),
  tabPanel(
    title = "Hourly",
    # uiOutput("date_range"),
    gt_output(outputId = "check_hourly")
  ),
  tabPanel("Forecaset-based")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$date_range <- renderUI({
    dateRangeInput(
      "daterange",
      "Date Range",
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      start = Sys.Date() - 14,
      end = Sys.Date()
    )
  })
  
  #create connection to Posit Connect pinboard with data
  # board <- board_connect()
  # daily <- board |> pin_read("ericrscott/azmet_daily")
  
  #set up default action levels for pointblank
  al <- action_levels(warn_at = 1, stop_at = 0.1)
  
  observe({
    if (input$navbar == "Daily") {
      output$check_daily <-
        gt::render_gt({
          
          req(input$daterange)
          start <- input$daterange[1]
          end <- input$daterange[2]
          daily <- az_daily(start_date = start, end_date = end)
          
          
          check_daily(daily, start, end, al)
        })
    }
  })
  
  
  # #only run server code for Hourly tab if it is active
  observe({
    if (input$navbar == "Hourly") {
      output$check_hourly <-
        gt::render_gt({
          
          req(input$daterange)
          start <- input$daterange[1]
          end <- input$daterange[2]
          hourly <- az_hourly(start_date_time = as.POSIXct(start), end_date_time = as.POSIXct(end))
          
          check_hourly(hourly, start, end, al)
        })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
