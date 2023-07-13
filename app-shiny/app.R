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
library(pins)
library(arrow)


# Define UI for application
ui <- navbarPage(
  # Application title
  title = "AZMET QA",
  id = "navbar",
  tabPanel(
    title = "Daily",
    uiOutput("daily_range"),
    gt_output(outputId = "check_daily"),
    
  ),
  tabPanel(
    title = "Hourly",
    uiOutput("hourly_range"),
    gt_output(outputId = "check_hourly")
  ),
  tabPanel(
    title = "Forecast-based",
    uiOutput("fc_range"),
    gt_output(outputId = "check_forecast")
  )
)

# Define server logic
server <- function(input, output) {
  board <- board_connect()
  fc_daily <- board |> pin_read("ericrscott/fc_daily")
  
  output$daily_range <- renderUI({
    dateRangeInput(
      "dailyrange",
      "Date Range",
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      start = Sys.Date() - 14,
      end = Sys.Date()
    )
  })
  
  output$hourly_range <- renderUI({
    dateRangeInput(
      "hourlyrange",
      "Date Range",
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      start = Sys.Date() - 2, #only 2 days because hourly
      end = Sys.Date()
    )
  })
  
  output$fc_range <- renderUI({
    dateRangeInput(
      "fcrange",
      "Date Range",
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      start = Sys.Date() - 14,
      end = Sys.Date()
    )
  })
  
  #set up default action levels for pointblank
  al <- action_levels(warn_at = 1, stop_at = 0.1)
  
  # Only run daily validations when "Daily" tab is active
  observe({
    if (input$navbar == "Daily") {
      output$check_daily <-
        gt::render_gt({
          
          req(input$dailyrange)
          start <- input$dailyrange[1]
          end <- input$dailyrange[2]
          daily <- az_daily(start_date = start, end_date = end)
          
          check_daily(daily, al)
        })
    }
  })
  
  
  # Only run hourly validations when "Hourly" tab is active
  observe({
    if (input$navbar == "Hourly") {
      output$check_hourly <-
        gt::render_gt({
          
          req(input$hourlyrange)
          start <- input$hourlyrange[1] |> as.POSIXct() |> format_ISO8601() #to convert to datetime
          end <- input$hourlyrange[2] |> as.POSIXct() |> format_ISO8601()
          hourly <- az_hourly(start_date_time = start, end_date_time = end)
          
          check_hourly(hourly, al)
        })
    }
  })
  
  # Only run forecast-based validations when "Forecast-based" tab is active
  observe({
    if (input$navbar == "Forecast-based") {
      
      output$check_forecast <- 
        gt::render_gt({
          
          req(input$fcrange, fc_daily)
          start <- input$fcrange[1]
          end <- input$fcrange[2]
          
          check_forecast(fc_daily, start, end, al)
        })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
