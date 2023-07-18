library(shiny)
library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(pins)
library(arrow)

source("R/check_daily.R")
source("R/format_report_gt.R")

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

server <- function(input, output, session) {
  # board <- board_connect()
  # fc_daily <- board |> pin_read("ericrscott/fc_daily")
  
  # Date selector inputs -----
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
  
  # Daily tab ----
  observe({
    if (input$navbar == "Daily") {
      output$check_daily <- gt::render_gt({
        req(input$dailyrange) #wait until input exists
        
        start <- input$dailyrange[1]
        end <- input$dailyrange[2]
        #query API
        daily <- az_daily(start_date = start, end_date = end)
        
        #do validation report
        report_daily <- check_daily(daily)
        
        #convert to gt table
        format_report_gt(report_daily, daily)
      })
    }
  })
  
  # Hourly tab ----
  observe({
    if (input$navbar == "Hourly") {
      output$check_hourly <- gt::render_gt({
        req(input$hourlyrange) #wait until input exists
        
        start <- input$hourlyrange[1] |> as.POSIXct() |> format_ISO8601() #to convert to datetime
        end <- input$hourlyrange[2] |> as.POSIXct() |> format_ISO8601()
        #query API
        hourly <- az_hourly(start_date_time = start, end_date_time = end)
        
        #do validation report
        report_hourly <- check_hourly(hourly)
        
        #convert to gt table
        format_report_gt(report_hourly, hourly)
      })
    }
  })
  
  
}

shinyApp(ui, server)