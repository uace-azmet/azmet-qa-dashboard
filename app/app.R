library(shiny)
library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(pins)
library(arrow)
library(bslib)

source("R/helpers.R")
source("R/check_daily.R")
source("R/format_report_gt.R")


ui <- page_navbar(
  # Application title
  title = "AZMET QA",
  id = "navbar",
  #TODO use a different calendar widget taht fits better
  #TODO make sidebar conditional on nav tab
  sidebar = sidebar(
    conditionalPanel(
      "input.navbar === 'Daily'",
      uiOutput("daily_range")
    ),
    conditionalPanel(
      "input.navbar === 'Hourly'",
      uiOutput("hourly_range")
    ),
    conditionalPanel(
      "input.navbar === 'Forecast-based'",
      uiOutput("fc_range")
    )
  ), 
  nav_panel(
    title = "Daily",
    card(
      max_height = 250,
      full_screen = TRUE,
      card_header(
        "Daily Data Validation"
      ),
      gt_output(outputId = "check_daily")
    ),
    card(
      full_screen = TRUE,
      layout_sidebar(
        fillable = TRUE,
        fill = TRUE,
        sidebar = sidebar(
          shiny::selectInput(
            "station_daily",
            "Station",
            choices = azmetr::station_info$meta_station_name,
            multiple = FALSE
          ),
          shiny::selectInput(
            "plot_cols_daily",
            "Variables",
            choices = c("temperature", "precipitation", "wind & sun")
          )
        ),
        plotOutput(outputId = "plot_daily")
      )
    )
    
  ),
  nav_panel(
    title = "Hourly",
    card(
      max_height = 250,
      full_screen = TRUE,
      card_header(
        "Hourly Data Validation"
      ),
      gt_output(outputId = "check_hourly")
    )
  ),
  nav_panel(
    title = "Forecast-based",
    card(
      max_height = 250,
      full_screen = TRUE,
      card_header(
        "Forecast-Based Validation"
      ),
      gt_output(outputId = "check_forecast")
    )
  )
)

server <- function(input, output, session) {
  
  # Date selector inputs -----
  output$daily_range <- renderUI({
    dateRangeInput(
      "dailyrange",
      "Date Range",
      start = Sys.Date() - 14,
      end = Sys.Date(),
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      format = "mm/dd/yy"
    )
  })
  
  output$hourly_range <- renderUI({
    dateRangeInput(
      "hourlyrange",
      "Date Range",
      start = Sys.Date() - 2, #only 2 days because hourly
      end = Sys.Date(),
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      format = "mm/dd/yy"
    )
  })
  
  output$fc_range <- renderUI({
    dateRangeInput(
      "fcrange",
      "Date Range",
      start = Sys.Date() - 14,
      end = Sys.Date(),
      min = ymd("2020-12-30"),
      max = Sys.Date(),
      format = "mm/dd/yy"
    )
  })
  
  # Daily tab ----
  observe({
    if (input$navbar == "Daily") {
      req(input$dailyrange) #wait until input exists
      
      start <- input$dailyrange[1]
      end <- input$dailyrange[2]
      #query API
      daily <- az_daily(start_date = start, end_date = end)
      output$check_daily <- gt::render_gt({
        
        
        #do validation report
        report_daily <- check_daily(daily)
        
        #convert to gt table
        format_report_gt(report_daily, daily)
        
      })
      output$plot_daily <- renderPlot({
       cols_daily <- 
         switch(input$plot_cols_daily,
              "temperature" = cols_daily_temp,
              "precipitation" = cols_daily_precip,
              "wind & sun" = cols_daily_wind_sun
              )
       plot_daily(daily, cols = cols_daily, station = input$station_daily)
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
  
  # Forecast-based tab ----
  
  # Only run forecast-based validations when "Forecast-based" tab is active
  observe({
    if (input$navbar == "Forecast-based") {
      board <- board_connect()
      fc_daily <- board |> pin_read("ericrscott/fc_daily")
      
      output$check_forecast <- 
        gt::render_gt({
          
          req(input$fcrange, fc_daily)
          start <- input$fcrange[1]
          end <- input$fcrange[2]
          
          report_fc <- check_forecast(fc_daily, start, end)
          
          #convert to gt table
          format_report_gt(report_fc, fc_daily)
        })
    }
  })
  
}

shinyApp(ui, server)