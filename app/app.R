library(shiny)
library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(pins)
library(arrow)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(shinyWidgets)

source("R/helpers.R")
source("R/check_daily.R")
source("R/format_report_gt.R")

# UI -----
ui <- page_navbar(
  # Application title
  title = "AZMET QA",
  id = "navbar",
  ## Sidebar ----
  sidebar = sidebar(
    bg = "darkgrey",
    #Makes sidebar conditional on active nav tab
    conditionalPanel(
      "input.navbar === 'Daily'",
      uiOutput("daily_range"),
      checkboxInput(
        "test_daily",
        span("Use test data",
             tooltip(
               bs_icon("info-circle"),
               "Use fake data with known errors for all validations"
             )),
        value = FALSE
      )
    ),
    conditionalPanel(
      "input.navbar === 'Hourly'",
      uiOutput("hourly_range"),
      checkboxInput(
        "test_hourly",
        span("Use test data",
             tooltip(
               bs_icon("info-circle"),
               "Use fake data with known errors for all validations"
             )),
        value = FALSE
      )
    ),
    conditionalPanel(
      "input.navbar === 'Forecast-based'",
      uiOutput("fc_range")
    )
  ), 
  ## Daily ----
  nav_panel(
    title = "Daily",
    layout_column_wrap(
      height = "100%",
      width = NULL,
      fill = FALSE,
      # plot area 1.5 times that of table area
      style = css(grid_template_columns = "1fr 1.5fr"),
      #card for validation table
      card(
        # max_height = 250,
        full_screen = TRUE,
        card_header(
          "Daily Data Validation"
        ),
        gt_output(outputId = "check_daily") |> withSpinner(4)
      ),
      
      #card for plots with its own sidebar inputs
      card(
        full_screen = TRUE,
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = "200px",
            shiny::selectInput(
              "station_daily",
              "Station",
              choices = azmetr::station_info$meta_station_name,
              multiple = FALSE
            ),
            shiny::selectInput(
              "plot_cols_daily",
              "Variables",
              choices = c("Temperature", "Precipitation", "Wind & Sun")
            )
          ),
          plotOutput(outputId = "plot_daily", height = 550) |> withSpinner(4)
        )
      )
    )
  ),
  ## Hourly ----
  nav_panel(
    title = "Hourly",
    layout_column_wrap(
      height = "100%",
      width = NULL,
      fill = FALSE,
      # plot area 1.5 times that of table area
      style = css(grid_template_columns = "1fr 1.5fr"),
      card(
        full_screen = TRUE,
        card_header(
          "Hourly Data Validation"
        ),
        gt_output(outputId = "check_hourly") |> withSpinner(4)
      ),
      card(
        full_screen = TRUE,
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = "200px",
            shiny::selectInput(
              "station_hourly",
              "Station",
              choices = azmetr::station_info$meta_station_name,
              multiple = FALSE
            ),
            shiny::selectInput(
              "plot_cols_hourly",
              "Variables",
              choices = c("Temperature", "Precipitation", "Wind & Sun")
            )
          ),
          plotOutput(outputId = "plot_hourly", height = 550) |> withSpinner(4)
        )
      )
    )
  ),
  ## Forecast-based ----
  nav_panel(
    title = "Forecast-based",
    layout_column_wrap(
      width = NULL,
      height = "100%",
      fill = FALSE,
      # plot area 1.5 times that of table area
      style = css(grid_template_columns = "1fr 1.5fr"),
      card(
        full_screen = TRUE,
        card_header(
          "Forecast-Based Validation",
          tooltip(
            bs_icon("info-circle"),
            "tooltip message"
          )
        ),
        gt_output(outputId = "check_forecast") |> withSpinner(4)
      ),
      card(
        full_screen = TRUE,
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = "200px",
            shiny::selectInput(
              "station_fc",
              "Station",
              choices = azmetr::station_info$meta_station_name,
              multiple = FALSE
            ),
            shiny::selectInput(
              "plot_cols_fc",
              "Variables",
              choices = c("Temperature", "Precipitation", "Wind & Sun")
            )
          ),
          plotOutput(outputId = "plot_fc", height = 550) |> withSpinner(4)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Date selector inputs -----
  output$daily_range <- renderUI({
    myAirDatepickerInput(
      inputId = "dailyrange",
      label = "Date Range",
      value = c(Sys.Date() - 14, Sys.Date()),
      range = TRUE,
      separator = " – ",
      dateFormat = "MM/dd/yy",
      minDate = "2020-12-30",
      maxDate = Sys.Date(),
      update_on = "close",
      addon = "none"
    )
  })
  
  output$hourly_range <- renderUI({
    myAirDatepickerInput(
      inputId = "hourlyrange",
      label = "Date Range",
      value = c(Sys.Date() - 2, Sys.Date()), #only 2 days because hourly
      range = TRUE,
      separator = " – ",
      dateFormat = "MM/dd/yy",
      minDate = "2020-12-30",
      maxDate = Sys.Date(),
      update_on = "close",
      addon = "none"
    )
  })
  
  output$fc_range <- renderUI({
    myAirDatepickerInput(
      inputId = "fcrange",
      label = "Date Range",
      value = c(Sys.Date() - 14, Sys.Date()), #only 2 days because hourly
      range = TRUE,
      separator = " – ",
      dateFormat = "MM/dd/yy",
      minDate = "2020-12-30",
      maxDate = Sys.Date(),
      update_on = "close",
      addon = "none"
    )
  })
  
  # Daily tab ----
  observe({
    if (input$navbar == "Daily") {
      req(input$dailyrange) #wait until input exists
      
      start <- input$dailyrange[1]
      end <- input$dailyrange[2]
      
      #temporary just for testing
      if(input$test_daily) { #
        daily <- read_csv("testdata_daily.csv") #
      } else { #
        #query API
        daily <- az_daily(start_date = start, end_date = end)
      } #
      output$check_daily <- gt::render_gt({
        #reload when input changes
        input$dailyrange
        
        #do validation report
        report_daily <- check_daily(daily)
        
        #convert to gt table
        format_report_gt(report_daily, daily)
        
      })
      output$plot_daily <- renderPlot({
        #reload when input changes
        input$dailyrange
        
        cols_daily <- 
          switch(input$plot_cols_daily,
                 "Temperature" = cols_daily_temp,
                 "Precipitation" = cols_daily_precip,
                 "Wind & Sun" = cols_daily_wind_sun)
        plot_daily(daily, cols = cols_daily, station = input$station_daily)
      })
    }
  })
  
  # Hourly tab ----
  observe({
    if (input$navbar == "Hourly") {
      req(input$hourlyrange) #wait until input exists
      
      start <- input$hourlyrange[1] |> as.POSIXct() |> format_ISO8601() #to convert to datetime
      end <- input$hourlyrange[2] |> as.POSIXct() |> format_ISO8601()
      
      #temporary for testing
      if(input$test_hourly) { #
        hourly <- read_csv("testdata_hourly.csv") #
      } else { #
        #query API
        hourly <- az_hourly(start_date_time = start, end_date_time = end)
      } #
      
      output$check_hourly <- gt::render_gt({
        # force reload as soon as input changes
        input$hourlyrange
        
        #do validation report
        report_hourly <- check_hourly(hourly)
        
        #convert to gt table
        format_report_gt(report_hourly, hourly)
      })
      output$plot_hourly <- renderPlot({
        # force reload as soon as input changes
        input$hourlyrange
        
        cols_hourly <- 
          switch(input$plot_cols_hourly,
                 "Temperature" = cols_hourly_temp,
                 "Precipitation" = cols_hourly_precip,
                 "Wind & Sun" = cols_hourly_wind_sun)
        plot_hourly(hourly, cols = cols_hourly, station = input$station_hourly)
      })
    }
  })
  
  # Forecast-based tab ----
  
  # Only run forecast-based validations when "Forecast-based" tab is active
  observe({
    if (input$navbar == "Forecast-based") {
      board <- board_connect()
      fc_daily <- board |> pin_read("ericrscott/fc_daily")
      req(input$fcrange, fc_daily)
      start <- input$fcrange[1]
      end <- input$fcrange[2]
      fc_daily <- fc_daily |>
        filter(datetime > start & datetime <= end) |> 
        arrange(varname)
      
      output$check_forecast <- 
        gt::render_gt({
          #reload when input changes
          input$fcrange
          
          report_fc <- check_forecast(fc_daily)
          
          #convert to gt table
          format_report_gt(report_fc, fc_daily)
        })
    }
    output$plot_fc <- 
      renderPlot({
        # force reload as soon as input changes
        input$fcrange
        
        cols_fc <- 
          switch(input$plot_cols_fc,
                 "Temperature" = cols_daily_temp,
                 "Precipitation" = cols_daily_precip,
                 "Wind & Sun" = cols_daily_wind_sun)
        plot_fc(fc_daily, cols = cols_fc, station = input$station_fc)
      })
  })
  
}

shinyApp(ui, server)