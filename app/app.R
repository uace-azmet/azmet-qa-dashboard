library(shiny)
library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(pins)
library(arrow)
library(bslib)
library(shinycssloaders)

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
    #Makes sidebar conditional on active nav tab
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
  ## Daily ----
  nav_panel(
    title = "Daily",
    layout_column_wrap(
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
          plotOutput(outputId = "plot_daily") |> withSpinner(4)
        )
      )
    )
  ),
  ## Hourly ----
  nav_panel(
    title = "Hourly",
    layout_column_wrap(
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
          plotOutput(outputId = "plot_hourly") |> withSpinner(4)
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
          "Forecast-Based Validation"
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
          plotOutput(outputId = "plot_fc") |> withSpinner(4)
        )
      )
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
      #query API
      hourly <- az_hourly(start_date_time = start, end_date_time = end)
      
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