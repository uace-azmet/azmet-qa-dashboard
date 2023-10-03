library(shiny)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(shinyWidgets)
library(htmltools)

library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(pins)
library(arrow)
library(plotly)
library(slider)
library(units)
library(patchwork)

# Everything in R/ should be sourced automatically, but I found I needed to add
# these to get it to work.  Not sure why.
source("R/helpers.R")
source("R/check_daily.R")
source("R/format_report_gt.R")

fc_popup <- popover(
  bs_icon("info-circle"),
  markdown("A timeseries model is fit to past data, a forecast for the current day is made, and that forecast is compared to observed values. Values outside the 99% predictive interval of the forecast don't pass the validation. **Interpret with caution**—a failing validation does not necessarily indicate a problem and could just be an extreme event. Read more about the model specifics [here](https://github.com/uace-azmet/azmet-forecast-qa#readme).")
)

# UI -----
ui <- page_navbar(
  # Application title
  title = "AZMET QA",
  id = "navbar",
  ## Sidebar ----
  sidebar = sidebar(
    bg = "darkgrey",
    # Makes sidebar conditional on active nav tab.  Separate sidebars are needed
    # for each tab.
    conditionalPanel(
      "input.navbar === 'Daily'",
      myAirDatepickerInput(
        inputId = "dailyrange",
        label = "Date Range",
        value = c(Sys.Date() - 15, Sys.Date() - 1),
        range = TRUE,
        separator = " – ",
        dateFormat = "MM/dd/yy",
        minDate = "2020-12-30",
        maxDate = Sys.Date() - 1,
        update_on = "close",
        addon = "none"
      ),
      # really just for testing purposes.  Can be removed if desired
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
      myAirDatepickerInput(
        inputId = "hourlyrange",
        label = "Date Range",
        value = c(Sys.Date() - 7, Sys.Date()), #only 7 days because hourly
        range = TRUE,
        separator = " – ",
        dateFormat = "MM/dd/yy",
        minDate = "2020-12-30",
        maxDate = Sys.Date(),
        update_on = "close",
        addon = "none"
      ),
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
      myAirDatepickerInput(
        inputId = "fcrange",
        label = "Date Range",
        value = c(Sys.Date() - 15, Sys.Date() - 1),
        range = TRUE,
        separator = " – ",
        dateFormat = "MM/dd/yy",
        minDate = "2023-02-05",
        maxDate = Sys.Date() - 1,
        update_on = "close",
        addon = "none"
      )
    ),
    conditionalPanel(
      "input.navbar === 'Battery'",
      myAirDatepickerInput(
        inputId = "batteryrange",
        label = "Date Range",
        value = c(Sys.Date() - 7, Sys.Date()),
        range = TRUE,
        separator = " – ",
        dateFormat = "MM/dd/yy",
        minDate = "2020-12-30",
        maxDate = Sys.Date(),
        update_on = "close",
        addon = "none"
      )
    )
  ), 
  ## Daily ----
  
  nav_panel(
    title = "Daily",
    # shinybusy::add_busy_spinner("semipolar", color = "#EF4056", position = "bottom-left", margins = c(50, 50)),
    layout_column_wrap(
      height = "100%",
      width = NULL,
      fill = FALSE,
      # Make the plot area 1.5 times that of table area
      style = css(grid_template_columns = "1fr 1.5fr"),
      # card for validation table
      card(
        # max_height = 250,
        full_screen = TRUE,
        card_header(
          "Daily Data Validation"
        ),
        gt_output(outputId = "check_daily")
      ),
      
      # card for plots with its own sidebar inputs for station and variables
      
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
              choices = c("Temperature", "Precip & Sun", "Wind")
            )
          ),
          # Unfortunately it is not easy to get plots to just fill their
          # containers dynamically.  I chose this height based on what looks
          # good on my laptop, but this could be adjusted.
          plotOutput(outputId = "plot_daily", height = 550) 
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
        gt_output(outputId = "check_hourly") 
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
              choices = c("Temperature", "Precip & Sun", "Wind")
            )
          ),
          plotOutput(outputId = "plot_hourly", height = 550) 
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
          fc_popup, #defined at top of this file
          class = "d-flex justify-content-between" # related to getting the info button in the right corner
        ),
        gt_output(outputId = "check_forecast") 
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
              choices = c("Temperature", "Precip & Sun", "Wind")
            )
          ),
          plotOutput(outputId = "plot_fc", height = 550) 
        )
      )
    )
  ),
  ## Battery ----
  nav_panel(
    title = "Battery",
    layout_column_wrap(
      width = NULL,
      height = "100%",
      fill = FALSE,
      style = css(grid_template_columns = "1fr 1.5fr"),
      card(
        full_screen = TRUE,
        card_header(
          "Daily Data"
        ),
        gt_output(outputId = "check_battery_daily") 
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Daily Data"
        ),
        plotlyOutput(outputId = "plot_battery_daily", height = "300px") 
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Hourly Data"
        ),
        gt_output(outputId = "check_battery_hourly") 
      ),
      card(
        full_screen = TRUE,
        card_header(
          "Hourly Data"
        ),
        plotlyOutput(outputId = "plot_battery_hourly", height = "300px") 
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Daily tab ----
  # observe({ if (input$navbar == "Daily")... is used to only run the code in this navbar tab when the navbar tab is active
  observe({
    if (input$navbar == "Daily") {
      req(input$dailyrange) #wait until input exists
      
      shinybusy::show_modal_spinner(
        "semipolar", color = "#AB0520", text = "Fetching data ...")
      
      start <- input$dailyrange[1]
      end <- input$dailyrange[2]
      
      #temporary just for testing
      if(input$test_daily) { 
        daily <- read_csv("testdata_daily.csv") 
      } else { 
        #query API
        daily <- az_daily(start_date = start, end_date = end)
      } 
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
                 "Precip & Sun" = cols_daily_precip,
                 "Wind" = cols_daily_wind)
        plot_daily(daily, cols = cols_daily, station = input$station_daily)
      })
      
      shinybusy::remove_modal_spinner()
    }
  })
  
  # Hourly tab ----
  observe({
    if (input$navbar == "Hourly") {
      req(input$hourlyrange) #wait until input exists
      
      shinybusy::show_modal_spinner(
        "semipolar", color = "#AB0520", text = "Fetching data ...")
      
      #to convert to datetime
      start <- input$hourlyrange[1] |> as.POSIXct() |> format_ISO8601() 
      end <- input$hourlyrange[2] |> as.POSIXct() |> format_ISO8601()
      
      #temporary for testing
      if(input$test_hourly) {
        hourly <- read_csv("testdata_hourly.csv")
      } else {
        #query API
        hourly <- az_hourly(start_date_time = start, end_date_time = end)
      }
      
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
                 "Precip & Sun" = cols_hourly_precip,
                 "Wind" = cols_hourly_wind)
        plot_hourly(hourly, cols = cols_hourly, station = input$station_hourly)
      })
      shinybusy::remove_modal_spinner()
    }
  })
  
  # Forecast-based tab ----
  observe({
    if (input$navbar == "Forecast-based") {
      
      shinybusy::show_modal_spinner(
        "semipolar", color = "#AB0520", text = "Fetching data ...")
      
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
          
          #run validation
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
                 "Precip & Sun" = cols_daily_precip,
                 "Wind" = cols_daily_wind)
        plot_fc(fc_daily, cols = cols_fc, station = input$station_fc)
      })
    shinybusy::remove_modal_spinner()
  })
  
  
  # Battery tab -------------------------------------------------------------
  
  observe({
    if(input$navbar == "Battery") {
      req(input$batteryrange)
      
      shinybusy::show_modal_spinner(
        "semipolar", color = "#AB0520", text = "Fetching data ...")
      
      #to convert to datetime
      start <- input$batteryrange[1] |> as.POSIXct() |> format_ISO8601() 
      end   <- input$batteryrange[2] |> as.POSIXct() |> format_ISO8601()
      
      daily <- az_daily(start_date = start, end_date = end)
      hourly <- az_hourly(start_date_time = start, end_date_time = end)
      
      output$check_battery_daily <- gt::render_gt({
        # force reload as soon as input changes
        input$batteryrange
        
        #do validation report
        report_battery_daily <- check_battery_daily(daily)
        
        #convert to gt table
        format_report_gt(report_battery_daily, daily)
      })
      
      output$check_battery_hourly <- gt::render_gt({
        # force reload as soon as input changes
        input$batteryrange
        
        #do validation report
        report_battery_hourly <- check_battery_hourly(hourly)
        
        #convert to gt table
        format_report_gt(report_battery_hourly, hourly)
      })
      
      #TODO move plotting code to function?
      output$plot_battery_hourly <- renderPlotly({
        h_time <-
          ggplot(hourly, aes(x = date_datetime, y = meta_bat_volt)) +
          geom_line(aes(color = meta_station_id)) +
          geom_hline(aes(yintercept = 9.6), color = "red") +
          geom_hline(aes(yintercept = 16), color = "orange") +
          geom_hline(aes(yintercept = 20), color = "red") +
          scale_y_continuous(limits = range(hourly$meta_bat_volt, na.rm = TRUE)) +
          theme_bw() +
          theme(legend.position = "none",  axis.title.x = element_blank())
        ggplotly(h_time)
      })
      
      output$plot_battery_daily <- renderPlotly({
        h_daily <-
          daily |> 
          ggplot(aes(x = datetime, y = meta_bat_volt_mean)) +
          geom_ribbon(aes(ymin = meta_bat_volt_min,
                          ymax = meta_bat_volt_max,
                          fill = meta_station_id),
                      alpha = 0.15) +
          geom_line(aes(color = meta_station_id)) +
          geom_hline(aes(yintercept = 9.6), color = "red") +
          geom_hline(aes(yintercept = 16), color = "orange") +
          geom_hline(aes(yintercept = 20), color = "red") +
          scale_y_continuous(
            limits = c(min(daily$meta_bat_volt_min, na.rm = TRUE),
                       max(daily$meta_bat_volt_max, na.rm = TRUE))
          ) +
          theme_bw() +
          theme(legend.position = "none", axis.title.x = element_blank())
        
        ggplotly(h_daily)
      })
      shinybusy::remove_modal_spinner()
    }
  })  
  
}

shinyApp(ui, server)