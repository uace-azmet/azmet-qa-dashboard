library(shiny)
library(bslib)
library(bsicons)
library(shinybusy)
library(shinyWidgets)
library(htmltools)

library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)
library(plotly)
library(slider)
library(units)
library(patchwork)
library(hms)
library(TrenchR)
library(tsibble)
library(rsconnect)

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
        minDate = min(azmetr::station_info$start_date),
        maxDate = Sys.Date() - 1,
        update_on = "close",
        addon = "none"
      ),
      # really just for testing purposes.  Can be removed if desired
      checkboxInput(
        "test_daily",
        span(
          "Use test data",
          tooltip(
            bs_icon("info-circle"),
            "Use fake data with known errors for all validations"
          )
        ),
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
        minDate = min(azmetr::station_info$start_date),
        maxDate = Sys.Date(),
        update_on = "close",
        addon = "none"
      ),
      p(
        bsicons::bs_icon("info-circle"),
        "If today's date is selected as the end date, data will be requested through the previous hour."
      ),
      checkboxInput(
        "test_hourly",
        span(
          "Use test data",
          tooltip(
            bs_icon("info-circle"),
            "Use fake data with known errors for all validations"
          )
        ),
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
      ),
      p(
        bsicons::bs_icon("info-circle"),
        "If today's date is selected as the end date, hourly data is to the previous hour and daily data to yesterday."
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
        gt_output(outputId = "reporting_daily"),
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
          plotOutput(outputId = "plot_daily")
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
        gt_output(outputId = "reporting_hourly"),
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
          plotOutput(outputId = "plot_hourly")
        )
      )
    )
  ),

  ## Battery ----
  nav_panel(
    "Battery",
    layout_column_wrap(
      width = 1 / 2,
      height = "100%",
      card(
        full_screen = TRUE,
        card_header("Validation"),
        gt_output(outputId = "check_battery_daily"),
        gt_output(outputId = "check_battery_hourly")
      ),
      layout_column_wrap(
        width = 1,
        navset_card_tab(
          full_screen = TRUE,
          title = "Timeseries",
          nav_panel(
            "Daily",
            plotlyOutput(outputId = "plot_battery_daily")
          ),
          nav_panel(
            "Hourly",
            plotlyOutput(outputId = "plot_battery_hourly")
          )
        ),
        navset_card_tab(
          full_screen = TRUE,
          title = "Voltage",
          nav_panel(
            "Min Temp",
            plotlyOutput(outputId = "plot_battery_min_temp")
          ),
          nav_panel(
            "Max Temp",
            plotlyOutput(outputId = "plot_battery_max_temp")
          ),
          nav_panel(
            "Solar Radiation",
            plotlyOutput(outputId = "plot_battery_sol_rad")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Daily tab ----
  # observe({ if (input$navbar == "Daily")... is used to only run the code in this navbar tab when the navbar tab is active
  observe({
    if (input$navbar == "Daily") {
      shinybusy::show_modal_spinner(
        "semipolar",
        color = "#AB0520",
        text = "Fetching data ..."
      )

      req(input$dailyrange) #wait until input exists

      start <- input$dailyrange[1]
      end <- input$dailyrange[2]

      if (is.na(end)) {
        end <- start
      }

      #temporary just for testing
      if (input$test_daily) {
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
        format_report_gt(report_daily, daily, title = "Data Values")
      })
      output$reporting_daily <- gt::render_gt({
        input$dailyrange
        format_report_gt(
          reporting_daily(daily),
          daily,
          title = "Data Reporting"
        )
      })
      output$plot_daily <- renderPlot({
        #reload when input changes
        input$dailyrange

        cols_daily <-
          switch(
            input$plot_cols_daily,
            "Temperature" = cols_daily_temp,
            "Precip & Sun" = cols_daily_precip,
            "Wind" = cols_daily_wind
          )
        plot_daily(daily, cols = cols_daily, station = input$station_daily)
      })

      shinybusy::remove_modal_spinner()
    }
  })
  
  # Hourly tab ----
  observe({
    if (input$navbar == "Hourly") {
      shinybusy::show_modal_spinner(
        "semipolar",
        color = "#AB0520",
        text = "Fetching data ..."
      )

      req(input$hourlyrange) #wait until input exists

      print(input$hourlyrange)

      start <- input$hourlyrange[1] |> as.Date()
      end <- input$hourlyrange[2] |> as.Date()

      # If only a single date is selected, set end date to start date
      if (is.na(end)) {
        end <- as.Date(start)
      }
      # If end date is today, assume user wants most recent data
      if (end == today()) {
        end <- now() - hours(1)
      }

      
#temporary for testing
      if (input$test_hourly) {
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
        format_report_gt(report_hourly, hourly, title = "Data Values")
      })
      output$reporting_hourly <- gt::render_gt({
        input$hourlyrange
        format_report_gt(
          reporting_hourly(hourly),
          hourly,
          title = "Data Reporting"
        )
      })
      output$plot_hourly <- renderPlot({
        # force reload as soon as input changes
        input$hourlyrange

        cols_hourly <-
          switch(
            input$plot_cols_hourly,
            "Temperature" = cols_hourly_temp,
            "Precip & Sun" = cols_hourly_precip,
            "Wind" = cols_hourly_wind
          )
        plot_hourly(hourly, cols = cols_hourly, station = input$station_hourly)
      })
      shinybusy::remove_modal_spinner()
    }
  })
  
  
  # Battery tab -------------------------------------------------------------
  
  observe({
    if(input$navbar == "Battery") {
      shinybusy::show_modal_spinner(
        "semipolar", color = "#AB0520", text = "Fetching data ...")
      
      req(input$batteryrange)
      
      # # to convert to date object (maybe not necessary?)
      start <- input$batteryrange[1] |> as.Date()
      end <- input$batteryrange[2] |> as.Date()

      # If only a single date is selected
      if (is.na(end)) {
        end <- start
      }

      # If the end date is today assume user wants most recent data
      if (end == today()) {
        end_daily <- end - days(1)
        end_hourly <- now() - hours(1)
      } else {
        end_daily <- end_hourly <- end
      }
      daily <- az_daily(start_date = start, end_date = end_daily)
      hourly <- az_hourly(start_date_time = start, end_date_time = end_hourly)
      
      output$check_battery_daily <- gt::render_gt({
        # force reload as soon as input changes
        input$batteryrange
        
        #do validation report
        report_battery_daily <- check_battery_daily(daily)
        
        #convert to gt table
        format_report_gt(report_battery_daily, daily, title = "Daily")
      })
      
      output$check_battery_hourly <- gt::render_gt({
        # force reload as soon as input changes
        input$batteryrange
        
        #do validation report
        report_battery_hourly <- check_battery_hourly(hourly)
        
        #convert to gt table
        format_report_gt(report_battery_hourly, hourly, title = "Hourly")
      })
      
      #TODO move plotting code to function?
      output$plot_battery_hourly <- renderPlotly({
        #TODO speed up by making with pure plotly
        h_time <-
          ggplot(hourly, aes(x = date_datetime, y = meta_bat_volt)) +
          geom_line(aes(color = meta_station_name)) +
          geom_hline(aes(yintercept = 9.6), color = "red") +
          geom_hline(aes(yintercept = 16), color = "orange") +
          geom_hline(aes(yintercept = 20), color = "red") +
          scale_y_continuous(limits = range(hourly$meta_bat_volt, na.rm = TRUE)) +
          theme_bw() +
          theme(legend.position = "none",  axis.title.x = element_blank())
        ggplotly(h_time)
      })
      
      output$plot_battery_daily <- renderPlotly({
        #TODO: speed up by making in pure plotly
        h_daily <-
          daily |> 
          ggplot(aes(x = datetime, y = meta_bat_volt_mean)) +
          geom_ribbon(aes(ymin = meta_bat_volt_min,
                          ymax = meta_bat_volt_max,
                          fill = meta_station_name),
                      alpha = 0.15) +
          geom_line(aes(color = meta_station_name)) +
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
      
      # Scatter plots
      output$plot_battery_min_temp <- renderPlotly({
        plot_voltage(daily, "temp_air_minC")
      })
      output$plot_battery_max_temp <- renderPlotly({
        plot_voltage(daily, "temp_air_maxC")
      })
      output$plot_battery_sol_rad <- renderPlotly({
        plot_voltage(daily, "sol_rad_total")
      })
      shinybusy::remove_modal_spinner()
    }
  })  
  
}

shinyApp(ui, server)