library(shiny)
library(data.validator)
library(tidyverse)
library(azmetr)
library(gt)

ui <- fluidPage(
  uiOutput("daily_range"),
  gt::gt_output("validation")
)

server <- function(input, output, session) {
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
  
  output$validation <- gt::render_gt({
    req(input$dailyrange) #wait until input exists
    
    start <- input$dailyrange[1]
    end <- input$dailyrange[2]
    #query API
    daily <- az_daily(start_date = start, end_date = end)
    
    #do validation report
    report <- check_daily(daily)
    
    #convert to gt table
    format_report_gt(report, daily)
  })
}

shinyApp(ui, server)