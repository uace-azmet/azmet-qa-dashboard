library(shiny)
library(data.validator)
library(magrittr)
library(assertr)
daily <- daily |> 
  mutate(wind_spd_mean_mps = if_else(meta_station_name == "Tucson", wind_spd_mean_mps + 2000, wind_spd_mean_mps))

# gte(x,y, na_pass = TRUE)

ui <- fluidPage(
  uiOutput("validation")
)

server <- function(input, output, session) {
  gte <- function(x,y, na_pass = FALSE) {
    res <- x >= y
    if(isTRUE(na_pass)) {
      res <- if_else(is.na(res), TRUE, res)
    }
    res
  }
  
  lte <- function(x,y, na_pass = FALSE) {
    res <- x <= y
    if(isTRUE(na_pass)) {
      res <- if_else(is.na(res), TRUE, res)
    }
    res
  }
  btwn <- function(x, low, high, na_pass = FALSE) {
    res <- between(x, low, high)
    if(isTRUE(na_pass)) {
      res <- if_else(is.na(res), TRUE, res)
    }
    res
  }
  report <- data_validation_report()
  
  validate(daily, name = "Daily Data") |>
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    validate_if(gte(temp_air_meanC, dwpt_mean, na_pass = TRUE), "Mean air temp ≥ dewpoint") |> 
    validate_if(
      btwn(
        temp_air_meanC,
        temp_air_minC,
        temp_air_maxC,
        na_pass = TRUE
      ),
      description = "air temp min ≤ mean ≤ max"
    ) |> 
    validate_if(
      lte(
        wind_spd_mean_mps,
        wind_spd_max_mps,
        na_pass = TRUE
      ),
      description = "mean wind speed ≤ max wind speed"
    ) |> 
    validate_if(
      btwn(
        relative_humidity_mean,
        relative_humidity_min,
        relative_humidity_max,
        na_pass = TRUE
      ),
      description = "RH min ≤ mean ≤ max"
    ) |> 
    validate_if(
      btwn(
        temp_soil_50cm_meanC,
        temp_soil_50cm_minC,
        temp_soil_50cm_maxC,
        na_pass = TRUE
      ),
      description = "soil temp at 50cm min ≤ mean ≤ max"
    ) |> 
    validate_if(
      btwn(
        temp_soil_10cm_meanC,
        temp_soil_10cm_minC,
        temp_soil_10cm_maxC,
        na_pass = TRUE
      ),
      description = "soil temp at 10cm min ≤ mean ≤ max"
    ) |> 
    add_results(report)
  
  output$validation <- renderUI({
    render_semantic_report_ui(get_results(report))
  })
}

shinyApp(ui, server)