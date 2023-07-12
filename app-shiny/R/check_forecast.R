check_forecast <- function(fc_daily, start, end, al) {
  forecast_validation <- 
    fc_daily |>  
    filter(datetime > start & datetime <= end) |> 
    create_agent(
      tbl_name = "Daily Measures",
      label = "Forecast-Based Validations",
      actions = al
    ) |> 
    col_vals_between(
      obs, vars(lower_99), vars(upper_99), segments = vars(varname)
    ) |> 
    interrogate()
  get_agent_report(forecast_validation, title = "Forecast-based Validation (Daily Data)")
}
