check_battery_daily <- function(daily) {
  #Calculate 10 timestep rolling mean of mean voltage (less than 10 days is
  #allowed at start of timeseries) and anomaly (absolute difference) from that
  #rolling mean.
  daily <- 
    daily |> 
    arrange(desc(datetime)) |> 
    mutate(
      meta_bat_mean_rollmean = slide_dbl(meta_bat_volt_mean, mean, .after = 10),
      meta_bat_volt_anomaly = abs(meta_bat_volt_mean - meta_bat_mean_rollmean),
      .by = meta_station_id
    ) 

  #initialize report
  report <- data.validator::data_validation_report()
  
  #validation
  data.validator::validate(daily, name = "Daily") |>
    validate_if(btwn(meta_bat_volt_mean, meta_bat_volt_min, meta_bat_volt_max),
                description = "`meta_bat_volt_*` (min ≤ mean ≤ max)") |> 
    validate_if(meta_bat_volt_min >= 9.6, "`meta_bat_volt_min` ≥ 9.6") |> 
    validate_if(meta_bat_volt_max <= 16, "`meta_bat_volt_max` ≤ 16") |> 
    validate_if(lte(meta_bat_volt_anomaly, 1, na_pass = TRUE), "∆ voltage from 10-day mean ≤ 1") |> 
    add_results(report)
  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of original data where there are problems
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        daily |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    }))
}


check_battery_hourly <- function(hourly) {
  hourly <- 
    hourly |> 
    arrange(desc(date_datetime)) |> 
    mutate(
      meta_bat_volt_rollmean = slide_dbl(meta_bat_volt, mean, .after = 10),
      meta_bat_volt_anomaly = abs(meta_bat_volt - meta_bat_volt_rollmean),
      .by = meta_station_id
    )
  
  #initialize report
  report <- data.validator::data_validation_report()
  
  #validation
  data.validator::validate(hourly, "Hourly") |> 
    validate_if(meta_bat_volt >= 9.6, "`meta_bat_volt` ≥ 9.6") |> 
    validate_if(meta_bat_volt <= 16, "`meta_bat_volt` ≤ 16") |> 
    validate_if(lte(meta_bat_volt_anomaly, 2, na_pass = TRUE), "∆ voltage from 10-hr mean ≤ 2") |> 
    add_results(report)
  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of original data where there are problems
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        daily |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    }))
}
