#' Create validation report for hourly data
#'
#' @param hourly data frame from azmetr::az_hourly()
#'
#' @return a data.validator report
#'
#' @examples
#' hourly <- azmetr::az_hourly()
#' check_hourly(hourly)
check_hourly <- function(hourly) {
  hourly <- hourly |>
    calc_sol_rad_theoretical() |> 
    dplyr::group_by(meta_station_name) |>
    dplyr::arrange(dplyr::desc(date_datetime)) |> #make sure arranged in datetime order
    # Create a few new columns for temporal consistency checks from 'NWS (1994) TSP 88-21-R2':
    mutate(
      temp_airC_delta = check_delta(temp_airC, 19.4),
      relative_humidity_delta = check_delta(relative_humidity, 50),
      wind_spd_delta = check_delta(wind_spd_mps, 10.3)
    ) |> 
    # Temporal consistency ('persistence') checks:
    mutate(
      #sol_rad_total should not be < 0.1 for more than 14 hours
      sol_rad_total_14 = !slider::slide_lgl(
        sol_rad_total, ~all(.x < 0.01),
        .after = 14, #.after because arrange(desc(datetime))
        .complete = TRUE
      ),
      # wind_spd_mps should not be < 0.1 for more than 14 hours
      wind_spd_mps_14 = !slider::slide_lgl(
        wind_spd_mps, ~all(.x < 0.1),
        .after = 14, #.after because arrange(desc(datetime))
        .complete = TRUE
      ),
      # wind_vector_dir should not be < 1 for more than 14 hours
      wind_vector_dir_14 = !slider::slide_lgl(
        wind_vector_dir, ~all(.x < 1),
        .after = 14, #.after because arrange(desc(datetime))
        .complete = TRUE
      )
    ) |> 
    ungroup()
  
  report <- data.validator::data_validation_report()
  data.validator::validate(hourly, name = "Hourly Data") |>
    data.validator::validate_if(# within rounding error
      gte(temp_airC, dwpt, na_pass = TRUE, tol = 0.2),
      "`temp_airC` ≥ `dwpt`") |>
    data.validator::validate_if(lte(wind_spd_mps, wind_spd_max_mps, na_pass = TRUE),
                                "`wind_spd` ≤ `wind_spd_max`") |>
    validate_if(temp_airC_delta, "|∆`temp_airC`| ≤ 19.4") |> 
    validate_if(relative_humidity_delta, "|∆`relative_humidity`| ≤ 50") |>
    validate_if(wind_spd_delta, "|∆`wind_spd_mps`| ≤ 10.3") |> 
    validate_if(lte(sol_rad_total, sol_rad_est, na_pass = TRUE, tol = 0.5),
                "`sol_rad_total` ≤ theoretical max") |> 
    #TODO would be nice if CSV would contain all 14+ hours in a row maybe?
    validate_if(sol_rad_total_14 | is.na(sol_rad_total_14),
                "`sol_rad_total` not < 0.01 for more than 14 hrs") |> 
    validate_if(wind_spd_mps_14 | is.na(wind_spd_mps_14),
                "`wind_spd_mps` not < 0.1 for more than 14 hrs") |> 
    validate_if(wind_vector_dir_14 | is.na(wind_vector_dir_14),
                "`wind_vector_dir` not < 1 for more than 14 hrs") |> 
    data.validator::add_results(report)
  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of data where there are problems
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        hourly |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    })) |> 
    
    #use missing dates tibble for "all stations reporting" validation
    mutate(bad_rows = ifelse(
      table_name == "missing_dates" & type == "error",
      list(
        hourly_ts |>
          tsibble::count_gaps(.full = end(), .name = c("gap_start", "gap_end", "n_missing"))
      ),
      bad_rows
    ))
}

reporting_hourly <- function(hourly) {
  report <- data.validator::data_validation_report()
  # Check that all stations are reporting all dates
  hourly_ts <- 
    hourly |>
    #round 23:59:59 to 00:00:00 of next day
    dplyr::mutate(date_datetime = lubridate::ceiling_date(date_datetime, "hour")) |> 
    tsibble::as_tsibble(key = c(meta_station_id, meta_station_name), index = date_datetime)
  
  hourly_ts |>
    tsibble::has_gaps(.full = end()) |> 
    data.validator::validate(name = 'missing_dates') |>
    data.validator::validate_cols(isFALSE, .gaps, description = "All stations reporting") |>
    add_results(report)
  
  data.validator::validate(hourly, name = "Hourly Data") |> 
    data.validator::validate_if(
      !is.na(temp_soil_10cmC) &
        !is.na(temp_soil_50cmC) |
        meta_station_id %in% c("az28", "az40", "az42", "az43"),
      description = "Soil probes reporting"
    ) |> 
    data.validator::validate_if(
      !is.na(relative_humidity) &
        !is.na(temp_airC),
      description = "Temp and RH sensors reporting"
    ) |> 
    data.validator::validate_if(
      #TODO add 2 min wind
      # Yuma Valley ETo didn't have wind sensors until later
      !is.na(wind_spd_mps) &
        !is.na(wind_vector_dir),
      description = "Wind sensors reporting"
    ) |> 
    data.validator::validate_if(
      !is.na(sol_rad_total),
      description = "Pyranometer reporting"
    ) |> 
    data.validator::validate_if(
      !is.na(precip_total) |
        meta_station_id %in% c("az42", "az43"), #no rain gauge here
      description = "Rain gauge reporting"
    ) |> 
    data.validator::add_results(report)
  

  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of data where there are problems
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        hourly |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    })) |> 
    
    #use missing dates tibble for "all stations reporting" validation
    mutate(bad_rows = ifelse(
      table_name == "missing_dates" & type == "error",
      list(
        hourly_ts |>
          tsibble::count_gaps(.full = end(), .name = c("gap_start", "gap_end", "n_missing"))
      ),
      bad_rows
    ))
}