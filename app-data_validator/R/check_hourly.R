check_hourly <- function(hourly) {
  hourly <- hourly |>
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
        sol_rad_total, ~all(.x < 0.1),
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
      gte(temp_airC, dwpt - 0.2, na_pass = TRUE),
      "`temp_airC` ≥ `dwpt`") |>
    data.validator::validate_if(lte(wind_spd_mps, wind_spd_max_mps, na_pass = TRUE),
                                "`wind_spd` ≤ `wind_spd_max`") |>
    validate_if(temp_airC_delta, "|∆`temp_airC`| ≤ 19.4") |> 
    validate_if(relative_humidity_delta, "|∆`relative_humidity`| ≤ 50") |>
    validate_if(wind_spd_delta, "|∆`wind_spd_mps`| ≤ 10.3") |> 
    validate_if(sol_rad_total_14 | is.na(sol_rad_total_14),
                "`sol_rad_total` not < 0.1 for more than 14 hrs") |> 
    validate_if(wind_spd_mps_14 | is.na(wind_spd_mps_14),
                "`wind_spd_mps` not < 0.1 for more than 14 hrs") |> 
    validate_if(wind_vector_dir_14 | is.na(wind_vector_dir_14),
                "`wind_vector_dir` not < 1 for more than 14 hrs") |> 
    
    data.validator::add_results(report)
  report
}