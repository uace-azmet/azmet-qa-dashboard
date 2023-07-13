check_hourly <- function(hourly, al) {
  hourly_check <- hourly |>
    create_agent(
      tbl_name = "Hourly measures",
      label = "Consistency Checks",
      actions = al
    ) |>
    
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    col_vals_gte(
      temp_airC,
      vars(dwpt),
      na_pass = TRUE,
      #account for rounding error I guess?
      preconditions = function(x) x |> mutate(dwpt = dwpt - 0.2)
    ) |>
    col_vals_lte(wind_spd_mps, vars(wind_spd_max_mps), na_pass = TRUE) |>
    
    # Temporal consistency checks from 'NWS (1994) TSP 88-21-R2':
    col_vals_lt(
      temp_airC_delta,
      19.4,
      na_pass = TRUE,
      brief = "Expect that |∆`temp_airC`| < 19.4",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(temp_airC_delta = abs(temp_airC - lag(temp_airC)),
               .after = temp_airC) |>
        ungroup()
    ) |>
    col_vals_lt(
      relative_humidity_delta,
      50,
      na_pass = TRUE,
      brief = "Expect that |∆`relative_humidity`| < 50",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(relative_humidity_delta = abs(relative_humidity - lag(relative_humidity)),
               .after = relative_humidity) |>
        ungroup()
    ) |>
    col_vals_lt(
      wind_spd_mps_delta,
      10.3,
      na_pass = TRUE,
      brief = "Expect that |∆`wind_spd_mps`| < 10.3",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(wind_spd_mps_delta = abs(wind_spd_mps - lag(wind_spd_mps)),
               .after = wind_spd_mps) |>
        ungroup()
    ) |>
    
    # Temporal consistency ('persistence') checks:
    col_vals_equal(
      sol_rad_total_14,
      FALSE, #true means < 1 for the past 14 hours
      na_pass = TRUE,
      brief = "Expect that sol_rad_total should not be < 1 for more than 14 hours",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(
          sol_rad_total_14 = slider::slide_lgl(
            sol_rad_total, ~all(.x < 0.01),
            .after = 14, #.after because arrange(desc(datetime))
            .complete = TRUE
          )
        ) |> ungroup()
    ) |>
    col_vals_equal(
      wind_spd_mps_14,
      FALSE, #true means < 1 for the past 14 hours
      na_pass = TRUE,
      brief = "Expect that wind_spd_mps should not be < 1 for more than 14 hours",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(
          #slide_lgl turns an anonymous function into a sliding window function
          wind_spd_mps_14 = slider::slide_lgl(
            wind_spd_mps, ~all(.x < 0.01),
            .after = 14, #.after because arrange(desc(datetime))
            .complete = TRUE
          )
        ) |> ungroup()
    ) |>
    col_vals_equal(
      wind_vector_dir_14,
      FALSE, #true means < 1 for the past 14 hours
      na_pass = TRUE,
      brief = "Expect that wind_vector_dir should not be < 1 for more than 14 hours",
      preconditions = function(x) x |>
        group_by(meta_station_id) |>
        mutate(
          wind_vector_dir_14 = slider::slide_lgl(
            wind_vector_dir, ~all(.x < 1),
            .after = 14, #.after because arrange(desc(datetime))
            .complete = TRUE
          )
        ) |> ungroup()
    ) |>
    interrogate()
  
  get_agent_report(hourly_check, title = "Hourly Data Consistency Check")
}