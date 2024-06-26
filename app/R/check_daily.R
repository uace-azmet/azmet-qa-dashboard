#' Create validation report for daily data
#'
#' @param daily data frame from azmetr::az_daily()
#'
#' @return a data.validator report
#' @examples
#' library(azmetr)
#' library(data.validator)
#' daily <- az_daily(start = "2023-07-01", end = "2023-07-12")
#' check_daily(daily)
check_daily <- function(daily) {
  report <- data.validator::data_validation_report()
  
  daily <- calc_sol_rad_theoretical(daily, freq = "daily") 
  
  data.validator::validate(daily, name = "Daily Data") |>
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    data.validator::validate_if(gte(temp_air_meanC, dwpt_mean, na_pass = TRUE),
                                "`temp_air_meanC` ≥ `dwpt_mean`") |>
    data.validator::validate_if(
      btwn(
        temp_air_meanC,
        temp_air_minC,
        temp_air_maxC,
        na_pass = TRUE
      ),
      description = "`temp_air_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      lte(
        wind_spd_mean_mps,
        wind_spd_max_mps,
        na_pass = TRUE
      ),
      description = "`wind_spd_mean_mps` ≤ `wind_spd_max_mps`"
    ) |>
    validate_if(lte(sol_rad_total, sol_rad_est, na_pass = TRUE),
                "`sol_rad_total` ≤ theoretical max") |> 
    data.validator::validate_if(
      btwn(
        relative_humidity_mean,
        relative_humidity_min,
        relative_humidity_max,
        na_pass = TRUE
      ),
      description = "`relative_humidity_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      btwn(
        temp_soil_50cm_meanC,
        temp_soil_50cm_minC,
        temp_soil_50cm_maxC,
        na_pass = TRUE
      ),
      description = "`temp_soil_50cm_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      btwn(
        temp_soil_10cm_meanC,
        temp_soil_10cm_minC,
        temp_soil_10cm_maxC,
        na_pass = TRUE
      ),
      description = "`temp_soil_10cm_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::add_results(report)
  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of original data where there are
    # problems.  This creates the data frame that will be downloaded with the
    # "CSV" button in the table
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        daily |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    })) |> 
    
    # use missing dates tibble for "all stations reporting" validation.  This
    # creates the data frame that will be downloaded with the "CSV" button in the
    # table
    mutate(bad_rows = ifelse(
      table_name == "missing_dates" & type == "error",
      list(
        daily |>
          tsibble::as_tsibble(key = c(meta_station_id, meta_station_name), index = datetime) |>
          tsibble::count_gaps(.full = end(), .name = c("gap_start", "gap_end", "n_missing"))
      ),
      bad_rows
    ))
  
}

reporting_daily <- function(daily) {
  report <- data.validator::data_validation_report()
  # # Check that all stations are reporting all dates
  # # Easiest way to do that is to convert to a tsibble and use `has_gaps()`
  daily |>
    tsibble::as_tsibble(key = c(meta_station_id, meta_station_name), index = datetime) |>
    tsibble::has_gaps(.full = end()) |>
    data.validator::validate(name = 'missing_dates') |>
    data.validator::validate_cols(isFALSE, .gaps, description = "All stations reporting") |>
    data.validator::add_results(report)
  
  data.validator::validate(daily, name = "Daily Data") |> 
    data.validator::validate_if(
      !is.na(temp_soil_10cm_meanC) &
        !is.na(temp_soil_50cm_meanC) |
        meta_station_id %in% c("az28", "az40", "az42", "az43", "az44"), #no soil probes here
      description = "Soil probes reporting"
    ) |>
    data.validator::validate_if(
      !is.na(relative_humidity_mean) &
        !is.na(temp_air_meanC),
      description = "Temp and RH sensors reporting"
    ) |>
    data.validator::validate_if(
      #TODO:
      # wind sensors weren't installed at Yuma Valley ETo until later
      # 2min wind variables weren't collected until later
      !is.na(wind_spd_mean_mps) &
        # !is.na(wind_2min_spd_mean_mps) & #would need to also include datetime > start of collection
        # !is.na(wind_2min_vector_dir) &
        !is.na(wind_vector_dir),
      description = "Wind sensors reporting"
    ) |>
    data.validator::validate_if(
      !is.na(sol_rad_total),
      description = "Pyranometer reporting"
    ) |>
    data.validator::validate_if(
      !is.na(precip_total_mm) |
        meta_station_id %in% c("az42", "az43"), #no rain gauge here
      description = "Rain gauge reporting"
    ) |>
    data.validator::add_results(report)
  
  get_results(report) |> 
    # make `bad_rows` list-column with slices of original data where there are
    # problems.  This creates the data frame that will be downloaded with the
    # "CSV" button in the table
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        daily |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    })) |> 
    
    # use missing dates tibble for "all stations reporting" validation.  This
    # creates the data frame that will be downloaded with the "CSV" button in the
    # table
    mutate(bad_rows = ifelse(
      table_name == "missing_dates" & type == "error",
      list(
        daily |>
          tsibble::as_tsibble(key = c(meta_station_id, meta_station_name), index = datetime) |>
          tsibble::count_gaps(.full = end(), .name = c("gap_start", "gap_end", "n_missing"))
      ),
      bad_rows
    ))
}
