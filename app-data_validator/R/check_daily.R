library(azmetr)
# library(data.validator)
# daily <- az_daily(start = "2023-07-01", end = "2023-07-12")

check_daily2 <- function(daily) {
report <- data_validation_report()
  
  validate(daily, name = "Daily Data") |>
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    validate_if(temp_air_meanC >= dwpt_mean, "Mean air temp ≥ dewpoint") |> 
    validate_if(temp_air_minC <= temp_air_meanC, "min air temp ≤ mean air temp") |> 
    validate_if(temp_air_meanC <= temp_air_maxC, "mean air temp ≤ max air temp") |> 
    validate_if(wind_spd_mean_mps <= wind_spd_max_mps, "mean wind speed ≤ max wind speed") |> 
    validate_if(temp_soil_10cm_meanC <= temp_soil_10cm_maxC, "mean soil temp at 10cm ≤ max soil temp at 10cm") |> 
    validate_if(temp_soil_10cm_minC <=temp_soil_10cm_meanC, "min soil temp at 10cm ≤ mean soil temp at 10cm") |>
    # validate_if(temp_soil_50cm_meanC<=temp_soil_50cm_maxC , "mean soil temp at 50cm ≤ max soil temp at 50cm") |>
    # validate_if(temp_soil_50cm_minC <=temp_soil_50cm_meanC, "min soil temp at 50cm ≤ mean soil temp at 50cm") |>
    # validate_if(relative_humidity_mean <= relative_humidity_max, "mean RH ≤ max RH") |> 
    validate_rows(\(x) between(x$relative_humidity_mean, x$relative_humidity_min, x$relative_humidity_max), \(x) isTRUE(ifelse(is.na(x), TRUE, x)), everything(), description = "min ≤ mean ≤ max for RH") |> 
    validate_rows(\(x) between(x$temp_soil_50cm_meanC, x$temp_soil_50cm_minC, x$temp_soil_50cm_maxC), \(x) isTRUE(ifelse(is.na(x), TRUE, x)), everything(), description = "min ≤ mean ≤ max for soil temp at 50cm") |> 
    add_results(report)
  
  

  #   col_vals_gte(temp_air_meanC, vars(dwpt_mean), na_pass = TRUE) |>
  #   col_vals_lte(temp_air_minC, vars(temp_air_meanC), na_pass = TRUE) |>
  #   col_vals_lte(temp_air_meanC, vars(temp_air_maxC), na_pass = TRUE) |>
  #   col_vals_lte(wind_spd_mean_mps, vars(wind_spd_max_mps), na_pass = TRUE) |>
  #   col_vals_lte(temp_soil_10cm_meanC, vars(temp_soil_10cm_maxC), na_pass = TRUE) |>
  #   col_vals_lte(temp_soil_10cm_minC, vars(temp_soil_10cm_meanC), na_pass = TRUE) |>
  #   col_vals_lte(temp_soil_50cm_meanC, vars(temp_soil_50cm_maxC), na_pass = TRUE) |>
  #   col_vals_lte(temp_soil_50cm_minC, vars(temp_soil_50cm_meanC), na_pass = TRUE) |>
  #   col_vals_lte(relative_humidity_mean,
  #                vars(relative_humidity_max),
  #                na_pass = TRUE) |>
  #   col_vals_lte(relative_humidity_min,
  #                vars(relative_humidity_mean),
  #                na_pass = TRUE)
  
}


