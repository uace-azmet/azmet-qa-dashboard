library(azmetr)
library(data.validator)
daily <- az_daily()
check_daily <- function(daily, start, end) {
  daily_check <-
    daily |>
    filter(datetime > start & datetime <= end) |>
    create_agent(
      # tbl_name = "Daily Data Consistency Checks",
      # label = "Consistency Checks",
      actions = al
    ) |>
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    col_vals_gte(temp_air_meanC, vars(dwpt_mean), na_pass = TRUE) |>
    col_vals_lte(temp_air_minC, vars(temp_air_meanC), na_pass = TRUE) |>
    col_vals_lte(temp_air_meanC, vars(temp_air_maxC), na_pass = TRUE) |>
    col_vals_lte(wind_spd_mean_mps, vars(wind_spd_max_mps), na_pass = TRUE) |>
    col_vals_lte(temp_soil_10cm_meanC, vars(temp_soil_10cm_maxC), na_pass = TRUE) |>
    col_vals_lte(temp_soil_10cm_minC, vars(temp_soil_10cm_meanC), na_pass = TRUE) |>
    col_vals_lte(temp_soil_50cm_meanC, vars(temp_soil_50cm_maxC), na_pass = TRUE) |>
    col_vals_lte(temp_soil_50cm_minC, vars(temp_soil_50cm_meanC), na_pass = TRUE) |>
    col_vals_lte(relative_humidity_mean,
                 vars(relative_humidity_max),
                 na_pass = TRUE) |>
    col_vals_lte(relative_humidity_min,
                 vars(relative_humidity_mean),
                 na_pass = TRUE) |>
    
    #TODO calculate max sol radiation based on date and location and check for that
    # col_vals_lt(sol_rad_total, sol_rad_expected, preconditions = ~calc_sol(date))
    interrogate()
  get_agent_report(daily_check, title = "Daily Consistency Checks")
  
}