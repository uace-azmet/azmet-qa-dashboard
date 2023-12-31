# Vectors of column names to be used for selecting which variables to plot
# Only includes measured variables, not calculated variables (except dwpt)
cols_daily_temp <- 
  c("temp_air_maxC", "temp_air_meanC", "temp_air_minC", "temp_soil_10cm_maxC", 
    "temp_soil_10cm_meanC", "temp_soil_10cm_minC", "temp_soil_50cm_maxC", 
    "temp_soil_50cm_meanC", "temp_soil_50cm_minC")


cols_daily_precip <- 
  c("sol_rad_total", #included here because precip seems to be biggest influence on "unusual" sol_rad data
    "precip_total_mm",
    "relative_humidity_max",
    "relative_humidity_mean",
    "relative_humidity_min",
    "dwpt_mean") #dwpt is calculated, but including because it's used in validation table

cols_daily_wind <- 
  c("wind_spd_max_mps",
    "wind_spd_mean_mps",
    "wind_2min_spd_max_mps",
    "wind_2min_spd_mean_mps",
    "wind_vector_dir",
    "wind_vector_dir_stand_dev",
    "wind_vector_magnitude")


cols_hourly_temp <- c("temp_airC",  "temp_soil_10cmC", "temp_soil_50cmC")

cols_hourly_precip <- c("sol_rad_total", "precip_total", "relative_humidity", "dwpt")

cols_hourly_wind <- c("wind_2min_spd_mean_mps", "wind_2min_spd_max_mps", "wind_2min_vector_dir", "wind_spd_max_mps", "wind_spd_mps", "wind_vector_dir", "wind_vector_dir_stand_dev", "wind_vector_magnitude")
