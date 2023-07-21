#only use measured variables

cols_daily_temp <- 
  c("temp_air_maxC", "temp_air_meanC", "temp_air_minC", "temp_soil_10cm_maxC", 
    "temp_soil_10cm_meanC", "temp_soil_10cm_minC", "temp_soil_50cm_maxC", 
    "temp_soil_50cm_meanC", "temp_soil_50cm_minC")


cols_daily_precip <- 
  c("precip_total_mm",
    "relative_humidity_max",
    "relative_humidity_mean",
    "relative_humidity_min")

cols_daily_wind_sun <- 
  c("sol_rad_total", 
    "wind_spd_max_mps",
    "wind_spd_mean_mps",
    "wind_2min_spd_max_mps",
    "wind_2min_spd_mean_mps",
    "wind_vector_dir",
    "wind_vector_dir_stand_dev",
    "wind_vector_magnitude")


cols_hourly_temp <- c("temp_airC",  "temp_soil_10cmC", "temp_soil_50cmC")

cols_hourly_precip <- c("precip_total", "relative_humidity")

cols_hourly_wind_sun <- c("sol_rad_total", "wind_2min_spd_mean_mps", "wind_2min_spd_max_mps", "wind_2min_vector_dir", "wind_spd_max_mps", "wind_spd_mps", "wind_vector_dir", "wind_vector_dir_stand_dev", "wind_vector_magnitude")
