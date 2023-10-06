# library(TrenchR)

# Wrapper to sum direct, diffuse, and reflected solar radiation output by `solar_radiation()`
solar_radiation_total <- function(doy, psi, tau, elev, rho) {
  purrr::pmap(list(doy, psi, tau, elev, rho), solar_radiation) |> 
    purrr::map_dbl(sum) 
}

#TODO: make this work for daily data too.
# - conditionally use different date column name
# - expand_grid differently depending on hourly vs daily
# - sum values differently depending on hourly vs daily

calc_sol_rad_theoretical <- function(data, freq = c("hourly", "daily")) {
  # NOTE: this calculation of midpoint is NOT ROBUST. Decimal values will get
  # coerced to integers and then be wrong. Edit with caution!
  n_segments <- 10L
  start <- 0 + (1/2 * 1/n_segments)
  midpts <- seq(start, by = 1/n_segments, length.out = n_segments) * 60
  
  left_join(data, station_info, by = join_by(meta_station_id, meta_station_name)) |> 
    select(meta_station_id, meta_station_name, date_datetime, latitude, longitude, elev_m, sol_rad_total) |> 
    expand_grid(
      minute = midpts
    ) |>
    mutate(grid_datetime = make_datetime(
      year = year(date_datetime),
      month = month(date_datetime),
      day = day(date_datetime),
      hour = hour(date_datetime),
      min = as.integer(minute)
    )) |> 
    #Zenith angle
    mutate(psi = zenith_angle(
      doy = yday(date_datetime),
      lat = latitude,
      lon = longitude,
      hour = as_hms(grid_datetime) |> as.duration() |> as.numeric("hours"),
      # hour = hour(date_datetime) + 0.5, #estimate for middle of hour
      offset = -7
    )) |> 
    #Total solar radiation
    mutate(
      sol_rad_est = solar_radiation_total(
        doy = yday(date_datetime),
        psi = psi * pi / 180,
        tau = 0.7, #atmospheric transmissivity, 0.7 is probably reasonable, but could also leave at 1 for estimating max theoretical solar radiation
        elev = elev_m,
        rho = 0.5 #typical albedo values: desert sand, 0.4; concrete, 0.55; bare soil, 0.17; asphalt, 0.04
      ) # in W/m^2
    ) |> 
    ## Convert instantaneous solar radiation to hourly total by summing over the
    ## hour, multiplying by bin width (1/n()) and then re-joining to the
    ## original dataset
    summarize(
      sol_rad_est = sum(sol_rad_est * (1/n()), na.rm = TRUE),
      .by = c(meta_station_id, meta_station_name, date_datetime)
    ) |>
    mutate(
      #convert to same units as azmet data: 1 W*hr/m^2 = 0.0036 MJ/m^2,
      sol_rad_est = sol_rad_est * 0.0036
    ) |> 
    # Sensor totals are for the previous hour, so lag estimates to match
    mutate(sol_rad_est = lag(sol_rad_est), .by = meta_station_id) |> 
    right_join(data, by = join_by(meta_station_id, meta_station_name, date_datetime))
}
# hourly <- az_hourly(start = "2023-05-29 00", end = "2023-6-05 23")
# hourly_calced <- calc_sol_rad_theoretical(hourly)
# nrow(hourly) == nrow(hourly_calced)
# 
# hourly_calced |>
#   filter(!lte(sol_rad_total, sol_rad_est, tol = 0.5)) |>
#   select(date_datetime, sol_rad_est, sol_rad_total)
# 
# hourly_calced |> 
#   filter(meta_station_id %in% c("az01", "az40")) |> 
# ggplot(aes(x = date_datetime)) +
#   geom_line(aes(y = sol_rad_total, color = "Observed")) +
#   geom_line(aes(y = sol_rad_est, color = "Theoretical max")) +
#   facet_wrap(~meta_station_id)
