library(TrenchR)

# Wrapper to sum direct, diffuse, and reflected solar radiation output by `solar_radiation()`
solar_radiation_total <- function(doy, psi, tau, elev, rho) {
  purrr::pmap(list(doy, psi, tau, elev, rho), solar_radiation) |> 
    purrr::map_dbl(sum) 
}
#TODO speed this up by getting rid of `units` (i.e. need to figure out the unit conversions "by hand")

calc_sol_rad_theoretical <- function(hourly) {
  # NOTE: this calculation of midpoint is NOT ROBUST. Decimal values will get
  # coerced to integers and then be wrong. Edit with caution!
  n_segments <- 10L
  start <- 0 + (1/2 * 1/n_segments)
  midpts <- seq(start, by = 1/n_segments, length.out = n_segments) * 60
  
  left_join(hourly, station_info, by = join_by(meta_station_id, meta_station_name)) |> 
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
        tau = 1, #atmospheric transmissivity, leave at 1 for estimating max theoretical solar radiation
        elev = elev_m,
        rho = 0.4 #typical albedo values: desert sand, 0.4; concrete, 0.55; bare soil, 0.17; asphalt, 0.04
      ) |> set_units("W m-2"),
      sol_rad_total = set_units(sol_rad_total, "MJ/m^2")
    ) |> 
    ## Convert instantaneous solar radiation to hourly total by summing over the
    ## hour, multiplying by bin width (1/n()) and then re-joining to the
    ## original dataset
    summarize(
      sol_rad_est = sum(signif(sol_rad_est, 3) * set_units(1/n(), "hr"), na.rm = TRUE) |>
        #convert to same units as data
        set_units("MJ m-2"),
      .by = c(meta_station_id, meta_station_name, date_datetime)
    ) |>
    # Sensor totals are for the previous hour, so lag estimates to match
    mutate(sol_rad_est = lag(sol_rad_est)) |> 
    mutate(sol_rad_est = signif(sol_rad_est, 1)) |>
    right_join(hourly, by = join_by(meta_station_id, meta_station_name, date_datetime))
}
# hourly <- az_hourly()
# hourly_calced <- calc_sol_rad_max(hourly) 
# 
# nrow(hourly) == nrow(hourly_calced)
