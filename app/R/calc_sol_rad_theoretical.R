# library(TrenchR)

# Wrapper to sum direct, diffuse, and reflected solar radiation output by `solar_radiation()`
solar_radiation_total <- function(doy, psi, tau, elev, rho) {
  purrr::pmap(list(doy, psi, tau, elev, rho), solar_radiation) |> 
    purrr::map_dbl(sum) 
}



#' Calculate theoretical maximum solar radiation
#'
#' @param data data returned by either `az_daily()` or `az_hourly()`
#' @param freq indicator of whether `data` is hourly or daily (default is hourly)
#'
#' @return `data`, but with a `sol_rad_est` column indicating the estimated
#'   theoretical maximum solar radiation for that date or datetime
#'   
calc_sol_rad_theoretical <- function(data, freq = c("hourly", "daily")) {
  # hourly and daily data have slightly different formats, so must deal with that
  freq <- match.arg(freq)
  if (freq == "hourly") {
    date_col <- sym("date_datetime")
  } else {
    date_col <- sym("datetime")
  }
  
  # Join location data ------------------------------------------------------
  data_joined <- 
    left_join(data, station_info, by = join_by(meta_station_id, meta_station_name)) |> 
    select(meta_station_id, meta_station_name, {{ date_col }}, latitude, longitude, elev_m, sol_rad_total)
  
  
  # Expand grid -------------------------------------------------------------
  # For hourly data, to get better resolution calculate instantaneous solar
  # radiation every few minutes and then sum. For daily data, calculate hourly
  # and sum.
  
  if (freq == "hourly") {
    
    # NOTE: this calculation of midpoint is NOT ROBUST. Decimal values will get
    # coerced to integers and then be wrong. Edit with caution!
    n_segments <- 10L
    start <- 0 + (1/2 * 1/n_segments)
    midpts <- seq(start, by = 1/n_segments, length.out = n_segments) * 60
    
    data_expand <-
      data_joined |> 
      expand_grid(
        minute = midpts
      ) |>
      mutate(grid_datetime = make_datetime(
        year = year(date_datetime),
        month = month(date_datetime),
        day = day(date_datetime),
        hour = hour(date_datetime),
        min = as.integer(minute)
      ))
  } else {
    data_expand <- 
      data_joined |> 
      expand_grid(
        hour = 0:23
      ) |> 
      mutate(grid_datetime = make_datetime(
        year = year(datetime),
        month = month(datetime),
        day = day(datetime),
        hour = hour,
        min = 30 #estimate at midpoint of hour
      ))
  }
  
  
  # Calculate instantaneous solar radiation ---------------------------------
  data_sol <- 
    data_expand |> 
    #Zenith angle
    mutate(psi = zenith_angle(
      doy = yday({{ date_col }}),
      lat = latitude,
      lon = longitude,
      hour = as_hms(grid_datetime) |> as.duration() |> as.numeric("hours"),
      offset = -7
    )) |> 
    #Total solar radiation
    mutate(
      sol_rad_est = solar_radiation_total(
        doy = yday({{ date_col }}),
        psi = psi * pi / 180,
        tau = 0.7, #atmospheric transmissivity, 0.7 is probably a reasonable maximum
        elev = elev_m,
        rho = 0.4 #typical albedo values: desert sand, 0.4; concrete, 0.55; bare soil, 0.17; asphalt, 0.04
      ) # in W/m^2
    ) 
  
  
  # Integrate instantaneous to total ----------------------------------------
  # For hourly, need to multiply by 1/n() (∆x).  For daily, just sum hourly for each date
  
  if (freq == "hourly") {
    data_summed <- 
      data_sol |> 
      summarize(
        sol_rad_est = sum(sol_rad_est * (1/n()), na.rm = TRUE),
        .by = c(meta_station_id, meta_station_name, date_datetime)
      ) |> 
      # Sensor totals are for the previous hour, so lag estimates to match
      mutate(sol_rad_est = lag(sol_rad_est), .by = meta_station_id)
  } else {
    data_summed <-
      data_sol |> 
      summarize(
        sol_rad_est = sum(sol_rad_est, na.rm = TRUE),
        .by = c(meta_station_id, meta_station_name, datetime)
      )
  }
  
  data_summed |> 
    mutate(
      #convert to same units as azmet data: 1 W*hr/m^2 = 0.0036 MJ/m^2,
      sol_rad_est = sol_rad_est * 0.0036
    ) |> 
    right_join(data, by = join_by(meta_station_id, meta_station_name, {{ date_col }}))
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
# 
# daily <- az_daily(start = "2023-05-29", end = "2023-06-05")
# daily_calced <- calc_sol_rad_theoretical(daily, "daily")
# daily_calced |> 
#   filter(!lte(sol_rad_total, sol_rad_est)) |>
#   select(datetime, sol_rad_est, sol_rad_total)
# 
# daily_calced |>
#   filter(meta_station_id %in% c("az01", "az40")) |>
#   ggplot(aes(x = datetime)) +
#   geom_line(aes(y = sol_rad_total, color = "Observed")) +
#   geom_line(aes(y = sol_rad_est, color = "Theoretical max")) +
#   facet_wrap(~meta_station_id)
