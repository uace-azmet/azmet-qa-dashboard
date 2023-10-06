library(azmetr)
library(TrenchR)
daily <- az_daily(start = "2023-01-01", end = "2023-10-01")

# NOTE: this is almost exaclty the same as the code in calc_sol_rad_theoretical.  Would be best to just generalize it to work with hourly or daily data.

daily2 <- 
  daily |> 
  left_join(station_info, by = join_by(meta_station_id, meta_station_name)) |> 
  select(meta_station_id, meta_station_name, datetime, latitude, longitude, elev_m, sol_rad_total) |> 
  expand_grid(
    hour = 0:23
  ) |> 
  mutate(grid_datetime = make_datetime(
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour
  ))|> 
  #Zenith angle
  mutate(psi = zenith_angle(
    doy = yday(datetime),
    lat = latitude,
    lon = longitude,
    hour = hour(grid_datetime) + 0.5, #estimate for middle of hour
    offset = -7
  )) |> 
  #Total solar radiation
  mutate(
    sol_rad_est = solar_radiation_total(
      doy = yday(datetime),
      psi = psi * pi / 180,
      tau = 0.7, #atmospheric transmissivity, leave at 1 for estimating max theoretical solar radiation
      elev = elev_m,
      rho = 0.5 #typical albedo values: desert sand, 0.4; concrete, 0.55; bare soil, 0.17; asphalt, 0.04
    ) # in W/m^2
  ) 

View(daily2)

daily_calced <- daily2 |> 
  ## Convert instantaneous solar radiation to hourly total by summing over the
  ## hour, multiplying by bin width (1/n()) and then re-joining to the
  ## original dataset
  summarize(
    sol_rad_est = sum(sol_rad_est, na.rm = TRUE),
    .by = c(meta_station_id, meta_station_name, datetime)
  ) |>
  mutate(
    #convert to same units as azmet data: 1 W*hr/m^2 = 0.0036 MJ/m^2,
    sol_rad_est = sol_rad_est * 0.0036
  ) |> 
  # Sensor totals are for the previous hour, so lag estimates to match
  mutate(sol_rad_est = lag(sol_rad_est), .by = meta_station_id) |> 
  right_join(daily, by = join_by(meta_station_id, meta_station_name, datetime))

daily_calced |> 
filter(!lte(sol_rad_total, sol_rad_est, tol = 0.5)) |>
    select(datetime, sol_rad_est, sol_rad_total)

daily_calced |>
  # filter(meta_station_id %in% c("az01", "az40")) |>
ggplot(aes(x = datetime)) +
  geom_line(aes(y = sol_rad_total, color = "Observed")) +
  geom_line(aes(y = sol_rad_est, color = "Theoretical max")) +
  facet_wrap(~meta_station_id)
