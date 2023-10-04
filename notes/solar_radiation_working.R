library(TrenchR)
# install.packages('azmetr', repos = c('https://aariq.r-universe.dev', 'https://cloud.r-project.org'))
library(azmetr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(units)

# Retrieve data -----------------------------------------------------------

hourly <- 
  az_hourly(station_id = "az01", start_date_time = "2023-04-01 00", end_date_time = "2023-04-02 00") |> 
  left_join(station_info) |> 
  select(date_datetime, latitude, longitude, elev_m, sol_rad_total) |> 
  az_add_units()

hourly <-
  hourly |> 
  mutate(psi = zenith_angle(
    doy = yday(date_datetime),
    lat = latitude,
    lon = longitude,
    hour = hour(date_datetime) + 0.5,
    offset = -7
  ))

# function to sum direct, diffuse, and reflected solar radiation output by `solar_radiation()`
solar_radiation_total <- function(doy, psi, tau, elev, rho) {
  purrr::pmap(list(doy, psi, tau, elev, rho), solar_radiation) |> 
    # [1] = direct, [2] = diffuse, [3] = reflected
    purrr::map_dbl(\(x) sum(x[c(1, 2, 3)])) 
}

hourly <- hourly |> 
  mutate(sol_rad_est = solar_radiation_total(
    doy = yday(date_datetime),
    psi = psi * pi / 180,
    tau = 1,
    elev = elev_m,
    rho = 0.7
  ) |> set_units("W m-2"))


hourly <- hourly |> 
  # Convert instantaneous solar radiation to hourly total. This assumes solar
  # radiation was the same for the entire hour, but I've also tested versions of
  # this workflow that get instantaneous solar radiation every 2 min to better
  # estimate hourly totals
  mutate(sol_rad_est = sol_rad_est * set_units(1, "hr")) |> 
  # sensor totals are for the previous hour, so lag them to match
  mutate(sol_rad_est = lag(sol_rad_est)) 

hourly |> 
  ggplot(aes(x = date_datetime)) +
  geom_line(aes(y = sol_rad_total, color = "Observed")) +
  geom_line(aes(y = sol_rad_est, color = "TrenchR"))

hourly |>
  filter(sol_rad_total > sol_rad_est)
