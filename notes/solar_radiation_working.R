library(TrenchR)
# install.packages('azmetr', repos = c('https://aariq.r-universe.dev', 'https://cloud.r-project.org'))
library(azmetr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(units)
library(hms)


# Functions ---------------------------------------------------------------

# Wrapper to sum direct, diffuse, and reflected solar radiation output by `solar_radiation()`
solar_radiation_total <- function(doy, psi, tau, elev, rho) {
  purrr::pmap(list(doy, psi, tau, elev, rho), solar_radiation) |> 
    purrr::map_dbl(sum) 
}

# Retrieve data -----------------------------------------------------------

hourly_raw <- 
  az_hourly(start_date_time = "2022-6-01 00", end_date_time = "2022-6-02 00") |> 
  left_join(station_info) |> 
  select(meta_station_id, meta_station_name, date_datetime, latitude, longitude, elev_m, sol_rad_total) |> 
  az_add_units()

# midnight is coded as 23:59:59 which is the same hour as 23:00:00, but it doesn't matter since it's dark at midnight, so I'll just ignore this.  0+0=0!



# Add more points ---------------------------------------------------------

# Make estimate higher "resolution" by adding more timepoints to get
# instantaneous estimates of solar radiation and then later summing "area under
# the curve"

# NOTE: this calculation of midpoint is NOT ROBUST. Decimal values will get
# coerced to integers and then be wrong. Edit with caution!
n_segments <- 10L
start <- 0 + (1/2 * 1/n_segments)
midpts <- seq(start, by = 1/n_segments, length.out = n_segments) * 60

hourly <-
  expand_grid(
    hourly_raw,
    minute = midpts
  ) |>
  mutate(grid_datetime = make_datetime(
    year = year(date_datetime),
    month = month(date_datetime),
    day = day(date_datetime),
    hour = hour(date_datetime),
    min = as.integer(minute)
  ))

# Calculate solar radiation -----------------------------------------------

hourly <-
  hourly |> 
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
  mutate(sol_rad_est = solar_radiation_total(
    doy = yday(date_datetime),
    psi = psi * pi / 180,
    tau = 1, #atmospheric transmissivity, leave at 1 for estimating max theoretical solar radiation
    elev = elev_m,
    rho = 0.4 #typical albedo values: desert sand, 0.4; concrete, 0.55; bare soil, 0.17; asphalt, 0.04
  ) |> set_units("W m-2")) |> 
  ## Convert instantaneous solar radiation to hourly total.
  ## If there's only one estimate per hour, this works
  # mutate(sol_rad_est = sol_rad_est * set_units(1, "hr")) |>
  ## But this works in all situations by summing over the hour, multiplying by
  ## bin width (1/n()) and then re-joining to the original dataset
  summarize(
    sol_rad_est = sum(signif(sol_rad_est, 2) * set_units(1/n(), "hr")) |>
      #convert to same units as data
      set_units("MJ m-2"),
    .by = c(meta_station_id, meta_station_name, date_datetime)
  ) |>
  # Sensor totals are for the previous hour, so lag estimates to match
  mutate(sol_rad_est = lag(sol_rad_est)) |> 
  mutate(sol_rad_est = signif(sol_rad_est, 1)) |>
  right_join(hourly)

hourly |> 
  filter(meta_station_id == "az04") |> 
  ggplot(aes(x = date_datetime)) +
  geom_line(aes(y = sol_rad_total, color = "Observed")) +
  geom_line(aes(y = sol_rad_est, color = "TrenchR")) +
  facet_wrap(~meta_station_id)

hourly |>
  filter(gte(sol_rad_total, sol_rad_est))

hourly |> 
  filter(!lte(sol_rad_total, sol_rad_est, tol = 0.01))


# Scratch paper -----------------------------------------------------------

#validate with some tolerance
lte <- function(x,y, na_pass = FALSE, tol = sqrt(.Machine$double.eps)) {
  eq <- purrr::map2_lgl(x, y, \(x,y) isTRUE(all.equal(x,y, tolerance = tol)))
  res <- x < y | eq
  if(isTRUE(na_pass)) {
    res <- ifelse(is.na(res), TRUE, res)
  }
  res
}

lte(y, x, na_pass = TRUE)

hourly[!lte(hourly$sol_rad_total, hourly$sol_rad_est, na_pass = TRUE, tol = 0.0001), ]


all.equal(set_units(0.000446, "MJ/m^2"), set_units(4e-16, "MJ/m^2"), tolerance = 0.001)
all.equal(set_units(0.000446, "MJ/m^2"), set_units(NA, "MJ/m^2"), tolerance = 0.001, check.attributes = FALSE)

