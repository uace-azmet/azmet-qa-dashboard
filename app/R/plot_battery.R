# library(azmetr)
# library(tidyverse)
# library(plotly)
# library(colorspace)

# daily_all <- az_daily(start_date = "2020-12-30")
# daily_historic <-
#   daily_all |> 
#   select(meta_station_id, meta_station_name, datetime, starts_with("meta_bat_volt"), temp_air_minC, temp_air_maxC, sol_rad_total)
# write_csv(daily_historic, "app/daily_historic.csv")


# from user input
# daily_check <- az_daily(start_date = "2023-08-01", end_date = "2023-08-15")


#pure plotly for speed

#TODO add toggle for min/max error bars?
#TODO make new points stand out even more (e.g. change shape)
plot_voltage <- function(check_data, xvar = c("temp_air_minC", "temp_air_maxC", "sol_rad_total")) {
  daily_historic <- read_csv("daily_historic.csv")
  xvar <- match.arg(xvar)
  xlab <- switch(
    xvar,
    "temp_air_minC" = "Minimum Air Temp [ºC]",
    "temp_air_maxC" = "Maximum Air Temp [ºC]",
    "sol_rad_total" = "Solar Radiation Total [MJ m<sup>-2</sup>]"
  )
  plot_ly(
    daily_historic,
    y = ~meta_bat_volt_mean,
    x = ~.data[[{{xvar}}]],
    colors = colorspace::qualitative_hcl(n = 30)
  ) |> 
    add_markers(
      color = I("black"),
      # alpha = 0.2,
      opacity = 0.2, #different from alpha, sets opacity of entire layer
      hoverinfo = 'none' #disable for this layer only
    ) |>
    add_markers(
      data = daily_check,
      color = ~ meta_station_id,
      ## adds error bars for min and max voltage, but doesn't look great
      # error_y = ~list(
      #   symmetric = FALSE, array = meta_bat_volt_max, arrayminus = meta_bat_volt_min
      # ),
      text = ~datetime,
      hovertemplate = "%{text}",
      showlegend = FALSE
    ) |> 
    layout(
      xaxis = list(title = xlab),
      yaxis = list(title = "Mean Voltage")
    )
  
}
# plot_voltage(daily_check)

