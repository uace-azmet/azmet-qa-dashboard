library(pins)
library(arrow)
library(tidyverse)
library(data.validator)
board <- board_connect()
fc_daily <- board |> pin_read("ericrscott/fc_daily")

check_forecast <- function(fc_daily, start, end) {
  df_list <- fc_daily |> 
    filter(datetime > start & datetime <= end) |> 
    group_by(varname) |> 
    group_split()
  names(df_list) <- map_chr(df_list, ~.x |> pull(varname) |> unique()) 
  #initialize report
  report <- data.validator::data_validation_report()
  #loop to add to report for each variable
  for(i in seq_along(df_list)) {
    data.validator::validate(df_list[[i]]) |> 
      validate_if(btwn(obs, lower_99, upper_99, na_pass = TRUE),
                  description = names(df_list)[i]) |> 
      add_results(report)
  }
  #return
  report
}

