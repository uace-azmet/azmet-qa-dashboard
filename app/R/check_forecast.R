# board <- board_connect()
# fc_daily <- board |> pin_read("ericrscott/fc_daily") |> arrange(varname) |> filter(datetime > "2023-07-14" & datetime < "2023-07-19")

#' Create timeseries forecast-based validation report for daily data
#'
#' Checks that observed values are within 99% predictive intervals of a
#' timeseries model
#'
#' @param fc_daily the pinned dataset here:
#'   https://viz.datascience.arizona.edu/content/2bd43bd6-ff6c-4362-8212-288820e5d412
#'
#' @return a tibble with list columns
#' 
#' @examples
#' library(pins)
#' library(arrow)
#' board <- board_connect()
#' fc_daily <- board |> pin_read("ericrscott/fc_daily")
#' check_forecast(fc_daily)
#' 
check_forecast <- function(fc_daily) {
  df_list <- fc_daily |> 
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
  # adjust error_df to have the correct index corresponding to the original
  # data, not split data
  n <- map_dbl(df_list, nrow) |> cumsum() |> lag()
  n[1] <- 0
  res <- 
    data.validator::get_results(report) |>
    dplyr::mutate(error_df = map2(error_df, n, \(.error_df, .n) {
      if(!is.null(.error_df)) {
        .error_df |> dplyr::mutate(index = index + .n)
      }
    })) |> 
    # make `bad_rows` list-column with slices of data where there are problems.
    # This will create the data set downloaded by the "CSV" button
    mutate(bad_rows = map(error_df, \(.x){
      if(length(.x$index) > 0) {
        fc_daily |> 
          dplyr::slice(.x$index)
      } else {
        NA
      }
    }))
  
  #return
  res
}
