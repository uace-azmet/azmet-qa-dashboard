#' Create plots for hourly data
#'
#' @param hourly tibble returned by `az_hourly()`
#' @param cols character vector of column names (or tidyselect helper)
#' @param station character; station name
#'
#' @return a patchwork object
#'
#' @examples
#' hourly <-  az_hourly(start = "2023-07-01 00", end = "2023-07-18 00")
#' plot_hourly(hourly, cols = starts_with("temp_air"))
#' 
plot_hourly <- function(hourly, cols, station = "Tucson") {
  hourly_sub <- 
    hourly |> 
    az_add_units() |> 
    filter(meta_station_name == station) |> 
    select(date_datetime, any_of(cols))
  
  plot_cols <- hourly_sub |> select(-date_datetime) |> colnames()
  plot_list <- 
    map(plot_cols, \(colname) {
      ggplot(hourly_sub, aes(x = date_datetime, y = .data[[colname]])) +
        geom_line() +
        labs(title = colname, y = "") +
        theme_bw() +
        theme(
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          # axis.text.y = element_blank(),
          # axis.ticks.y = element_blank(),
          plot.title = element_text()
        )
    })
  wrap_plots(plot_list, ncol = 3)
}


