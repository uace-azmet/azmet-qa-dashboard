#' Create plots for daily data
#'
#' @param daily tibble returned by `az_daily()`
#' @param cols character vector of column names (or tidyselect helper)
#' @param station character; station name
#'
#' @return a patchwork object
#'
#' @examples
#' daily <-  az_daily(start = "2023-07-01", end = "2023-07-18")
#' plot_daily(daily, cols = starts_with("temp_air"))
#' 
plot_daily <- function(daily, cols, station = "Tucson") {
  daily_sub <- 
    daily |> 
    az_add_units() |> 
    filter(meta_station_name == station) |> 
    select(datetime, any_of(cols))
  
  plot_cols <- daily_sub |> select(-datetime) |> colnames()
  plot_list <- 
    map(plot_cols, \(colname) {
      ggplot(daily_sub, aes(x = datetime, y = .data[[colname]])) +
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


