library(tidyverse)
library(pins)
library(arrow)
library(units)
library(patchwork)

board <- board_connect()
fc_daily <- board |>
  pin_read("ericrscott/fc_daily") |>
  arrange(varname) |>
  filter(datetime > "2023-06-01" & datetime < "2023-07-19") 

plot_fc <- function(fc_daily, cols, station = "Tucson") {
  
  fc_filtered <- 
    fc_daily |> 
    filter(meta_station_name == station) |> 
    filter(varname %in% cols)
  #get actual vars from filtered dataset. 
  vars <- unique(fc_filtered$varname)
  
  # Could use faceting, but I'll make separate plots to match the other tabs
  plot_list <- 
    map(vars, \(var) {
      fc_plot <- 
        fc_filtered |> 
        filter(varname == var) |> 
        mutate(within_99 = between(obs, lower_99, upper_99) |> factor(levels = c(TRUE, FALSE)))
      
      #figure out units for this variable and set units on tibble columns
      unit_vec <- tibble(!!var := 0) |> azmetr::az_add_units() |> pull()
      units(fc_plot$obs) <- units(fc_plot$fc_mean) <-
        units(fc_plot$lower_99) <- units(fc_plot$upper_99) <-
        units(unit_vec)
      
      fc_plot |> 
        ggplot(aes(x = datetime)) +
        geom_ribbon(aes(ymin = lower_99, ymax = upper_99), alpha = 0.3) +
        geom_line(aes(y = fc_mean)) +
        geom_point(aes(y = obs, color = within_99), size = 1.5, shape = 21) +
        scale_color_manual(
          "Observed within 99% interval?",
          values = c("TRUE" = "black", "FALSE" = "red"),
          labels = c("TRUE" = "Yes", "FALSE" = "No"),
          drop = FALSE #don't drop unused levels
        )+
        labs(title = var, y = "") +
        theme_bw() +
        theme(
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text()
        )
    })  
  
  wrap_plots(plot_list, ncol = 3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
}

# source("app/R/plot_vars.R")
# plot_fc(fc_daily, cols = cols_daily_temp)
