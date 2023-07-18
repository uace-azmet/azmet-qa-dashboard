library(gt)
#' Convert `data.validator` report into custom `gt` table
#'
#' @param report a `data.validator` report object
#' @param data original data frame used to generate the report
#'
#' @return a `gt` table
format_report_gt <- function(report, data) {
  #TODO make these arguments so they can be adjusted in the dashboard maybe?
  warn_at <- 1 #warn if ≥ 1 row fails a validation
  error_at <- 0.05 #error if ≥ 5% of rows fail validation
  
  data.validator::get_results(report) |> 
    #for some reason, report has duplicated rows. Seems like a bug in data.validator
    slice_head(n = 1, by = assertion.id) |> 
    mutate(n_failed = if_else(is.na(num.violations), 0, num.violations),
           p_failed = n_failed / nrow(data)) |> 
    mutate(mark = case_when(
      p_failed >= error_at ~ "🛑",
      n_failed >= warn_at ~ "⚠️️",
      .default = "✅"
    )) |> 
    mutate(bad_rows = map(error_df, ~dl_btn(.x, data))) |> 
    select(assertion.id, mark, validation = description, n_failed, p_failed, bad_rows) |> 
    mutate(bad_rows = ifelse(
      is.na(bad_rows), #if no bad rows
      "&mdash;", #print an emdash
      make_csv_button(assertion.id, bad_rows) #else make tibble into a download button
    )) |> 
    gt() |> 
    cols_hide(assertion.id) |> 
    cols_label(
      validation = "Validation",
      n_failed = "Failed Rows",
      p_failed = "% Failed",
      bad_rows = "Download",
      mark = ""
    ) |>
    tab_style(
      cell_fill(color = "#FFB800B5"),
      locations = cells_body(rows = n_failed >= warn_at)
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = "#FF1212B2"),
        cell_text(color = "white")
      ),
      locations = cells_body(rows = p_failed >= error_at)
    ) |> 
    cols_align(
      align = "center",
      columns = c(mark, bad_rows)
    ) |> 
    fmt_percent(p_failed) |> 
    fmt_markdown(columns = c(validation, bad_rows))
}

#' helper function to turn the error_df column of a data.validator report into a
#' list column of tibbles. The `index` column contains the row numbers of the
#' original data that did not pass validations.
dl_btn <- function(error_df, data) {
  if(length(error_df$index) > 0) {
    data |> 
      dplyr::slice(error_df$index)
  } else {
    NA
  }
}


#helper to reformat list column of tibbles to be html for a button that downloads the tibble as a .csv
#adapted from pointblank package: https://github.com/rich-iannone/pointblank/blob/645698bc3ac1080f97ee14081d6cfe61a2c1d70f/R/get_agent_report.R#L1365

make_csv_button <- function(assertion.id, bad_df) {
  
  map2_chr(assertion.id, bad_df, \(assertion.id, bad_df) {
    
    temp_file <- tempfile(pattern = paste0("csv_file_", assertion.id), fileext = ".csv")
    utils::write.csv(bad_df, file = temp_file, row.names = FALSE)
    on.exit(unlink(temp_file))
    file_encoded <- base64enc::base64encode(temp_file)
    output_file_name <- paste0(assertion.id, ".csv")
    
    as.character(
      htmltools::a(
        href = paste0("data:text/csv;base64,", file_encoded),
        download = output_file_name,
        htmltools::tags$button(
          # `aria-label` = title_text, #tooltip?? not sure what this is
          `data-balloon-pos` = "left",
          style = htmltools::css(
            `background-color` = "#67C2DC",
            color = "#FFFFFF",
            border = "none",
            padding = "5px",
            `font-weight` = "bold",
            cursor = "pointer",
            `border-radius` = "4px"
          ),
          "CSV"
        )
      )
    )
    
  })
}

