#' Convert `data.validator` report into custom `gt` table
#'
#' @param report a tibble created by `data.validator::get_results()`
#' @param data original data frame used to generate the report
#'
#' @return a `gt` table
format_report_gt <- function(report, data, title = NULL) {
  #TODO make warn_at and error_at arguments so they can be adjusted in the dashboard maybe?
  #TODO would it be possible to add a header to the table that displays these thresholds?
  warn_at <- 1 #warn if ≥ 1 row fails a validation
  error_at <- 0.01 #error if ≥ 1% of rows fail validation
  
  table <- report |> 
    #for some reason, report has duplicated rows. Seems like a bug in data.validator
    slice_head(n = 1, by = assertion.id) |> 
    mutate(n_failed = if_else(is.na(num.violations), 0, num.violations),
           p_failed = n_failed / nrow(data)) |> 
    mutate(mark = case_when(
      p_failed >= error_at ~ "🛑",
      n_failed >= warn_at ~ "⚠️️",
      .default = "✅"
    )) |> 
    select(assertion.id, mark, validation = description, n_failed, p_failed, bad_rows) |> 
    mutate(bad_rows = ifelse(
      is.na(bad_rows), #if no bad rows...
      "&mdash;", #...print an emdash (—) instead of "NA"...
      make_csv_button(assertion.id, bad_rows) #...else convert tibble into a download button
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
  
  if (!is.null(title)) {
    table <- table |> tab_header(title = md(title))
  }
  table
}




#' Reformat tibbles in a list-column into html code for a download button
#'
#'helper to reformat list column of tibbles to be html for a button that
#'downloads the tibble as a .csv. Adapted from `pointblank` package:
#'https://github.com/rich-iannone/pointblank/blob/645698bc3ac1080f97ee14081d6cfe61a2c1d70f/R/get_agent_report.R#L1365
#'
#' @param assertion.id randomly generated id number column from data.validator
#' @param bad_df list column of tibbles containing data from failed validations
#'
#' @return a vector of HTML code for buttons
#' 
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

