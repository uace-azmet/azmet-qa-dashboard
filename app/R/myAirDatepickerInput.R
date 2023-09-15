#' Modified version of `airDatepickerInput()` that makes the text field non-editable
#'
#' @param ... arguments passed to `airDatepickerInput()`
#'
myAirDatepickerInput <- function(...) {
  air <- airDatepickerInput(...)
  # Modify HTML tags with {htmltools}
  # more docs here: https://rstudio.github.io/htmltools/articles/tagQuery.html
  tagQ <- tagQuery(air)
  air <- tagQ$
    find("input")$
    addAttrs("readonly" = NA)$ # make the input read-only
    allTags()
  tagList(
    # Change input background color in read-only mode, otherwise it'll be grey
    tags$style(".form-control[readonly] {background-color: #fff;}"),
    air
  )
}