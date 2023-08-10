#How to make the date field non-editable
library(shinyWidgets)
library(htmltools)

myAirDatepickerInput <- function(...) {
  air <- airDatepickerInput(...)
  # Modify HTML tags with {htmltools}
  # more docs here: https://rstudio.github.io/htmltools/articles/tagQuery.html
  tagQ <- tagQuery(air)
  air <- tagQ$
    find("input")$
    addAttrs("readonly" = NA)$ # make the input readonly
    allTags()
  tagList(
    # Change input background color in readonly mode, otherwise it'll be grey
    tags$style(".form-control[readonly] {background-color: #fff;}"),
    air
  )
}