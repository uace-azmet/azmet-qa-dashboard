#' @param na_pass logical; do NAs count as passing the validation step?
gte <- function(x,y, na_pass = FALSE) {
  res <- x >= y
  if(isTRUE(na_pass)) {
    res <- if_else(is.na(res), TRUE, res)
  }
  res
}

lte <- function(x,y, na_pass = FALSE) {
  res <- x <= y
  if(isTRUE(na_pass)) {
    res <- if_else(is.na(res), TRUE, res)
  }
  res
}

btwn <- function(x, low, high, na_pass = FALSE) {
  res <- between(x, low, high)
  if(isTRUE(na_pass)) {
    res <- if_else(is.na(res), TRUE, res)
  }
  res
}
