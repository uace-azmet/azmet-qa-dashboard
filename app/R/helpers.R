#' @param na_pass logical; do NAs count as passing the validation step?
gte <- function(x,y, na_pass = FALSE) {
  res <- x >= y
  if(isTRUE(na_pass)) {
    res <- ifelse(is.na(res), TRUE, res)
  }
  res
}

lte <- function(x,y, na_pass = FALSE) {
  res <- x <= y
  if(isTRUE(na_pass)) {
    res <- ifelse(is.na(res), TRUE, res)
  }
  res
}

btwn <- function(x, low, high, na_pass = FALSE) {
  res <- dplyr::between(x, low, high)
  if(isTRUE(na_pass)) {
    res <- ifelse(is.na(res), TRUE, res)
  }
  res
}


#' Validate changes
#' 
#' Checks for changes in an numeric vector `x` that are greater than some
#' `delta`.  If |x[i] - x[i-1]|  <= `delta` it returns `FALSE` for *both*
#' indexes.  The point of this is to get both rows to get extracted in the report
#' so you can evaluate the change.
#' 
#' @param x a numeric vector
#' @param delta maximum allowed change in x from one step to the next
#'
#' @return logical vector
#'
#' @examples
#' x <- c(1,2,3,4,10,11,12,15)
#' check_delta(x, 1)
check_delta <- function(x, delta) {
  out <- abs(x - lag(x)) <= delta & abs(x - lead(x)) <= delta
  #NAs pass
  out[is.na(out)] <- TRUE
  out
}

