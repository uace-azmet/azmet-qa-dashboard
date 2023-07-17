#' Create validation report for daily data
#'
#' @param daily data frame from azmetr::az_daily()
#'
#' @return a data.validator report
#' @examples
#' library(azmetr)
#' library(data.validator)
#' daily <- az_daily(start = "2023-07-01", end = "2023-07-12")
#' check_daily(daily)
check_daily <- function(daily) {
  report <- data.validator::data_validation_report()
  data.validator::validate(daily, name = "Daily Data") |>
    # Internal consistency checks from 'NWS (1994) TSP 88-21-R2':
    data.validator::validate_if(gte(temp_air_meanC, dwpt_mean, na_pass = TRUE),
                "`temp_air_meanC` ≥ `dwpt_mean`") |>
    data.validator::validate_if(
      btwn(
        temp_air_meanC,
        temp_air_minC,
        temp_air_maxC,
        na_pass = TRUE
      ),
      description = "`temp_air_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      lte(
        wind_spd_mean_mps,
        wind_spd_max_mps,
        na_pass = TRUE
      ),
      description = "`wind_spd_mean_mps` ≤ `wind_spd_max_mps`"
    ) |>
    data.validator::validate_if(
      btwn(
        relative_humidity_mean,
        relative_humidity_min,
        relative_humidity_max,
        na_pass = TRUE
      ),
      description = "`relative_humidity_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      btwn(
        temp_soil_50cm_meanC,
        temp_soil_50cm_minC,
        temp_soil_50cm_maxC,
        na_pass = TRUE
      ),
      description = "`temp_soil_50cm_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::validate_if(
      btwn(
        temp_soil_10cm_meanC,
        temp_soil_10cm_minC,
        temp_soil_10cm_maxC,
        na_pass = TRUE
      ),
      description = "`temp_soil_10cm_*` (min ≤ mean ≤ max)"
    ) |>
    data.validator::add_results(report)
  report
  
}

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

