#' Smooth coneflower data
#'
#' A data set containing size, survival, and flowering in smooth coneflower
#'
#' @usage data(coneflower)
#' @format A data frame with 136 rows and 5 variables:
#' \describe{
#'   \item{logA}{natural log of total leaf area in the first year}
#'   \item{logAp}{natural log of total leaf area in the second year (Ap = A prime)}
#'   \item{surv}{indicator of survival from year one to year 2 (0,1)}
#'   \item{flower_p}{indicator of flowering in year 2 (0,1)}
#' }
#' @source Data provided graciously by Rachel Collins (Roanoke College).
#' @keywords datasets
#' @examples
#'
#' data(coneflower)
#' ggplot2::qplot(logA, logAp, data=coneflower)
#'
"coneflower"
