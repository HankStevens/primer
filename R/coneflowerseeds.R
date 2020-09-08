#' Smooth coneflower seed data
#'
#' A data set containing size and seed set of individual coneflowers.
#'
#' @usage data(coneflowerseeds)
#' @format A data frame with 136 rows and 5 variables:
#' \describe{
#'   \item{logAs}{natural log of total leaf area in the first year}
#'   \item{seeds}{number of seeds produced in the first year}
#' }
#' @keywords datasets
#' @examples
#'
#' data(coneflowerseeds)
#' ggplot2::qplot(logAs, seeds, data=coneflowerseeds)
#'
"coneflowerseeds"
