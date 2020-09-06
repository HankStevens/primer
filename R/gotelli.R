#' Propagule Rain Metapopulation Model
#'
#' A function for the propagule rain or mainland-island metapopulation
#' dynamics, for use with \code{ode} in the \code{deSolve} package.
#'
#'
#' @param t Argument for time
#' @param y A scalar for the population variable
#' @param parms Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variable (a
#' scalar for the proportion of sites occupied).}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{gotelli}}, \code{\link{hanski}}, \code{\link{lande}},
#' \code{\link{MetaSim}}, \code{\link{clogistic}}
#' @references Gotelli, N.J. (1991) Metapopulation models: the rescue effect,
#' the propagule rain, and the core-satellite hypothesis. \emph{The American
#' Naturalist}, \bold{138}, 768--776.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#'
#' ## The function is currently defined as
#' function (t, y, parms)
#' {
#'     p <- y[1]
#'     with(as.list(parms), {
#'         dp <- ce * (1 - p) - e * p
#'         return(list(dp))
#'     })
#'   }
#'
#' library(deSolve)
#' p <- c(ce=.1, e=.01)
#' time <- 1:10
#' initialN <- .3
#' out <- ode(y=initialN, times=time, func=gotelli, parms=p)
#' plot(time, out[,-1], type='l')
#'
`gotelli` <-
function (t, y, parms)
{
    p <- y[1]
    with(as.list(parms), {
        dp <- ce * (1 - p) - e * p
        return(list(dp))
    })
}
