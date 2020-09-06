#' A Metapopulation Model with Habitat Destruction
#'
#' A function for the metaapopulation dynamics, for use with \code{ode} in the
#' \code{deSolve} package.
#'
#'
#' @param t Argument for time
#' @param y A scalar for the population variable
#' @param parms Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variable (a
#' scalar for the proportion of sites occupied).}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{gotelli}}, \code{\link{hanski}},\code{\link{levins}},
#' \code{\link{MetaSim}}, \code{\link{clogistic}}
#' @references P. Kareiva and Wennergren, U. (1995) Connecting landscape
#' patterns to ecosystem and population processes. \emph{Nature}, \bold{373},
#' 299--302.
#'
#' Lande, R. (1987) Extinction thresholds in demographic models of territorial
#' populations. \emph{The American Naturalist}, \bold{130}, 624--635.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!  Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' ## The function is currently defined as
#' function (t, y, parms)
#' {
#'     p <- y[1]
#'     with(as.list(parms), {
#'         dp <- ci * p * (1 - D - p) - e * p
#'         return(list(dp))
#'     })
#'   }
#' library(deSolve)
#' p <- c(ci=.1, e=.01, D=.5)
#' time <- 1:10
#' initialN <- .3
#' out <- ode(y=initialN, times=time, func=lande, parms=p)
#' plot(time, out[,-1], type='l')
#'
`lande` <-
function (t, y, parms)
{
    p <- y[1]
    with(as.list(parms), {
        dp <- ci * p * (1 - D - p) - e * p
        return(list(dp))
    })
}
