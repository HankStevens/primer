#' Continuous Theta-Logistic Growth
#'
#' A function for continuous theta-logistic growth, for use with \code{ode} in
#' the \code{deSolve} package.
#'
#' The user does not put these directly into this function, but rather uses
#' \code{ode} in the \code{deSolve} package. See \code{ode} in the
#' \code{deSolve} package.
#'
#' @param times Times points that will return N
#' @param y N
#' @param parms a vector of logistic growth parameters
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens (HStevens@@muohio.edu)
#' @seealso \code{\link{clogistic}}
#' @references Sibly, R.M., Barker, D., Denham, M.C., Hone, J., and Pagel, M.
#' (2005) On the regulation of populations of mammals, birds, fish, and
#' insects. \emph{Science}, \bold{309}, 607--610.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' p <- c(r=1,alpha=.01, theta=.5)
#' time <- seq(1,10, by=.1)
#' initialN <- 10
#' out <- ode(y=initialN, times=time, func=thetalogistic, parms=p)
#' plot(time, out[,-1], type='l')
#'
`thetalogistic` <-
function (times, y, parms)
{
    n <- y[1]
    with(as.list(parms), {
        dN.dt <- r * n * (1 - (alpha * n)^theta)
        return(list(c(dN.dt)))
    })
}
