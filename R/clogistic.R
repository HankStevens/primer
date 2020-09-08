#' Continuous Logistic Growth
#'
#' A function for continuous logistic growth, for use with \code{ode} in the
#' \code{deSolve} package.
#'
#' The user does not put these directly into this function, but rather uses
#' \code{ode} in the \code{deSolve} package.
#'
#' The function implements the logistic growth equation
#' \deqn{\frac{dN}{dt} = rN\left(1-\alpha N\right)}
#' or equivalently
#' \deqn{\frac{dN}{dt} = rN\left(\frac{K-N}{K}\right)}
#'
#' @param times times points that will return N
#' @param y N
#' @param parms a vector of logistic growth parameters; the first must be r, and the second must be alpha (1/K).
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens <HankStevens@@miamioh.edu>
#' @seealso \code{\link{dlogistic}}, \code{\link{thetalogistic}},
#' \code{\link{levins}}, \code{\link{lvcomp2}}, \code{\link{lvcompg}}
#' @references
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' p <- c(r=1, alpha=.01)
#' time <- 1:10
#' initialN <- 10
#' out <- ode(y=initialN, times=time,
#'              func=clogistic, parms=p)
#' plot(time, out[,-1], type='l')
#'
`clogistic` <-
function (times, y, parms)
{
    n <- y[1]
    r <- parms[1]
    alpha <- parms[2]
    dN.dt <- r * n * (1 - alpha * n)
    return(list(c(dN.dt)))
}
