#' Logistic growth with the Allee effect
#'
#' A function for continuous logistic growth with the Allee effect, for use with \code{ode} in the \code{deSolve} package.
#' @usage alogistic(t, y, p)
#' @param t Time points for which N wll be returned.
#' @param y N, population size
#' @param p a vector of logistic growth (r, alpha) and Allee effect parameters (a, tau); a is the threshold population size.
#' @keywords logistic Allee effect
#' @details
#' The user does not put these directly into this function, but rather uses code{ode} in the \code{deSolve} package.
#' The function is based on the logistic growth equation
#' \deqn{\frac{dN}{dt} = rN\left(1-\alpha N\right)\left(1 - \frac{a+\tau}{N+\tau}\right)}
#' with \eqn{a} being the threshold population size.
#' @export
#' @author Hank Stevens
#' @references
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series. Springer.
#' @seealso \code{\link{clogistic}},\code{\link{dlogistic}}, \code{\link{thetalogistic}}, \code{\link{levins}}
#'
#' @examples
#' library(deSolve)
#' p <- c(r=1,alpha=.01, a=10, tau=.1)
#' time <- 0:10
#' initialN <- 11
#' out <- ode(y=initialN, times=time,
#'              func=alogistic, parms=p)
#' plot(time, out[,-1], type='l')
#'
alogistic <- function(t, y, p){
  with(as.list(p),{
  dN.dt <- r * y[1] * (1 - alpha * y[1])*(1 - (a + tau)/(y[1]+tau))
  return( list( dN.dt ) )
  })
}

