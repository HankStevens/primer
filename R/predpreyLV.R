#' The Lotka-Volterra Predator-prey Model
#'
#' The Lotka-Volterra predator-prey model, for use with \code{ode} in the
#' \code{deSolve} package.
#'
#'
#' @param t Argument for time
#' @param y A vector of length 2, for population 1 and 2
#' @param params Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variables,
#' y.}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{predpreyRM}}
#' @references Lotka, A.J. (1956) \emph{Elements of Mathematical Biology}.
#' Dover Publications, Inc.
#'
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' params1 <- c(b=.5, a=.01, s=.2, e=.1)
#' Time <- seq(0,100, by=.1) # Set time here
#' LV.out <- ode(c(H0=25,P0=5), Time, predpreyLV, params1)
#' matplot(Time, (LV.out[,2:3]), type="l", ylab="Population Size")
#'
`predpreyLV` <-
function (t, y, params)
{
    H <- y[1]
    P <- y[2]
    with(as.list(params), {
        dH.dt <- b * H - a * P * H
        dP.dt <- e * a * P * H - s * P
        return(list(c(dH.dt, dP.dt)))
    })
}
