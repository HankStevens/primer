#' Two-species Competition-colonization Metapopulation Model
#'
#' This model implements a Levins-type metapopulation model for two species,
#' after Hastings (1980).  For use with \code{ode} in the \code{deSolve}
#' package.
#'
#' @param t Argument for time
#' @param y A vector for population 1 and 2
#' @param params Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variables,
#' y.}
#' @author Hank Stevens <HankStevens@@miamioh.edu>
#' @seealso \code{\link{levins}}, \code{\link{compcolM}},
#' \code{\link{succniche}}
#' @references Hastings, A. (1980) Disturbance, coexistence, history, and
#' competition for space. \emph{Theoretical Population Biology}, \bold{18},
#' 363--373.
#'
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' pars <- c(c1 = .3, c2 = 1, m1 = .1, m2 = .1)
#' pops <- c(.1,.1)
#' out <- ode(y=pops, t=0:20, fun=compcol, parms = pars)
#' matplot(out[,1], out[,-1], type='l')
#'
`compcol` <-
function (t, y, params)
{
    p1 <- y[1]
    p2 <- y[2]
    with(as.list(params), {
        dp1.dt <- c1 * p1 * (1 - p1) - m1 * p1
        dp2.dt <- c2 * p2 * (1 - p1 - p2) - m2 * p2 - c1 * p1 *
            p2
        return(list(c(dp1.dt, dp2.dt)))
    })
}
