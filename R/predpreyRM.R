#' Rosenzweig-MacArthur Predator-prey Model
#'
#' An implementation of a predator-prey model, after Rosenzweig and MacArthur
#' (1963).  Includes prey density-dependence, and a type-II predator functional
#' response.  For use with \code{ode} in the \code{deSolve} package.
#'
#'
#' @param t Argument for time
#' @param y A vector of length 2, for population 1 and 2
#' @param p Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variables,
#' y.}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{predpreyLV}}
#' @references Rosenzweig, M.L. and MacArthur, R.H. (1963) Graphical
#' representation and stability conditions of predator-prey interactions.
#' \emph{The American Naturalist}, \bold{97}, 209--223.
#'
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' pars <- c(b = .8, e = 0.07, s = .2, w = 5, D = 400, alpha = 0.001)
#' Time <- 50
#' RM1 <- ode(c(900,120), 1:Time, predpreyRM, pars)
#' matplot(1:Time, RM1[,-1], type='l')
#'
`predpreyRM` <-
function (t, y, p)
{
    H <- y[1]
    P <- y[2]
    with(as.list(p), {
        dH.dt <- b * H * (1 - alpha * H) - w * P * H/(D + H)
        dP.dt <- e * w * P * H/(D + H) - s * P
        return(list(c(dH.dt, dP.dt)))
    })
}
