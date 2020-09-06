#' Discrete Logistic Growth
#'
#' Lotka-Volterra single species discrete population growth.
#'
#' Of the form, \deqn{N_{t+1} = N_{t} + r_d N_{t}\left(1 - \alpha N_{t}\right)}
#'
#' @param alpha per capita negative density dependence (a positive value will
#' result in the usual negative effect)
#' @param rd discrete growth increment
#' @param N0 initial population size
#' @param t end time point
#' @return Returns a vector of population sizes from \eqn{N_0,\, \ldots,\,
#' N_t}{N[0] to N[t]}, for integer time points.
#' @author Hank Stevens (HStevens@@muohio.edu)
#' @seealso \code{\link{clogistic}}, \code{\link{dlvcomp2}},
#' \code{\link{lvcompg}}
#' @references Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' # MUST use the 'euler' integration method with integer time steps
#' p <- c(r=1, alpha=.01)
#' time <- 0:10
#' initialN <- 10
#' out <- ode(y=initialN, times=time,
#'              func=clogistic, parms=p, method='euler')
#' plot(time, out[,-1], type='l')
#'
`dlogistic` <-
function (alpha = 0.01, rd = 1, N0 = 2, t = 15)
{
    N <- c(N0, numeric(t))
    for (i in 1:t) N[i + 1] <- {
        N[i] + rd * N[i] * (1 - alpha * N[i])
    }
    return(N)
}
