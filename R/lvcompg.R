#' A General Lotka-Volterra Competition Model
#'
#' A general Lotka-Volterra competition model, for any number of species.  For
#' use with \code{ode} in the \code{deSolve} package. This function uses a
#' vector and matrix within the list of parameters.
#'
#'
#' @param t the time point for a realization of the integration.
#' @param n the vector of populations, at each time t.
#' @param parms a LIST containing a vector of growth rates (r), and a matrix of
#' interaction coefficients (a).
#' @return Returns a list of length one which is the vector of the rates of
#' increase (required by \code{ode}).
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{lvcomp3}}, \code{\link{clogistic}}, \code{\link{igp}},
#' \code{\link{scheffer}}
#' @references Lotka, A.J. (1956) \emph{Elements of Mathematical Biology}.
#' Dover Publications, Inc.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' ## Specify the number of species
#' S <- 10
#' alpha <- .01
#' r <- runif(S)*2
#' a <- matrix(rnorm(S^2, m=alpha, sd=alpha/10), nrow=S, ncol=S)
#' parms <- list(r,a)
#' t=seq(0,40, by=.1)
#' N0 <- runif(S)/(S*alpha)
#' library(deSolve)
#' lvout <- ode(N0, t, lvcompg, parms)
#' matplot(t, lvout[,-1], type="l", ylab="N", log='y')
#'
`lvcompg` <-
function (t, n, parms)
{
    r <- parms[[1]]
    a <- parms[[2]]
    dns.dt <- r * n * (1 - (a %*% n))
    return(list(c(dns.dt)))
}
