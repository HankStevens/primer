#' Multi-species Competition-colonization Model, With Habitat Destruction
#'
#' Multi-species competition colonization model, with habitat destruction,
#' after Nee and May (1992).  For use with \code{ode} in the \code{deSolve}
#' package.
#'
#'
#' @param t Argument for each time point
#' @param y A vector for the populations
#' @param params Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variables,
#' y.}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{levins}}, \code{\link{compcol}},
#' \code{\link{succniche}}
#' @references Nee, S. and May, R.M. (1992) Dynamics of metapopulations:
#' habitat destruction and competitive coexistence. \emph{Journal of Animal
#' Ecology}, \bold{61}, 37--40.
#'
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' S <- 10
#' ci <- 2^seq(-5, 5, length=S)
#' m <- rep(.1, S)
#' params <- list(ci=ci, m=m, S=S, D=0)
#' init.N <- rep(0.01, S); t=seq(1, 200, .1)
#' cc.out <- ode(init.N, t, compcolM, params)
#' matplot(t, cc.out[, -1], type="l", ylab="Proportion of Habitat", xlab="Years")
#'
#'
`compcolM` <-
  function(t, y, params)
{
## This models S species with a competition-colonization tradeoff
  ## This requires 'params' is a list with named elements
  S <- params[["S"]]; D <- params[["D"]]
  with(params,
list( dpi.dt <- sapply(1:S, function(i) {
params[["ci"]][i] * y[i] * ( 1 - D - sum(y[1:i]) ) -
   params[["m"]][i] * y[i] - sum( params[["ci"]][0:(i-1)] * y[0:(i-1)] * y[i] )  }
                       )
     )
       )
}
