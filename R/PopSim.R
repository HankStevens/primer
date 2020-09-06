#' Population Simulator
#'
#' Resampling stochastic simulator for a single density-independent population.
#'
#' Designed to simulate trajectories based on resampled observed N[t+1]/N[t].
#'
#' @param Rs vector of observed annual growth rates (N[t+1]/N[t]).
#' @param N0 initial population size.
#' @param years number of years to simulate.
#' @param sims number of simulated populations.
#' @return Returns a matrix of population sizes for time = t (rows) for each
#' replicated population (columns).
#' @seealso \code{\link{sparrows}}
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' lambdas <- runif(10, .5, 1.5)
#' out <- PopSim(Rs=lambdas, years=50, N0=50)
#' matplot(0:50, out, type='l')
#' summary(out[51,])
#'
`PopSim` <-
function (Rs, N0, years = 50, sims = 10)
{
    sim.RM = matrix(sample(Rs, size = sims * years, replace = TRUE),
        nrow = years, ncol = sims)
    output <- numeric(years + 1)
    output[1] <- N0
    outmat <- sapply(1:sims, function(i) {
        for (t in 1:years) output[t + 1] <- round(output[t] *
            sim.RM[t, i], 0)
        output
    })
    return(outmat)
}
