#' Simulation of Stochastic Metapopulation Models
#'
#' Originally focused on creating a community of core-satellite species, this
#' function allows simulation of several metapopulation models, where
#' colonization and extinction rates are stochastic draws from uniform
#' distributions, with specified means and ranges.
#'
#' \code{phi} is one half of the relative range of each rate (colonization and
#' extinction).  For each time step, each rate is drawn from a uniform
#' distribution, Unif(rate-phi, rate+phi).  Thus, the range is 2*phi, and
#' center on the specified mean (ci or e).
#'
#' @param Time A scalar for the number of time steps over which to simulate
#' each population.
#' @param NSims A scalar for the number of simulations, which is analogous to
#' the number of species in the community.
#' @param method A character string, in quotes, specifying which metapopulation
#' model to use: "hanksi", "gotelli", "lande", "levins". See "See Also" below.
#' @param ci Scalar for mean colonization rate.
#' @param e Scalar for mean extinction rate.
#' @param phi A scalar for the relative variability in rates. See Details.
#' @param p0 Initial proportion of sites occupied for each species.
#' @param D Parameter for habitat destruction; applies to only the "lande"
#' model.
#' @return Function returns a list with these components.  \item{method }{The
#' method used (default is "hanski").} \item{time }{The integer sequence of
#' times, from 0 to the value of the argument \code{Time}.} \item{Ns
#' }{\code{Time} by \code{NSims} matrix of observed population sizes.}
#' \item{Parameters }{A named vector of the parameters used for the
#' simulations.}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{gotelli}}, \code{\link{hanski}}, \code{\link{levins}}
#' @references Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' out <- MetaSim(NSims=2)
#' pops <- out$Ns
#' matplot(out$t, pops, type='l')
#' title(sub=paste(out$method, "model"))
#'
`MetaSim` <-
function (Time = 50, NSims = 1, method = "hanski", ci = 0.25,
    e = 0.25, phi = 0.75, p0 = 0.5, D = 0.5)
{

    out.stochastic <- matrix(NA, nrow = Time + 1, ncol = NSims)
    for (run in 1:NSims) out.stochastic[, run] <- {
        dpdt <- switch(pmatch(method, c("hanski", "levins", "gotelli",
            "lande"), nomatch = ""), `1` = function(t, y, parms) {
            p <- y[1]
            with(as.list(parms), {
                dp <- cir * p * (1 - p) - er * p * (1 - p)
                return(list(dp))
            })
        }, `2` = function(t, y, parms) {
            p <- y[1]
            with(as.list(parms), {
                dp <- cir * p * (1 - p) - er * p
                return(list(dp))
            })
        }, `3` = function(t, y, parms) {
            p <- y[1]
            with(as.list(parms), {
                dp <- cir * (1 - p) - er * p
                return(list(dp))
            })
        }, `4` = function(t, y, parms) {
            p <- y[1]
            with(as.list(parms), {
                dp <- cir * p * (1 - D - p) - er * p
                return(list(dp))
            })
        })
        nout <- numeric(Time + 1)
        nout[1] <- p0
        for (j in 1:Time) nout[j + 1] <- {
            cir <- runif(1, ci * (1 - phi), ci * (1 + phi))
            er <- runif(1, e * (1 - phi), e * (1 + phi))
            params <- c(cir = cir, er = er, D = D)
            n <- ode(nout[j], 0:1, dpdt, params)[2, 2]
            if (n < 0)
                n = 0
            if (n > 1)
                n = 1
            n
        }
        nout
    }
    list(method = method, t = 0:(Time), Ns = as.matrix(out.stochastic),
        Parameters = c(NSims = NSims, ci = ci, e = e, phi = phi,
            p0 = p0, D = D))
}
