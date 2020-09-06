#' A Lotka-Volterra Model of Intraguild Predation
#'
#' A Lotka-Volterra model of intraguild predation, after Holt and Polis (1997).
#' For use with \code{ode} in the \code{deSolve} package.
#'
#'
#' @param t the time point for a realization of the integration.
#' @param y the vector of populations, at each time t.
#' @param params a vector or list containing the necessary parameters.
#' @return Returns a list of length one which is the vector of the rates of
#' increase (required by \code{ode}).
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{lvcompg}}, \code{\link{scheffer}}
#' @references Holt, R.D. and Polis, G.A. (1997) A theoretical framework for
#' intraguild predation. \emph{The American Naturalist}, \bold{149}, 745--764.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' params <- c(bpb= 0.032, abp=10^-8, bpn=10^-5, anp=10^-4,  mp=1,
#'              bnb=0.04, abn=10^-8, mn=1,
#'              r=1, abb=10^-9.5)
#' t=seq(0, 60, by=.1)
#' N.init <- c(B = 10^9, N = 10^4, P = 10^3)
#' igp.out <- ode(N.init, t, igp, params)
#' matplot(t, log10(igp.out[,-1]+1), type="l",
#'           ylab="log(Abundance)")
#' legend('right', c("B", "N", "P"), lty=1:3, col=1:3, lwd=2,
#'        bty="n")
#'
`igp` <-
function (t, y, params)
{
    B <- y[1]
    N <- y[2]
    P <- y[3]
    with(as.list(params), {
        dPdt <- bpb * abp * B * P + bpn * anp * N * P - mp *
            P
        dNdt <- bnb * abn * B * N - mn * N - anp * N * P
        dBdt <- r * B * (1 - abb * B) - abn * B * N - abp * B *
            P
        return(list(c(dBdt, dNdt, dPdt)))
    })
}
