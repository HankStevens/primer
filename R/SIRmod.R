#' The S-I-R Epidemilogical Disease Model
#'
#' The S-I-R epidemiological disease model with births and deaths (population
#' dynamics), for use with \code{ode} in the \code{deSolve} package.  This
#' model uses scaled transmission, where z controls the degree of density- and frequency-dependence.
#'
#' The user does not put these directly into this function, but rather uses
#' \code{ode} in the \code{deSolve} package.
#'
#' @param t times points for which values will be returned
#' @param y the vector of disease states of hosts (S, I, R)
#' @param p a vector of parameters
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens <Hank.Stevens@@miamioh.edu>
#' @seealso \code{\link{ross}}, \code{\link{SIRf}}, \code{\link{SIRd}}
#' @references Ellner, S.P. and Guckenheimer, J. (2006) \emph{Dynamic Models in
#' Biology}, Princeton University Press.
#'
#' Kermack, W.O. and McCormick, W.G. (1927) A contribution to the mathematical
#' theory of epidemics. \emph{Proceedings of the Royal Society, Series A},
#' \bold{115}, 700--721.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}, Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' N <- 10^6; R <- 0; I <- 1; S <- N - I - R
#' g <- 1/(13/365); b <- 1/50; z <- 0;
#' age <- 5; R0 <- 1 + 1/(b*age)
#' B <- R0 * (g + b) / N
#' parms <- c(B = B, g = g, b = b, mu=b)
#' years <- seq(0,30, by=.1)
#' SIR.out <- data.frame(ode(c(S=S,I=I,R=R), years, SIRmod, parms, hmax=.01))
#' matplot(SIR.out[,1], sqrt(SIR.out[,-1]), type='l',
#'         lty=1:3, ylab="sqrt(No. of Individuals)", xlab='Years')
#' legend('right', c('S','I','R'), lty=1:3, col=1:3, bty='n')
#'
`SIRmod` <-
function (t, y, p)
{
    S <- y[1]
    I <- y[2]
    R <- y[3]
    N <- sum(y)
    with(as.list(p), {
        dS.dt <- b * (S + I + R) - B * I * S/N^z - mu * S
        dI.dt <- B * I * S/N^z - g * I - mu * I
        dR.dt <- g * I - mu * R
        return(list(c(dS.dt, dI.dt, dR.dt)))
    })
}

