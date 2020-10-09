#' The S-I-R Epidemilogical Disease Model with Births and Deaths
#'
#' The S-I-R epidemilogical disease model with births and deaths (population
#' dynamics), for use with \code{ode} in the \code{deSolve} package.  This
#' model uses mass action transmission.
#'
#' The user does not put these directly into this function, but rather uses
#' \code{ode} in the \code{deSolve} package.
#'
#' @param t times points for which values will be returned
#' @param y the vector of disease states of hosts (S, I, R)
#' @param p a vector of parameters
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens <HStevens@@muohio.edu>
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
#' g <- 1/(13/365); b <- 1/50;
#' age <- 5; R0 <- 1 + 1/(b*age)
#' B <- R0 * (g + b) / N
#' parms <- c(B = B, g = g, b = b, m=b)
#' years <- seq(0,30, by=.1)
#' SIRbd.out <- data.frame(ode(c(S=S,I=I,R=R), years, SIRbd, parms, hmax=.01))
#' matplot(SIRbd.out[,1], sqrt(SIRbd.out[,-1]), type='l',
#'         lty=1:3, ylab="sqrt(No. of Individuals)", xlab='Years')
#' legend('right', c('S','I','R'), lty=1:3, col=1:3, bty='n')
#'
`SIRbd` <-
function (t, y, p)
{
    S <- y[1]
    I <- y[2]
    R <- y[3]
    with(as.list(p), {
        dS.dt <- b * (S + I + R) - B * I * S - m * S
        dI.dt <- B * I * S - g * I - m * I
        dR.dt <- g * I - m * R
        return(list(c(dS.dt, dI.dt, dR.dt)))
    })
}
