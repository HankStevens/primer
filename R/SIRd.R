#' The S-I-R Epidemilogical Disease Model
#'
#' The S-I-R epidemilogical disease model with density-dependent transmission, for use with \code{ode} in the
#' \code{deSolve} package.
#'
#' The user does not put these directly into this function, but rather uses
#' \code{ode} in the \code{deSolve} package.
#'
#' @param t times points for which values will be returned
#' @param y the vector of disease states of hosts (S, I, R)
#' @param p a vector of parameters
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens <Hank.Stevens@@miamioh.edu>
#' @seealso \code{\link{ross}}, \code{\link{SIRf}}, \code{\link{SIRbd}}
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
#' N <- 10^3; I <- R <- 1; S <- N - I - R
#' parms <- c(B=.01, g=4)
#' months <- seq(0, 3, by=0.01)
#' require(deSolve)
#' SIR.out <- data.frame( ode(c(S,I,R), months, SIRd, parms) )
#' matplot(months, SIR.out[,-1], type='l')
#' legend('right', c('R', 'I', 'S'), lty=3:1, col=3:1, bty='n')
#'
`SIRd` <-
function (t, y, p)
{
    {
        S <- y[1]
        I <- y[2]
        R <- y[3]
    }
    with(as.list(p), {
        dS.dt <- -B * I * S
        dI.dt <- B * I * S - g * I
        dR.dt <- g * I
        return(list(c(dS.dt, dI.dt, dR.dt)))
    })
}
