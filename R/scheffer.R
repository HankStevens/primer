#' A resource-based model of alternative stable states
#'
#' A model of floating vs. submerged plant dominance in shallow aquatic
#' systems, after Scheffeer \emph{et al}. (2003).  For use with \code{ode} in
#' the \code{deSolve} package.  Floating plants are better competitors for
#' light, as long as submerged plants cannot drive down nitrogen levels.
#'
#'
#' @param t the time point for a realization of the integration.
#' @param y the vector of populations, at each time t.
#' @param p a vector or list containing the necessary parameters.
#' @return Returns a list of length one which is the vector of the rates of
#' increase (required by \code{ode}).
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{lvcompg}}, \code{\link{igp}}
#' @references Scheffer, M., Szabo, S., Gragnani, A., van Nes, E.H., Rinaldi,
#' S., Kautsky, N., Norberg, J., Roijackers, R.M.M. and Franken, R.J.M. (2003)
#' Floating plant dominance as a stable state. \emph{Proceeding of the National
#' Academy of Sciences, U.S.A.}, \bold{100}, 4040--4045.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' p <- c(N=2.5, as=0.01, af=0.01, b=0.02, qs=0.075, qf=0.005,
#'        hs=0, hf=0.2, ls=0.05, lf=0.05, rs=0.5, rf=0.5, W=0)
#' t <- 1:200
#' Initial <- c(F=10, S=10)
#' S.out1 <- ode(Initial, t, scheffer, p)
#' matplot(t, S.out1[,-1], type='l')
#' legend('right', c("F", "S"), lty=1:2, col=1:2, bty='n')
#'
`scheffer` <-
function (t, y, p)
{
    F <- y[1]
    S <- y[2]
    with(as.list(p), {
        n <- N/(1 + qs * S + qf * F)
        dF <- rf * F * (n/(n + hf)) * (1/(1 + af * F)) - lf *
            F
        dS <- rs * S * (n/(n + hs)) * (1/(1 + as * S + b * F +
            W)) - ls * S
        return(list(c(dF, dS)))
    })
}
