#' A Four-state model of Successional Dynamics
#'
#' This is the five-state, two-species model of the succession-niche model,
#' after Pacala and Rees (1998).  For use with \code{ode} in the \code{deSolve}
#' package.
#'
#'
#' @param t Argument for the time point at integration
#' @param y A vector of length four, for states, E, M, S, and R.
#' @param params Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variable (a
#' scalar for the proportion of sites occupied).}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{levins}}, \code{\link{compcol}}, \code{\link{compcolM}}
#' @references Pacala, S.W. and Rees, M. (1998) Models suggesting field
#' experiments to test two hypotheses explaining successional diversity.
#' \emph{The American Naturalist}, \bold{152}, 729--737.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' params.suc <- c(a=7, c=0.2, g=.1, m=0.04, D=0)
#' t=seq(0,50,.1)
#' init.suc <- c(S=0, E=0.5, M=0.5, R=0.00) # Free space is implicit, F=1-(S+E+M+R).
#' ccg.out <- data.frame(ode(init.suc, t, succniche, params.suc))
#' matplot(t, ccg.out[,-1], type="l", ylab="Relative Frequency",
#'          xlab="Time", ylim=c(0,1) )
#' legend("right", colnames(ccg.out)[5:2], lty=4:1,  bty="n")
#'
`succniche` <-
function (t, y, params)
{
    S <- y[1]
    E <- y[2]
    M <- y[3]
    R <- y[4]
    F <- max(c(0, 1 - params["D"] - S - E - M - R))
    with(as.list(params), {
        dS = c * (S + R + M) * F - a * c * (M + E) * S - g *
            S - m * S
        dR = g * (S + M) - m * R
        dM = a * c * (M + E) * S + c * (S + R + M) * E - g *
            M - m * M
        dE = a * c * (M + E) * F - c * (S + R + M) * E - m *
            E
        return(list(c(dS, dE, dM, dR)))
    })
}
