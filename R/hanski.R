#' Core-satellite Metapopulation Model
#'
#' A function for the core-satellite metaapopulation dynamics, for use with
#' \code{ode} in the \code{deSolve} package.
#'
#'
#' @param t Argument for time
#' @param y A scalar for the population variable
#' @param parms Vector or list of parameters
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variable (a
#' scalar for the proportion of sites occupied).}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link{gotelli}}, \code{\link{hanski}},\code{\link{lande}},
#' \code{\link{MetaSim}}, \code{\link{clogistic}}
#' @references Hanski, I. (1982) Dynamics of regional distribution: the core
#' and satellite species hypothesis. \emph{Oikos}, \bold{38}, 210--221.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!  Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' prms <- c(ci<- 0.15, e=0.05)
#' out <- ode(y=.2, times=1:100, func=hanski, parms=prms )
#' matplot(out[,1], out[,2], type='l', ylab="p", xlab="time")
#'
`hanski` <-
function (t, y, parms)
{
    p <- y[1]
    with(as.list(parms), {
        dp <- ci * p * (1 - p) - e * p * (1 - p)
        return(list(dp))
    })
}
