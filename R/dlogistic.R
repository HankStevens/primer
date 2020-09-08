#' Discrete Logistic Growth
#'
#' Single species discrete logistic growth -- a difference equation.
#' A function for continuous logistic growth, for use with \code{ode} in the
#' \code{deSolve} package, using method = 'euler' and integer time steps.
#'
#' Of the form,
#' \deqn{N_{t+1} - N_{t} = r_d N_{t}\left(1 - \alpha N_{t}\right)}
#'
#' @param t times points that will return N
#' @param y N
#' @param p a vector of labeled logistic growth parameters; the first must be labeled rd, and the second must be labeled alpha (the value of alpha is 1/K).
#' @return Returns of list of one component (required by \code{ode}).
#' @author Hank Stevens (HankStevens@@miamioh.edu)
#' @seealso \code{\link{clogistic}}, \code{\link{lvcompg}}
#' @references Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' library(deSolve)
#' # MUST use the 'euler' integration method with integer time steps
#' p <- c(rd=1, alpha=.01)
#' time <- 0:10
#' initialN <- 10
#' out <- ode(y=initialN, times=time,
#'              func=dlogistic, parms=p, method='euler')
#' plot(time, out[,-1], type='l')
#'
`dlogistic` <- function (t, y, p) {
        N <- y[1]
        with( as.list(p),{
            N.diff <-  rd * N * (1 - alpha * N)
            return(list(N.diff))  }) }
