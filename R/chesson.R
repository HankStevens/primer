#' Two-species model of the storage effect
#'
#' Simulates a fluctuating environment over time, and two species' responses to
#' the environment, after Chesson (1994).
#'
#' The argument \code{type} controls the value of \eqn{e^C}, the effect of
#' competition on reproduction, where the annual finite rate of increase is
#' \eqn{R=e^{E-C}}{R=e^(E-C)}.  \code{type = 1} causes \eqn{e^C = \alpha_i
#' N_{J,i}}{e^C = alpha[i] N[J,i]}, that is, a species-specific fixed fraction
#' of juveniles that depends on each species response to competition. This is
#' illustrated in a for-loop in Stevens (2009, \emph{Ch. 9, Storage Effect,
#' Simulating Dynamics}). Any other value for \code{type} results in the same
#' negative effect on both species that depends on the number of juveniles and
#' the disturbance rate.
#'
#' @param alpha a vector of length 2; the negative effects of all individuals
#' (of both species) on each population -- typically different among species.
#' @param d disturbance rate; the proportion of all individuals killed at each
#' time step.
#' @param years numbers of time steps
#' @param N0 vector of length 2; initial abundances.
#' @param w vector of length 2; average fitnesses for each species.
#' @param env.var degree of environmental variability.
#' @param specialization determines the narrowness of each species fitness
#' response.
#' @param spread determines how far apart the peak fitness responses are.
#' @param type determines the form of \emph{C}, the negative effect of
#' competition.
#' @return Returns a list of length one, for use with \code{ode} in the
#' \code{deSolve} package.  \item{Component 1 }{vector of the state variables,
#' y.}
#' @author Hank Stevens (HankStevens@@miamioh.edu)
#' @seealso \code{\link{succniche}}
#' @references
#'
#' Chesson, P.L. (1994) Multispecies competition in variable environments.
#' \emph{Theoretical Population Biology}, \bold{45}, 227--276.
#'
#' Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer.
#' @keywords methods
#' @export
#' @examples
#'
#' out <- chesson(years=50)
#' layout(matrix(1:4, nc=2))
#' matplot(out[["time"]], out[["Ns"]], type='l', lty=c(1:3),
#'         xlab="Time", ylab="N", log="y")
#' plot(out[["time"]][-1], out[["env"]], type='l',
#'      xlab="Time", ylab="Environment")
#' matplot(out[["env"]], out[["Es"]], xlab="Environment",
#'  ylab="Density-independent reproduction")
#' matplot(out[["env"]], out[["Rs"]], xlab="Environment",
#'  ylab="Annual growth rate")
#'
#'
'chesson' <-
  function(alpha=c(1.1*1e-5, 1e-5), d=.1, years=10, N0=c(1e3,1e5), w=c(.6, 1), env.var=1, specialization=1, spread=0.67, type=1) {
  if(spread>1 | spread<0) {
    stop("'spread' must be between zero and 1.")
  }
  if(specialization < 0 ) {
    stop("'specialization' must be non-negative.")
  }
### In terms of the Beta distribution:
  ## 'specialization' is a - 1,
  ## b is a function of the mode which is a function of 'spread'.
t <- 1:years
env.beta <- rbeta(years, 1/env.var, 1/env.var)

w.rare <- w[1]
w.comm <- w[2]
a.r <- specialization + 1
mode.r <- .5 + spread/2
b.r <- (a.r - 1)/mode.r - a.r + 2
  if(is.infinite(a.r)) stop("Niche not logical (a of Beta(x,a,b) is infinite)")
a.c <- b.r
b.c <- a.r
Bs <- matrix(NA, nrow=years, ncol=2)
a.r;b.r
  Bs[,1] <- dbeta(env.beta, a.r, b.r)
Bs[,2] <- dbeta(env.beta, a.c, b.c)
rhof <- function(x) {min(dbeta(x, a.r,b.r),dbeta(x,a.c,b.c)) }
rho <- integrate(Vectorize(rhof), 0, 1)
Es <- matrix(NA, nrow=years, ncol=2)
Es[,1] <- w.rare * Bs[,1]
Es[,2] <- w.comm * Bs[,2]
alpha <- alpha
d <- d
Ns <- matrix(NA, nrow=years+1, ncol=2)
Ns[1,] <- N0
Cs <- matrix(NA, nrow=years, ncol=2)
Rs <- matrix(NA, nrow=years, ncol=2)

for(i in 1:years) Ns[i+1,] <- {
  juveniles <- sum(exp(Es[i,])*Ns[i,])
  if(type==1)
    Cs[i,]  <-  alpha*juveniles   else {
    Cs[i,] <- rep( log(juveniles/sum(d*Ns[i,])), 2) }
  Rs[i,] <- exp(Es[i,]-Cs[i,])
  (1-d) * Ns[i,] + Rs[i,]*Ns[i,]
                              }
 return(list(time=c(0,t), Ns=Ns, Es=Es, Cs=Cs, Rs=Rs, Bs=Bs,
             env=env.beta-.5, overlap=rho[["value"]],
             params=c(a.r,b.r,a.c,b.c, spread)))
}
