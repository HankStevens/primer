% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clogistic.R
\name{clogistic}
\alias{clogistic}
\title{Continuous Logistic Growth}
\usage{
clogistic(times, y, parms)
}
\arguments{
\item{times}{times points that will return N}

\item{y}{N}

\item{parms}{a vector of logistic growth parameters; the first must be r, and the second must be alpha (1/K).}
}
\value{
Returns of list of one component (required by \code{ode}).
}
\description{
A function for continuous logistic growth, for use with \code{ode} in the
\code{deSolve} package.
}
\details{
The user does not put these directly into this function, but rather uses
\code{ode} in the \code{deSolve} package.

The function implements the logistic growth equation
\deqn{\frac{dN}{dt} = rN\left(1-\alpha N\right)}
or equivalently
\deqn{\frac{dN}{dt} = rN\left(\frac{K-N}{K}\right)}
}
\examples{

library(deSolve)
p <- c(r=1, alpha=.01)
time <- 1:10
initialN <- 10
out <- ode(y=initialN, times=time,
             func=clogistic, parms=p)
plot(time, out[,-1], type='l')

}
\references{
Stevens. M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series. Springer.
}
\seealso{
\code{\link{dlogistic}}, \code{\link{thetalogistic}},
\code{\link{levins}}, \code{\link{lvcomp2}}, \code{\link{lvcompg}}
}
\author{
Hank Stevens <HankStevens@miamioh.edu>
}
\keyword{methods}
