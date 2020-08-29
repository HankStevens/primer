#' plot_f
#'
#' Function to plot a time series and its periodogram, and calculate the confidence interval of 2 times the spectral power.
#'
#' @param z numeric vector.
#' @keywords noise, 1/f, color spectra, spectral mimicry, periodogram
#' @export
#' @author Hank Stevens
#' @references
#' J. M. Halley. Ecology, evolution and 1/f-noise. Trends in Ecology & Evolution, 11:33-37, 1996.
#' O. L. Petchey, A. Gonzalez, and H. B. Wilson. Effects on population persistence: the interaction between environmental noise colour, intra-specific competition and space. Proceedings of the Royal Society of London Series B, 264:1841-1847, 1997.
#' J. E. Cohen, C. M. Newman, A. E. Cohen, O. L. Petchey, and A. Gonzalez. Spectral mimicry: a method of synthesizing matching time series with different Fourier spectra. Circuits, Systems and Signal Processing, 18:431-442, 1999.
#' @seealso
#' [one_over_f()] to generate 1/f noise; [spec_mimic()] to rearrange one vector, X, to mimic the spectrum of another vector; [spectrum()] for the hard work.
#' @examples
#'
#'## white noise
#'plot_f(z=runif(50))
#'
#' plot_f
plot_f <- function(z){
## plotting function, which also estimates of the slope of
## log(amplitude) - log(freq)
##
n <- length(z)
z1 <- z[1:(n/2)][-1]
spz <- stats::spectrum(z1)
lsa <- log(spz$spec)
lf <- log(spz$freq)
m <- lm(lsa~lf)
par(mfrow=c(1,2))
plot(1:n, z, type='l')
plot(lf, lsa)
abline(m)
confint(m)[2,]
}
