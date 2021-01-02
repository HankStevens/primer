#' Function to mimic the power spectrum of an observed time series.
#'
#' This function rearranges one vector, X, to mimic the spectrum of another
#' vector, Y.
#'
#' If Y is NULL, this function will use [one_over_f()] to generate a random
#' series with power = gamma.
#'
#' @param X numeric vector.
#' @param Y if not NULL, a numeric vector.
#' @param gamma power of the 1/f noise; used only if Y is NULL.
#' @author Hank Stevens
#' @seealso [one_over_f()] to generate 1/f noise; [plot_f()] to plot the time
#' series and the spectrogram of the series.
#' @references J. M. Halley. Ecology, evolution and 1/f-noise. Trends in
#' Ecology & Evolution, 11:33-37, 1996. O. L. Petchey, A. Gonzalez, and H. B.
#' Wilson. Effects on population persistence: the interaction between
#' environmental noise colour, intra-specific competition and space.
#' Proceedings of the Royal Society of London Series B, 264:1841-1847, 1997. J.
#' E. Cohen, C. M. Newman, A. E. Cohen, O. L. Petchey, and A. Gonzalez.
#' Spectral mimicry: a method of synthesizing matching time series with
#' different Fourier spectra. Circuits, Systems and Signal Processing,
#' 18:431-442, 1999.
#' @keywords 1/f color mimicry noise spectra
#' @export
#' @examples
#' N = 50
#' set.seed(1)
#' X1 <- runif(N)
#' Y <- one_over_f(gamma=2, N=N)
#' X2 <- spec_mimic(X1, Y)
#' series <- cbind(X1, Y, X2)
#' matplot(1:50, series, type='l')
#' legend("bottomright", legend=colnames(series), col=1:3, lty=1:3)
#'
spec_mimic <- function(X, Y=NULL, gamma=1){
  ## Based on J. E. Cohen, C. M. Newman, A. E. Cohen, O. L. Petchey, and A. Gonzalez. Spectral mimicry: a method of synthesizing matching time series with different Fourier spectra. Circuits, Systems and Signal Processing, 18:431-442, 1999.

  ## X is the raw data that we want to rearrange to match some
  ## particular spectrum

  ## Y is the time series whose spectrum we would like to mimic.

  ## gamma is the power of a simulated spectrum.
  ## If we do not specify Y, then this function will generate a random
  ## time series with with 1/f noise with this power.

  if(!is.null(Y)) {
    Z <- sort(X)[rank(Y)]
  } else {
    n <- length(X)
    Y2 <- one_over_f(N=n, gamma = gamma)
    Z <- sort(X)[rank(Y2)]
  }
  return(Z)

}

