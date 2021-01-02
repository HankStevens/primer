#' Function to generate 1/f noise.
#'
#' Generates 1/f noise with a specified power or amplitude.
#' @param gamma spectral power, numeric, where 0 generates a white noise time series, 2 generates reddened noise. Defaults to 1 (pink).
#' @param N length of the time series.
#' @keywords noise 1/f color spectra
#' @export
#' @author Hank Stevens
#' @references
#' J. M. Halley. Ecology, evolution and 1/f-noise. Trends in Ecology & Evolution, 11:33-37, 1996.
#' O. L. Petchey, A. Gonzalez, and H. B. Wilson. Effects on population persistence: the interaction between environmental noise colour, intra-specific competition and space. Proceedings of the Royal Society of London Series B, 264:1841-1847, 1997.
#' J. E. Cohen, C. M. Newman, A. E. Cohen, O. L. Petchey, and A. Gonzalez. Spectral mimicry: a method of synthesizing matching time series with different Fourier spectra. Circuits, Systems and Signal Processing, 18:431-442, 1999.
#' @seealso
#' [spec_mimic()] to rearrange one vector, X, to mimic the spectrum of another vector, Y; [plot_f()] to plot the time series and the spectrogram of the series.
#' @keywords 1/f color noise spectra
#' @examples
#'
#' set.seed(1)
#' time.series <- one_over_f(gamma=2, N=50)
#' plot(1:50, time.series, type='l', main="Reddened noise")
#' time.series <- one_over_f(gamma=0, N=50)
#' plot(1:50, time.series, type='l', main="White noise")
#' one_over_f()
#'
#' @export one_over_f
one_over_f <- function(gamma=1, N=200){
  ## Generate 1/f noise with power = gamma
  ## after Petchey SAS code, etc.
  N.2 <- N/2
  sine.waves <- matrix(NA, nrow=N, ncol=N.2)
  steps=2*pi*(1:N)/N
  phase <- stats::runif(N.2, 0, 2*pi)
  for(i in 1:N.2) {
    freq <- i
    weight <- 1/(freq^gamma)
    y <- weight*sin(freq*steps+phase[i])
    sine.waves[,i] <- y

  }
  my.series <- rowSums(sine.waves)
  return(my.series)
}
