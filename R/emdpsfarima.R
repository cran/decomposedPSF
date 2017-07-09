#' Function to predict with EMD-PSF,ARIMA model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EMD-PSF,ARIMA model
#' @import Rlibeemd
#' @import forecast
#' @import tseries
#' @export
#' @examples
#' # emdpsfarima(data = nottem, n.ahead = 6)


emdpsfarima <- function(data, n.ahead)
{
  options(warn=-1)
  a <- emd(data)
  #b <- eemd(data)
  a_imf <- ncol(a)
  #b_imf <- ncol(b)
  #=====================================================
  #EMD-PSF,ARIMA
  #=====================================================
  xn <- NULL
  yn <- 0
  #a <- emd(data)
  for(i in 1:a_imf)
  {
    if (kpss.test(a[,i], null = "Trend")$p.value == 0.1)
    {
      xn[[i]] <- lpsf(data = a[,i], n.ahead = n.ahead)
    }
    else
    {
      dummy <- forecast(auto.arima(a[,i]), n.ahead)$mean
      xn[[i]] <- as.numeric(dummy)
    }

    yn <- yn + xn[[i]]
  }
  emd_psf_ar <- yn
  #=====================================================
  return(emd_psf_ar)
}
