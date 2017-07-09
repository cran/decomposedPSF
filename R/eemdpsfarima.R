#' Function to predict with EEMD-PSF,ARIMA model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EEMD-PSF,ARIMA model
#' @import Rlibeemd
#' @import forecast
#' @import tseries
#' @export
#' @examples
#' # eemdpsfarima(data = nottem, n.ahead = 6)


eemdpsfarima <- function(data, n.ahead)
{
  options(warn=-1)
  #a <- emd(data)
  b <- eemd(data)
  #a_imf <- ncol(a)
  b_imf <- ncol(b)
  #=====================================================
  #EEMD-PSF,ARIMA
  #=====================================================
  xn <- NULL
  yn <- 0
  #a <- emd(data)
  for(i in 1:b_imf)
  {
    if (kpss.test(b[,i], null = "Trend")$p.value == 0.1)
    {
      xn[[i]] <- lpsf(data = b[,i], n.ahead = n.ahead)
    }
    else
    {
      dummy <- forecast(auto.arima(b[,i]), n.ahead)$mean
      xn[[i]] <- as.numeric(dummy)
    }

    yn <- yn + xn[[i]]
  }
  eemd_psf_ar <- yn
  #=====================================================
  return(eemd_psf_ar)
}
