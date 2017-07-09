#' Function to predict with EMD-ARIMA model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EMD-ARIMA model
#' @import Rlibeemd
#' @import forecast
#' @export
#' @examples
#' # emdarima(data = nottem, n.ahead = 6)


emdarima <- function(data, n.ahead)
{
  options(warn=-1)
  a <- emd(data)
  #b <- eemd(data)
  a_imf <- ncol(a)
  #b_imf <- ncol(b)
  #=====================================================
  #EMD-ARIMA
  #=====================================================
  x2 <- NULL
  y2 <- 0
  #a <- emd(data)
  for(i in 1:a_imf)
  {
    dummy <- forecast(auto.arima(a[,i]), n.ahead)$mean
    x2[[i]] <- as.numeric(dummy)
    y2 <- y2 + x2[[i]]
  }
  emd_arima <- y2
  #=====================================================
  return(emd_arima)
}
