#' Function to predict with EEMD-ARIMA model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EEMD-ARIMA model
#' @import Rlibeemd
#' @import forecast
#' @export
#' @examples
#' # eemdarima(data = nottem, n.ahead = 6)


eemdarima <- function(data, n.ahead)
{
  options(warn=-1)
  #a <- emd(data)
  b <- eemd(data)
  #a_imf <- ncol(a)
  b_imf <- ncol(b)
  #=====================================================
  #EMD-ARIMA
  #=====================================================
  x2 <- NULL
  y2 <- 0
  #a <- emd(data)
  for(i in 1:b_imf)
  {
    dummy <- forecast(auto.arima(b[,i]), n.ahead)$mean
    x2[[i]] <- as.numeric(dummy)
    y2 <- y2 + x2[[i]]
  }
  eemd_arima <- y2
  #=====================================================
  return(eemd_arima)
}
