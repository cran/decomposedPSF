#' Function to restrict the legth of dataset in multiples of 24
#'
#' @param data as inpute time series
#' @param n.ahead as horizon of values to be predicted
#' @import PSF
#' @import forecast
#' @importFrom stats predict
#' @return returns the predictied results
#' @export


lpsf <- function(data, n.ahead)
{
  options(warn=-1)
  x <- data
  xn <- length(x)
  y <- xn%%24
  x1 <- x[y:xn]
  tryCatch({x2 <- psf(data = x1, cycle =24)
  x2 <- predict(object = x2, n.ahead = n.ahead)
  return(x2)},

  error = function(e) {
    x2 <- forecast(auto.arima(data), n.ahead)
    x2 <- x2$mean
    x3 <- as.numeric(x2)
    return(x3)
  })
  # return(x2)
}
