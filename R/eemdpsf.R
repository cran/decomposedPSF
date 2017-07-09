#' Function to predict with EEMD-PSF model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EEMD-PSF model
#' @import Rlibeemd
#' @export
#' @examples
#' # eemdpsf(data = nottem, n.ahead = 6)



eemdpsf <- function(data, n.ahead)
{
  options(warn=-1)
  #a <- emd(data)
  b <- eemd(data)
  #a_imf <- ncol(a)
  b_imf <- ncol(b)
  #=====================================================
  #EEMD-PSF
  #=====================================================
  x <- NULL
  y <- 0
  #a <- emd(data)
  for(i in 1:b_imf)
  {
    #dummy <- psf(data = a[,i], cycle = 24)
    #x[[i]] <- predict(object = dummy, n.ahead = n.ahead)
    x[[i]] <- lpsf(data = b[,i], n.ahead = n.ahead)
    y <- y + x[[i]]
  }
  eemd_psf <- y
  #=====================================================
  return(eemd_psf)
}
