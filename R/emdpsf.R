#' Function to predict with EMD-PSF model
#'
#' @param data as input time series data
#' @param n.ahead as horizon of values to be predicted
#' @return predicted values with EMD-PSF model
#' @import Rlibeemd
#' @export
#' @examples
#' # emdpsf(data = nottem, n.ahead = 6)


emdpsf <- function(data, n.ahead)
{
  options(warn=-1)
  a <- emd(data)
  #b <- eemd(data)
  a_imf <- ncol(a)
  #b_imf <- ncol(b)
  #=====================================================
  #EMD-PSF
  #=====================================================
  x <- NULL
  y <- 0
  #a <- emd(data)
  for(i in 1:a_imf)
  {
    #dummy <- psf(data = a[,i], cycle = 24)
    #x[[i]] <- predict(object = dummy, n.ahead = n.ahead)
    x[[i]] <- lpsf(data = a[,i], n.ahead = n.ahead)
    y <- y + x[[i]]
  }
  emd_psf <- y
  #=====================================================
  return(emd_psf)
}
