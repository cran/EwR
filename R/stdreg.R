#' Standardized Regression
#'
#' This function computes standardized regression model.
#' @param y series name,
#' @param x series name
#' @keywords Standardized Regression
#' @references
#' Selahattin Güriş, Ebru Çağlayan Akay, Burak Güriş, R ile Temel Ekonometri, DER Yayinevi, 2020.
#' @export
#' @importFrom stats lm sd
#' @examples
#' IHR = REcoData$IHR
#' ITH = REcoData$ITH
#' stdreg(IHR,ITH)
#'

stdreg <- function(y,x){
  yy = (y-mean(y))/sd(y)
  xx = (x-mean(x))/sd(x)
  model = lm(yy~xx-1)
  print(summary(model))

}

