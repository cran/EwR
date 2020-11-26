#' Weighted Least Square
#'
#' This Function makes Weighted Least Square estimation.
#' @param y series name,
#' @param x series name
#' @keywords Weighted Least Square estimation
#' @references
#' Selahattin Güriş, Ebru Çağlayan Akay, Burak Güriş, R ile Temel Ekonometri, DER Yayinevi, 2020.
#' @export
#' @importFrom stats lm fitted
#' @examples
#' IHR = REcoData$IHR
#' ITH = REcoData$ITH
#' Wls(ITH,IHR)
#'


Wls <- function (y,x) {
  model1 = lm(y~x)
  fit1 = fitted(model1)
  sabit = rep(1,length(y))
  model2 = lm(I(y/fit1) ~ 0 + I(x/fit1) + I(sabit/fit1))
  print(model2)

}
