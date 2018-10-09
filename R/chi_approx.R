#' Muffles warning for `chisq.test`
#'
#' @param w character string that may contain "Chi-squared approximation may be incorrect"
#' @details Use as argument for warning in `chisq.test` to muffle warning of "Chi-squared approximation may be incorrect"
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' data(iris)
#' Mat <- table(iris$Species == "setosa", iris$Petal.Width > 1)
#' chisq.test(Mat, correct = FALSE), warning = chi_approx)

chi_approx <- function(w){
  if(any(grepl( "Chi-squared approximation may be incorrect", w))){
    invokeRestart("muffleWarning")
  }
  if(any(grepl("cannot compute exact p-value with ties",w))){
    invokeRestart("muffleWarning")
  }
}
