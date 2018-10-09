#' Returns empty string for pvalue
#' 
#' @param x A p-value
#' @param test_method A string, statistical test name
#' @details returns an empty string. Used for pvalue_fmt argument in binary_entry, cat_entry, etc. 
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' data(iris)
#' cat_entry(c(), x = species, y = ___, pvalue_fmt = empty_pvalue)

empty_pvalue <- function(x, test_method){
  ""
  }