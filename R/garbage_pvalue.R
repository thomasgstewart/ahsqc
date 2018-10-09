#' Returns pvalue format with NV specification
#' 
#' @param x A p-value
#' @param test_method A string, statistical test name
#' @details Returns a specific format. Used for pvalue_fmt argument in binary_entry, cat_entry, etc. 
#' NV	indicates: As the baseline demographic and pre-operative characteristics indicate, the patients in each repair group differ in terms of baseline disease severity and other characteristics associated with repair outcomes. In light of the imbalance, an unadjusted comparison between groups would not be interpretable as it would be confounded by factors related the surgeon’s choice of surgical approach. For this reason, post operative outcomes were not formally compared between groups in the unadjusted analysis.
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' data(iris)
#' cat_entry(c(), x = species, y = ___, pvalue_fmt = garbage_pvalue)

garbage_pvalue <- function(x, test_method){ "NV: " %|% formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>" }