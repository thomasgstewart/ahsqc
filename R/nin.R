#' %nin%
#'
#' Opposite of %in%
#'
#' @param x vector: the values to be matched
#' @param y vector, list, or data frame: the value to be matched against
#'
#' @keywords render
#' @return boolean
#' @export
#' @examples
#' # Not run:
#'
#'
nin <- function(x, table) !match(x, table, nomatch = 0L) > 0L
'%nin%' <- function(x,y) (nin(x,y))
