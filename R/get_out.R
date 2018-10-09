#' Creates an empty data frame
#' 
#' @param nrow integer, number of rows
#' @param ncol integer, number of columns
#' @details Creates an empty data frame of nrows and ncols
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' get_out(3,4)

get_out <- function(nrow,ncol){
  as.data.frame(array("",dim=c(nrow,ncol)), stringsAsFactors = FALSE)
}
