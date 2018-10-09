#' Creates a label or shows label
#' 
#' @param x data vector
#' @param value string, label value. If left NULL, returns label
#' @details Creates an empty data frame of nrows and ncols
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:

ahsqc_label <- function(x, value = NULL){
  if(!is.null(value)){
    attributes(x)$label <- value
    return(x)
  } 
  if(is.null(value)){
    out <- attributes(x)$label
    if(is.null(out)) out <- ""
    return(out)
  }
}
