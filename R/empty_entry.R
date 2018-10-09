#' Creates an empty entry in table
#' 
#' @param out empty vector 
#' @param y grouping variable
#' @param dt data table
#' @param fill string
#' 
#' @details 
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:

empty_entry <- function(out, y , dt , fill = ""){
  browser()
  d2 <- eval(substitute(dt[,.(j = 1, y)]))
  d2[[1]] <- ahsqc_label(d2[[1]], "J")
  d2[[1]][1] <- 0
  addout <- eval(substitute(cat_entry(list(),j,y, d2)))[[1]][1:2, ]
  addout[2,] <- ""
  for(j in 1:min(length(fill),ncol(addout))) addout[2, j] <- fill[j]
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}