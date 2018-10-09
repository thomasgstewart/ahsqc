#' Creates a binary entry for a table
#' 
#' @param out empty vector 
#' @param x variable for row in table
#' @param y grouping variable
#' @param dt data table
#' @param xlab Label for entry in table
#' @param pvalue Boolean
#' @param fmt Two values are "norm_fmt" or "count_fmt"
#' @param pvalue_fmt A function that takes a pvalue and a test method
#' 
#' @details 
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:

binary_entry <- function(
  out
  , x
  , y = initial_approach
  , dt = data
  , xlab = NULL
  , level = c("Yes", "1")
  , pvalue = TRUE
  , fmt = "norm_fmt"
  , pvalue_fmt = function(x, test_method){
    formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
  }
){
  if(fmt == "norm_fmt") fmt <- "%1.0f (%s)%s"
  if(fmt == "count_fmt" ) fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
  cat <- eval(substitute(cat_entry(
    list()
    , x
    , y
    , dt
    , xlab
    , pvalue = pvalue
    , fmt = fmt
    , pvalue_fmt = pvalue_fmt
  )))
  ny <- eval(substitute(dt[,length(unique(y))]))
  count_cols <- 1:ny + 2
  matches <- cat[[1]][-c(1:2),1] %in% c("@@" %|% level)
  row <- if(sum(matches)==1){ which(matches) }else{1}
  cat[[1]][2,count_cols] <- cat[[1]][2 + row, count_cols]
  addout <- cat[[1]][1:2,]
  addout[2,2] <- ""
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}