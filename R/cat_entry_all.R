#' Creates a categorical entry for a table
#' 
#' @param out empty vector 
#' @param x variable for row in table
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

cat_entry_all <- function(
  out
  , x
  , dt
  , xlab = NULL
  , pvalue = TRUE
  , fmt = "norm_fmt"
){
  if(fmt == "norm_fmt") fmt <- "%1.0f (%s)%s"
  if(fmt == "count_fmt" ) fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
  
  d2 <- eval(substitute(dt[,.(x)]))
  tbl <- table(d2[[1]], useNA = "always")
  dimt <- dim(tbl)
  M <- tbl[-dimt[1]] %>% as.matrix %>% as.table()
  
  dimm <- dimt-1
  addout <- get_out(dimt[1]+1, 2 + 1 + 1 - 1 + 1)
  dima <- dim(addout)
  addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
  addout[-c(1:2), 1:1 + 2] <- formatpct(M, fmt)
  addout[2, 1] <- if(is.null(xlab)){ahsqc_label(d2[[1]])}else{xlab}
  addout[2, 2] <- "N (%)"
  #addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
  
  miss <- formatpct(rbind(colSums(M),tbl[dimt[1]]))
  addout[1, (dima[2] - 1+1):dima[2]] <- "Missing" 
  addout[2, (dima[2] - 1+1):dima[2]] <- miss[2,]
  
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}
