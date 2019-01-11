#' Creates a continuous entry for a table
#' 
#' @param out empty vector 
#' @param x variable for row in table
#' @param dt data table
#' @param xlab Label for entry in table
#' @param pvalue Boolean
#' @param pvalue_fmt A function that takes a pvalue and a test method
#' 
#' @details 
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:



cont_entry_all <- function(
  out
  , x
  , dt
  , xlab = NULL
  , pvalue = TRUE
  , pvalue_fmt = function(x, test_method){
    formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
  }
){
  d1 <- eval(substitute(dt[,.(x)])) ## mao: changed data[,.(x,y)] to dt[.(x,y)]
  d2 <- d1[complete.cases(d1)]
  d3 <- eval(substitute(
    d2[,.(N = .N, Mean = mean(x), SD = sd(x), Q1 = as.numeric(quantile(x, .25)), Median = as.numeric(median(x)), Q3 = as.numeric(quantile(x, .75)))]
  )) %>% as.matrix %>% as.table()
  
  dimt <- dim(d3)
  addout <- get_out(dimt[2] + 1, 2 + dimt[1] + dimt[1] - 1 + dimt[1])
  dima <- dim(addout)
  addout[1,1:dimt[1] + 2] <- d3[[1]]
  addout[1:(dimt[2]-1) + 2, 1:dimt[1] + 2] <- round(t(as.matrix(d3[, -1])))[1,]
  addout[2,1] <- if(!is.null(xlab)){xlab}else{eval(substitute(ahsqc_label(dt[,.(x)][[1]])))}
  
  miss <- table(factor(1*is.na(d1[[1]]), 0:1, 0:1)) %>% as.matrix %>% as.table()
  addout[1,(dima[2] - dimt[1] + 1):(dima[2])] <- "Missing" 
  addout[2,(dima[2] - dimt[1] + 1):(dima[2])] <- formatpct(miss)[2,]
  
  addout[1:(dimt[2]-1) + 2, 2] <- colnames(d3)[-1]
  
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}