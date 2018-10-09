#' Creates a continuous entry for a table
#' 
#' @param out empty vector 
#' @param x variable for row in table
#' @param y grouping variable
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



cont_entry <- function(
  out
  , x
  , y 
  , dt  
  , xlab = NULL
  , pvalue = TRUE
  , pvalue_fmt = function(x, test_method){
    formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
  }
){
  d1 <- eval(substitute(dt[,.(x,y)])) ## mao: changed data[,.(x,y)] to dt[.(x,y)]
  d2 <- d1[complete.cases(d1)]
  d3 <- eval(substitute(
    d2[,.(N = .N, Mean = mean(x), SD = sd(x), Q1 = as.numeric(quantile(x, .25)), Median = as.numeric(median(x)), Q3 = as.numeric(quantile(x, .75))), y] %>%
      arrange(y)
  ))
  
  dimt <- dim(d3)
  addout <- get_out(dimt[2] + 1, 2 + dimt[1] + dimt[1] - 1 + dimt[1])
  dima <- dim(addout)
  addout[1,1:dimt[1] + 2] <- d3[[1]]
  addout[1:(dimt[2]-1) + 2, 1:dimt[1] + 2] <- round(t(as.matrix(d3[, -1, with=FALSE])))
  addout[2,1] <- if(!is.null(xlab)){xlab}else{eval(substitute(ahsqc_label(dt[,.(x)][[1]])))}
  
  miss <- table(factor(1*is.na(d1[[1]]), 0:1, 0:1),d1[[2]])
  addout[1,(dima[2] - dimt[1] + 1):(dima[2])] <- "Missing: " %|% dimnames(miss)[[2]]
  addout[2,(dima[2] - dimt[1] + 1):(dima[2])] <- formatpct(miss)[2,]
  
  addout[1:(dimt[2]-1) + 2, 2] <- names(d3)[-1]
  
  for(j in 2:dimt[1]){
    holdin <- d3[[1]][c(1,j)]
    d4 <- eval(substitute(d2 %>% filter(y %in% holdin)))
    wt1 <- eval(substitute(wilcox.test(x ~ y, data = d4)))
    if(pvalue){
      addout[2, 2 + dimt[1] + j - 1] <- pvalue_fmt(wt1$p.value, test_method = "WR")
      addout[1, 2 + dimt[1] + j - 1] <- "p-value: " %|%
      addout[1, 2 + 1] %|% " vs " %|%  addout[1, 2 + j]
    }
  }
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}