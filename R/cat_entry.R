#' Creates a categorical entry for a table
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

cat_entry <- function(
  out
  , x
  , y 
  , dt 
  , xlab = NULL
  , pvalue = TRUE
  , fmt = "norm_fmt"
  , pvalue_fmt = function(x, test_method){
    formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
  }
){
  if(fmt == "norm_fmt") fmt <- "%1.0f (%s)%s"
  if(fmt == "count_fmt" ) fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
  
  d2 <- eval(substitute(dt[,.(x,y)]))
  tbl <- table(d2[[1]], d2[[2]], useNA = "always")
  dimt <- dim(tbl)
  M <- tbl[-dimt[1], -dimt[2]] %>% as.data.frame %>% t
  
  dimm <- dimt-1
  addout <- get_out(dimt[1]+1, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
  dima <- dim(addout)
  addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
  addout[-c(1:2), 1:dimm[2] + 2] <- formatpct(M, fmt)
  addout[2, 1] <- if(is.null(xlab)){ahsqc_label(d2[[1]])}else{xlab}
  addout[2, 2] <- "N (%)"
  addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
  
  miss <- formatpct(rbind(colSums(M),tbl[dimt[1],-dimt[2]]))
  addout[1, (dima[2] - dimm[2]+1):dima[2]] <- "Missing: " %|% dimnames(M)[[2]]
  addout[2, (dima[2] - dimm[2]+1):dima[2]] <- miss[2,]
  
  for(j in 2:dimm[2]){
    M_compare <- M[,c(1,j)]
    addout[1, dimt[2] + j] <- "p-value: " %|%
      dimnames(M)[[2]][1] %|% " vs " %|% dimnames(M)[[2]][j]
    if(sum(M_compare)==0 | !pvalue) next
    E_compare <- rowSums(M_compare) %*% t(colSums(M_compare)) / sum(M_compare)
    smallest_expected_cell <- min(E_compare)
    
    if(smallest_expected_cell >= 1 | sum(M_compare)>2000){
      withCallingHandlers(cst <- chisq.test(M_compare, correct = FALSE), warning = chi_approx)
      stat <- cst$statistic * (sum(M_compare) - 1)/sum(M_compare)
      pval <- pchisq(stat, cst$parameter, lower.tail = FALSE)
      test_method <- "EP"
    }else{
      pval <- fisher.test(M_compare)$p.value
      test_method <- "FE"
    }
    
    addout[2, dimt[2] + j] <- pvalue_fmt(pval, test_method)
  }
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}