#' Creates an entry with number of unique observations
#' 
#' @param out empty vector 
#' @param x variable for row in table
#' @param y grouping variable
#' @param dt data table
#' @param xlab Label for entry in table
#' 
#' @details 
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:


n_unique <- function(out,
                     x, 
                     y, 
                     dt, 
                     xlab = NULL){
  dt1 <- eval(substitute(dt[,.(unique(x), N = "N"), y][,table(N,y, useNA = "always")]))
  dimt <- dim(dt1)
  M <- dt1[-dimt[1],-dimt[2], drop = FALSE]
  dimm <- dim(M)
  
  addout <- get_out(2, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
  dima <- dim(addout)
  addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
  addout[2,1:dimm[2] + 2] <- M[1,]
  addout[2,1] <- if(!is.null(xlab)){xlab}else{eval(substitute(ahsqc_label(dt[,.(x)][[1]])))}
  
  addout[1, (dima[2] - dimm[2] + 1):dima[2]] <- "Missing: " %|% dimnames(M)[[2]]
  addout[2, (dima[2] - dimm[2] + 1):dima[2]] <- dt1[dimt[1],-dimt[2]]
  addout[2,2] <- "N"
  
  for(j in 2:dimm[2]){
    addout[1, 2 + dimm[2] + j - 1] <- "p-value: " %|%
      addout[1, 2 + 1] %|% " vs " %|%  addout[1, 2 + j]
  }
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}