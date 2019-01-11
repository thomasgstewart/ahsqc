#' Creates an entry with number of unique observations
#' 
#' @param out empty vector 
#' @param x variable for row in table
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


n_unique_all <- function(out,
                     x, 
                     dt,  
                     xlab = NULL){
  browser()
  dt1 <- eval(substitute(dt[,.(unique(x), N = "N")][,table(N, useNA = "always")])) %>% 
    as.matrix %>% as.table()
  dimt <- dim(dt1)
  M <- dt1[-dimt[1], drop = FALSE] %>% as.matrix %>% as.table()
  dimm <- dim(M)
  
  addout <- get_out(2, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
  dima <- dim(addout)
  #addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
  addout[2,1 + 2] <- M[1,]
  addout[2,1] <- if(!is.null(xlab)){xlab}else{eval(substitute(ahsqc_label(dt[,.(x)][[1]])))}
  
  addout[1, (dima[2] - dimm[2] + 1):dima[2]] <- "Missing"
  addout[2, (dima[2] - dimm[2] + 1):dima[2]] <- dt1[dimt[1]]
  addout[2,2] <- "N"
  
  
  if(length(out)>0) addout <- addout[-1,]
  out[[length(out)+1]] <- addout
  return(out)
}