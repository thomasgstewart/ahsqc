#' Format 2x2 table for html
#'
#' @param M a 2x2 matrix
#' @param fmt string that represents a format
#' @details 
#'
#'
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' data(iris)
#' Mat <- table(iris$Species == "setosa", iris$Petal.Width > 1)
#' formatpct(M = mat)

formatpct <- function(M, fmt = "%1.0f (%s)%s"){
  dm <- dim(M)
  CP <- array(NA_character_, dm)
  cs <- colSums(M)
  for(i in 1:dm[1]){
    for(j in 1:dm[2]){
      if(cs[j]>0){
        rn <- as.character(round(M[i,j]/cs[j]*100))
      }else{
        rn <- "NA"
      }
      if(rn=="0" & M[i,j] > 0) rn <- "<1"
      if(rn=="100" & M[i,j] != cs[j]) rn <- ">99"
      pad <- paste(rep("&nbsp;", 3-nchar(rn)), collapse="")
      CP[i,j] <- sprintf(fmt, M[i,j], rn, pad)
    }
  }
  return(CP)
}
