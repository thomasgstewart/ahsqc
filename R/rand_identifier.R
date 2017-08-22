#' Map an ID variable to randomly generated IDs
#'
#' Generates a random ID variable based on a previously existing ID variable.
#' @param x A numeric or character vector; the ID variable
#' @param seed Seed for random sampling
#' @details Maps ID values to a random ordering of integers to generate the new ID. 
#' @keywords rand_identifier
#' @export
#' @examples
#' rand_identifier(LETTERS, 234)
#' rand_identifier(1:23, 345)

rand_identifier <- function(x, seed){
  if(sum(is.na(x))>0) error("Some IDs are NA")
  set.seed(seed)
  z <- factor(
    x,
    levels = sort(unique(x)), 
    labels = sample.int(length(unique(x)))
  )
  y <- as.numeric(as.character(z))
  return(y)
}
