#' Returns odds ratio of two variables from data frame
#'
#' @details Returns a dataframe with variable, estimate, and CI of odds ratio
#' @param v1 Character, variable 1 name
#' @param v2 Character, variable 2 name
#' @param data dataframe or datatable
#' @keywords AHSQC, odds ratio, OR
#' @export
#' @examples
#' # Not run:
#' # unadj_or("flg_cmp_postop_ssi", "e_gender", data = d1)


unadj_or <- function(v1, v2, d1){
  require(epitools)
  or <- oddsratio(table(d1[[v1]], d1[[v2]]) %>% as.matrix)$measure
  or <- or[2:nrow(or),] %>% as.matrix()
  out <- data.frame(
    var = label(d1[[v2]]),
    est = or[1,],
    lb = or[2,],
    ub = or[3,],
    ci = format(round(or[1,], 2), nsmall = 2) %|% 
      " (" %|% 
      format(round(or[2,],2), nsmall = 2) %|% 
      ", " %|% 
      format(round(or[3,], 2), nsmall = 2) %|%
      ")",
    N = sum(!is.na(d1[[v2]])),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  return(out)
}