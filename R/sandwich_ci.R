#' Takes a model fit and returns a data frame with variable, estimate, CI, and N
#'
#' @details Returns a dataframe with variable, estimate, and CI information from model fit
#' @param fit model object
#' @param antilog boolean, set to `FALSE` for log scale or `TRUE` for antilog scale
#' @param data dataframe or datatable
#' @param vnames character, set to `"labels"` to use variable labels rather than variable names in the output
#' @param outcome character, outcome variable name
#' @param exposure character, exposure variable name
#' @keywords AHSQC, sandwich
#' @export
#' @examples
#' # Not run:
#' # sandwich_ci(mod_obj, antilog = T, data = df, vnames = "labels")
#' # sandwich_ci(mod_obj, antilog = T, data = df, vnames = "labels", outcome = "flg_cmp_postop_ssi", exposure = "initial_approach")


sandwich_ci <- function(fit, 
                        antilog = FALSE, 
                        data, 
                        vnames = "names", 
                        outcome = NULL, 
                        exposure = NULL){
  require(sandwich)
  
  if(missing(fit)) stop("must provide object fit")
  if(missing(data)) stop("must provide data frame or table")
  if(is.null(outcome) & !is.null(exposure)) stop("Please supply outcome variable")
  if(is.null(exposure) & !is.null(outcome)) stop("Please supply expsoure variable")
  
  
  est <- coef(fit)[!grepl("y>", names(coef(fit)))] 
  se <- sqrt(diag(fit[["var"]]))[!grepl("y>", names(diag(fit[["var"]])))] 
  lb <- est - 1.96*se
  ub <- est + 1.96*se
  if(antilog){
    est <- exp(est)
    lb <- exp(lb)
    ub <- exp(ub)
  }
  
  getlab <- function(xx) {
    vn  <- gsub("(\\=*)\\=.*|\\'", "", xx)
    ll   <- label(data[[vn]])
    if(grepl("\\=", xx)) {
      subs <- gsub(".*\\=", paste0(ll, "="), xx)
    } else {
      subs <- gsub(vn, ll, xx)
    }
    return(ifelse(subs == "", xx, subs))
  }
  
  if(vnames %in% "labels") nms <- purrr::map(.x = names(coef(fit)), .f = getlab) %>% unlist
  if(vnames %in% "names") nms <- names(coef(fit))
  
  out <- data.frame(
    var = nms,
    est = round(est,2),
    lb = round(lb,2),
    ub = round(ub,2),
    ci = signif(est,3)  %|% " (" %|% signif(lb, 3) %|% ", " %|% signif(ub, 3) %|% ")",
    N = fit[["stats"]][1],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  if(!is.null(exposure)){
    rownum <- which(grepl(exposure, names(coef(fit))))
    out <- out[rownum,]
    if(vnames %in% "labels") out$var <- getlab(outcome)
    if(vnames %in% "names") out$var <- outcome
  }
  
  return(out)  
}



