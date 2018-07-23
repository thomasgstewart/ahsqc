#' Generate standard AHSQC tables
#'
#' Generates standard AHSQC tables in HTML for .Rmd files. \code{generate_standard_tables}
#' will print standard
#' tables with no changes by default. If changes to individual tables are desired, they
#' can be made with \code{get_standard_table} using separate .R files in the working directory.
#' @param changes a boolean. Set to TRUE to indicate that changes have been made to at least one table.
#' @param y a character string to indicate group variable for tables.
#' @param data a data table.
#' @param format a string that describes the output format. Currently only "shiny" or
#'  "rmd". Default is "rmd"
#' @param module a string that indicates which data module, ventral or inguinal, is used to create
#' tables. Set to "ventral" for ventral module and "inguinal" for inguinal table. Once cannot
#' generate tables for ventral and inguinal modules together.
#' @param pvalue a boolean. Set to TRUE if pvalues are desired for all tables.
#' @details Function returns the standard set of tables for the AHSQC in HTML code.
#'
#' If \code{changes = TRUE}, the code for each table should be in individual .R files
#' with the \code{tbl}n\code{.R} nomenclature (ex. tbl1.R, tbl2.R, etc.) should be stored in the
#' working directory.  The function \code{get_standard_table} will do this.
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' # d0 <- ahs_get_data()
#' # d1 <- d0[["analyticview"]]
#' # generate_standard_tables(dt = d1, y = "e_gender")

generate_standard_tables <- function(
  changes = FALSE
  , y
  , data
  , format = "rmd"
  , module = "ventral"
  , pvalue = FALSE
){

  ##########################################
  ## add helper functions
  `%ni%` <- function(a,b){!(a %in% b)}
  # --- %|%
  `%|%` <- function(a,b) paste0(a,b)

  # --- varlabify
  varlabify <- function(df){
    for(i in seq_along(df)){
      if(label(df[[i]]) == ""){
        label(df[[i]]) <- names(df)[i]
      }
    }
    return(df)
  }

  # --- `label<-`
  `label<-` <- function(x, value){attributes(x)$label <- value; x}

  # --- label
  label <- function(x){
    out <- attributes(x)$label
    if(is.null(out)) out <- ""
    out
  }
  # --- garbage_pvalue
  garbage_pvalue <- function(x, test_method){ "NV: " %|% formatp(x, digits = 3) %|%
      "<sup>" %|% test_method %|% "</sup>" }
  # --- empty_pvalue
  empty_pvalue <- function(x, test_method){""}
  # ---  count_fmt
  count_fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"

  # --- chi_approx
  chi_approx <- function(w){
    if(any(grepl( "Chi-squared approximation may be incorrect", w))){
      invokeRestart("muffleWarning")
    }
  }

  # --- get_out
  get_out <- function(nrow,ncol){
    as.data.frame(array("",dim=c(nrow,ncol)), stringsAsFactors = FALSE)
  }

  # --- formatpct
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
  # ----- formatp
  formatp <- function (x, digits = 3, sig = 0.05, sig_marker = c("", ""))
  {
    p <- ifelse(is.na(x), NA, ifelse(round(x, digits) < 1/(10^digits),
                                     "< " %|% sprintf("%4." %|%
                                                        digits %|% "f", 1/(10^digits)),
                                     sprintf("%4." %|% digits %|% "f", x)))
    p <- ifelse(is.na(x), NA, ifelse(x < sig, sig_marker[1] %|%
                                       p %|% sig_marker[2], p))
    return(p)
  }
  # ---- samena
  samena <- function(x,y){
    x[is.na(y)] <- NA
    x
  }

  # ---- nazero
  nazero <- function(x){
    if(is.factor(x)){
      lx <- levels(x)
      if(!"0" %in% lx) stop("0 is not a factor level of a variable processed by nazero")
      out <- as.character(x) %>%
        (function(x) ifelse(is.na(x), "0", x)) %>%
        factor(lx)
      return(out)
    }
    if(is.numeric(x)) return(ifelse(is.na(x),0,x))
  }

  y_in <- rlang::sym(y)
  #y_in <- dt[[y]]
  #y_text <- y
  cat_entry <- function(
    out
    , x
    , y = y_in
    , dt = data
    , xlab = NULL
    , pvalue = FALSE
    , fmt = "%1.0f (%s)%s"
    , pvalue_fmt = function(x, test_method){
      formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
    }
  ){
    #browser()
    y = y_in
    d2 <- eval(substitute(dt[,.(x,y)]))
    tbl <- table(d2[[1]], d2[[2]], useNA = "always")
    #browser()
    dimt <- dim(tbl)
    M <- tbl[-dimt[1], -dimt[2]]

    dimm <- dimt-1
    addout <- get_out(dimt[1]+1, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
    dima <- dim(addout)
    addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
    addout[-c(1:2), 1:dimm[2] + 2] <- formatpct(M, fmt)
    addout[2, 1] <- if(is.null(xlab)){label(d2[[1]])}else{xlab}
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

  cat_entry_ <- function(
    out
    , x
    , y = y
    , dt = data
    , xlab = NULL
    , pvalue = FALSE
    , fmt = "%1.0f (%s)%s"
    , pvalue_fmt = function(x, test_method){
      formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
    }
  ){

    d2 <- dt[, c(x,y), with = FALSE]
    tbl <- table(d2[[1]], d2[[2]], useNA = "always")
    dimt <- dim(tbl)
    M <- tbl[-dimt[1], -dimt[2]]
    dimm <- dimt-1

    addout <- get_out(dimt[1]+1, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
    dima <- dim(addout)
    addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
    addout[-c(1:2), 1:dimm[2] + 2] <- formatpct(M, fmt)
    addout[2, 1] <- if(is.null(xlab)){label(d2[[1]])}else{xlab}
    addout[2, 2] <- "N (%)"
    addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]

    miss <- formatpct(rbind(colSums(M),tbl[dimt[1],-dimt[2]]))
    addout[1, (dima[2] - dimm[2]+1):dima[2]] <- "Missing: " %|% dimnames(M)[[2]]
    addout[2, (dima[2] - dimm[2]+1):dima[2]] <- miss[2,]

    for(j in 2:dimm[2]){
      addout[1, dimt[2] + j] <- "p-value: " %|%
        dimnames(M)[[2]][1] %|% " vs " %|% dimnames(M)[[2]][j]
      M_compare <- M[,c(1,j)]
      if(sum(M_compare)==0 | !pvalue) next
      E_compare <- rowSums(M_compare) %*% t(colSums(M_compare)) / sum(M_compare)
      smallest_expected_cell <- min(E_compare)

      if(smallest_expected_cell >= 1){
        withCallingHandlers(cst <- chisq.test(M_compare, correct = FALSE),
                            warning = chi_approx)
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

  n_unique <- function(out, x, y = y_in, dt = data, xlab = NULL){
    #y = y_in
    #browser()
    dt1 <- eval(substitute(dt[,.(unique(x), N = "N"), y][,table(N,y, useNA = "always")]))
    dimt <- dim(dt1)
    M <- dt1[-dimt[1],-dimt[2], drop = FALSE]
    dimm <- dim(M)

    addout <- get_out(2, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
    dima <- dim(addout)
    addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
    addout[2,1:dimm[2] + 2] <- M[1,]
    addout[2,1] <- if(!is.null(xlab)){xlab}else{eval(substitute(label(dt[,.(x)][[1]])))}

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


  cont_entry <- function(out, x, y = y_in, dt = data, xlab = NULL,
                         pvalue_fmt = function(x, test_method) {
                           formatp(x, digits = 3) %|% "<sup>" %|% test_method %|%
                             "</sup>"
                         }) {
    y = y_in
    d1 <- eval(substitute(dt[, .(x, y)]))
    d2 <- d1[complete.cases(d1)]
    #browser()
    d3 <- eval(substitute(d2[, .(N = .N, Mean = as.numeric(mean(x)),
                                 SD = sd(x), Q1 = as.numeric(quantile(x, 0.25)), Median = as.numeric(median(x)),
                                 Q3 = as.numeric(quantile(x, 0.75))), y] %>% arrange(y)))

    e1 <- eval(substitute(d2[,table(y)]))
    empty_idx <- which(e1 == 0)
    empty_names <- names(e1)[empty_idx]
    forder <- data.table(V1 = names(e1), V2 = 1:length(e1))
    e2 <- d3[rep(1,sum(e1==0))]
    e2[] <- NA
    eval(substitute(e2[,y := empty_names]))
    e2[,N := 0]

    if(dim(e2)[1] > 0){
      for(i in 2:ncol(d3)){
        d3[[i]] <- as.numeric(d3[[i]])
        e2[[i]] <- as.numeric(e2[[i]])
      }
      e3 <- rbind(d3,e2)
      e4 <- merge(e3, forder, by.x = names(e3)[1], by.y = "V1")
      d3 <- e4[order(V2)][, V2 := NULL]
    }
    dimt <- dim(d3)
    addout <- get_out(dimt[2] + 1, 2 + dimt[1] + dimt[1] -
                        1 + dimt[1])
    dima <- dim(addout)
    addout[1, 1:dimt[1] + 2] <- d3[[1]]
    addout[1:(dimt[2] - 1) + 2, 1:dimt[1] + 2] <- round(t(as.matrix(d3[,
                                                                       -1, with = FALSE])))
    addout[2, 1] <- if (!is.null(xlab)) {
      xlab
    }
    else {
      eval(substitute(label(dt[, .(x)][[1]])))
    }
    miss <- table(factor(1 * is.na(d1[[1]]), 0:1, 0:1), d1[[2]])
    addout[1, (dima[2] - dimt[1] + 1):(dima[2])] <- "Missing: " %|%
      dimnames(miss)[[2]]
    addout[2, (dima[2] - dimt[1] + 1):(dima[2])] <- formatpct(miss)[2,
                                                                    ]
    addout[1:(dimt[2] - 1) + 2, 2] <- names(d3)[-1]

    for(j in 2:dimt[1]){
      holdin <- d3[[1]][c(1,j)]
      d4 <- eval(substitute(d2 %>% filter(y %in% holdin)))
      wt1 <- eval(substitute(wilcox.test(x ~ y, data = d4)))
      addout[2, 2 + dimt[1] + j - 1] <- pvalue_fmt(wt1$p.value, test_method = "WR")
      addout[1, 2 + dimt[1] + j - 1] <- "p-value: " %|%
        addout[1, 2 + 1] %|% " vs " %|%  addout[1, 2 + j]
    }


    if (length(out) > 0)
      addout <- addout[-1, ]
    out[[length(out) + 1]] <- addout
    return(out)
  }


  binary_entry <- function(
    out
    , x
    , y = y_in
    , dt = data
    , xlab = NULL
    , level = c("Yes", "1")
    , pvalue = FALSE
    , fmt = "%1.0f (%s)%s"
    , pvalue_fmt = function(x, test_method){
      formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
    }
  ){
    y = y_in
    # browser()
    cat <- eval(substitute(cat_entry(
      list()
      , x
      , y
      , dt
      , xlab
      , pvalue = pvalue
      , fmt = fmt
      , pvalue_fmt = pvalue_fmt
    )))

    ny <- length(eval(substitute(dt[,table(y)])))
    count_cols <- 1:ny + 2
    matches <- cat[[1]][-c(1:2),1] %in% c("@@" %|% level)
    row <- if(sum(matches)==1){ which(matches) }else{1}
    cat[[1]][2,count_cols] <- cat[[1]][2 + row, count_cols]
    addout <- cat[[1]][1:2,]
    addout[2,2] <- ""

    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }

  binary_entry_ <- function(
    out
    , x
    , y = y
    , dt = data
    , xlab = NULL
    , level = c("Yes", "1")
    , pvalue = FALSE
    , fmt = "%1.0f (%s)%s"
    , pvalue_fmt = function(x, test_method){
      formatp(x, digits = 3) %|% "<sup>" %|% test_method %|% "</sup>"
    }
  ){

    cat <- cat_entry_(list(),x,y,dt,xlab,pvalue = pvalue, fmt = fmt, pvalue_fmt = pvalue_fmt)
    ny <- length(unique(dt[[y]]))
    count_cols <- 1:ny + 2
    matches <- cat[[1]][-c(1:2),1] %in% c("@@" %|% level)
    row <- if(sum(matches)==1){ which(matches) }else{1}
    cat[[1]][2,count_cols] <- cat[[1]][2 + row, count_cols]
    addout <- cat[[1]][1:2,]
    addout[2,2] <- ""

    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }

  empty_entry <- function(
    out
    , y = y_in
    , dt = data
    , fill = ""){
    y = y_in
    d2 <- eval(substitute(dt[,.(j = 1, y)]))
    label(d2[[1]]) <- "J"
    d2[[1]][1] <- 0
    addout <- eval(substitute(cat_entry(list(),j,y, d2)))[[1]][1:2, ]
    addout[2,] <- ""
    for(j in 1:min(length(fill),ncol(addout))) addout[2, j] <- fill[j]

    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }




  ##########################################
  ## start function coding here
  if(missing(y)) stop("Provide a y variable")
  if(missing(data)) stop("Provide a data table")
  if(is.character(data)) stop("data should be a data table, not a character string")
  if(format %ni% c("shiny","rmd")) stop("format must be either \"shiny\" or \"rmd\"")
  if(module %ni% c("ventral","inguinal")) stop("module must be either \"ventral\" or \"inguinal\" ")
  #dt <- deparse(substitute(data))

  if(module == "ventral"){
    if(changes == TRUE){
      for(table in 1:9){
        if(paste0("tbl",table,".R") %in% list.files()){
          source(paste0("tbl",table,".R"))
        } else{
          assign(paste0("tbl",table),
                 eval(parse(text = get_standard_table(table, data = data, print=TRUE, pval = pvalue))))
        }
      }
    } else {
      for(table in 1:9){
        assign(paste0("tbl",table),
               eval(parse(text = get_standard_table(table, data = data, print=TRUE, pval = pvalue))))    }
    }

    if(format %in% "shiny"){
      return_list <- list()
      for(tbl in 1:9){
        file <- get(paste0("tbl" ,tbl))
        tbln <- file
        title <- attr(tbln, "title")
        names(tbln) <- tbln[1,]
        tbln[,1] <- gsub("@@","&nbsp;&nbsp;&nbsp;", tbln[,1])

        ncols <- ncol(tbln)
        align <- c("l",rep("r", ncols - 1))
        return_list[tbl] <- kable(
          tbln[-1,]
          , align = align
          , format = "html"
          , row.names = FALSE
          , table.attr = "class=\"table table-condensed\""
          , escape = FALSE
          , caption = paste0("Table",tbl,": ", title)
        ) %>%
          toString

      }
      return(return_list)
    }

    if(format %in% "rmd"){
      for (tbl in 1:9) {
        file <- get(paste0("tbl", tbl))
        tbln <- file
        cat("\n### Table " %|% tbl %|% ": " %|% attr(tbln, "title") %|%
              "\n\n")
        names(tbln) <- tbln[1, ]
        tbln[, 1] <- gsub("@@", "&nbsp;&nbsp;&nbsp;", tbln[,
                                                           1])
        ncols <- ncol(tbln)
        align <- c("l", rep("r", ncols - 1))
        kable(tbln[-1, ], align = align, format = "html", row.names = FALSE,
              table.attr = "class=\"table table-condensed\"", escape = FALSE) %>%
          print
      }
    }
  }

  if(module == "inguinal"){
    if(changes == TRUE){
      for(table in 1:9){
        if(paste0("ing_tbl",table,".R") %in% list.files()){
          source(paste0("ing_tbl",table,".R"))
        } else{
          assign(paste0("ing_tbl",table),
                 eval(parse(text = get_standard_table_inguinal(table, data = data, print=TRUE, pval = pvalue))))
        }
      }
    } else {
      for(table in 1:9){
        assign(paste0("ing_tbl",table),
               eval(parse(text = get_standard_table_inguinal(table, data = data, print=TRUE, pval = pvalue))))    }
    }

    if(format %in% "shiny"){
      return_list <- list()
      for(tbl in 1:9){
        file <- get(paste0("ing_tbl" ,tbl))
        tbln <- file
        title <- attr(tbln, "title")
        names(tbln) <- tbln[1,]
        tbln[,1] <- gsub("@@","&nbsp;&nbsp;&nbsp;", tbln[,1])

        ncols <- ncol(tbln)
        align <- c("l",rep("r", ncols - 1))
        return_list[tbl] <- kable(
          tbln[-1,]
          , align = align
          , format = "html"
          , row.names = FALSE
          , table.attr = "class=\"table table-condensed\""
          , escape = FALSE
          , caption = paste0("Table",tbl,": ", title)
        ) %>%
          toString

      }
      return(return_list)
    }

    if(format %in% "rmd"){
      for (tbl in 1:9) {
        file <- get(paste0("ing_tbl", tbl))
        tbln <- file
        cat("\n### Table " %|% tbl %|% ": " %|% attr(tbln, "title") %|%
              "\n\n")
        names(tbln) <- tbln[1, ]
        tbln[, 1] <- gsub("@@", "&nbsp;&nbsp;&nbsp;", tbln[,
                                                           1])
        ncols <- ncol(tbln)
        align <- c("l", rep("r", ncols - 1))
        kable(tbln[-1, ], align = align, format = "html", row.names = FALSE,
              table.attr = "class=\"table table-condensed\"", escape = FALSE) %>%
          print
      }
    }
  }

}
