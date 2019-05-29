#' Generate standard AHSQC tables
#'
#' Generates standard AHSQC tables in HTML for .Rmd files. \code{generate_standard_tables}
#' will print standard
#' tables with no changes by default. If changes to individual tables are desired, they
#' can be made with \code{get_standard_table} using separate .R files in the working directory.
#' @param changes a boolean. Set to TRUE to indicate that changes have been made to at least one table.
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

generate_standard_tables_all <- function(
  changes = FALSE
  , data
  , date
  , format = "rmd"
  , module = "ventral"
  , pvalue = FALSE
  , display = TRUE
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

  
  
  
  binary_entry_all <- function(
    out
    , x
    , dt = data
    , xlab = NULL
    , level = c("Yes", "1")
    , pvalue = TRUE
    , fmt = "norm_fmt"
  ){
    if(fmt == "norm_fmt") fmt <- "%1.0f (%s)%s"
    if(fmt == "count_fmt" ) fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
    cat <- eval(substitute(cat_entry_all(
      list()
      , x
      , dt
      , xlab
      , pvalue = pvalue
      , fmt = fmt
    )))
    count_cols <- 1:1 + 2
    matches <- cat[[1]][-c(1:2),1] %in% c("@@" %|% level)
    row <- if(sum(matches)==1){ which(matches) }else{1}
    cat[[1]][2,count_cols] <- cat[[1]][2 + row, count_cols]
    addout <- cat[[1]][1:2,]
    addout[2,2] <- ""
    
    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }
  
  
  
  cont_entry_all <- function(
    out
    , x
    , dt = data
    , xlab = NULL
    , pvalue = TRUE
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
                           dt = data,  
                           xlab = NULL){
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
  
  
  #' Creates a categorical entry for a table
  #' 
  #' @param out empty vector 
  #' @param x variable for row in table
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
  
  cat_entry_all <- function(
    out
    , x
    , dt = data
    , xlab = NULL
    , pvalue = TRUE
    , fmt = "norm_fmt"
  ){
    if(fmt == "norm_fmt") fmt <- "%1.0f (%s)%s"
    if(fmt == "count_fmt" ) fmt <- "%1.0f&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
    
    d2 <- eval(substitute(dt[,.(x)]))
    tbl <- table(d2[[1]], useNA = "always")
    dimt <- dim(tbl)
    M <- tbl[-dimt[1]] %>% as.matrix %>% as.table()
    
    dimm <- dimt-1
    addout <- get_out(dimt[1]+1, 2 + 1 + 1 - 1 + 1)
    dima <- dim(addout)
    addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
    addout[-c(1:2), 1:1 + 2] <- formatpct(M, fmt)
    addout[2, 1] <- if(is.null(xlab)){ahsqc_label(d2[[1]])}else{xlab}
    addout[2, 2] <- "N (%)"
    #addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]
    
    miss <- formatpct(rbind(colSums(M),tbl[dimt[1]]))
    addout[1, (dima[2] - 1+1):dima[2]] <- "Missing" 
    addout[2, (dima[2] - 1+1):dima[2]] <- miss[2,]
    
    
    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }
  
  
  #' Creates an empty entry in table
  #'
  #' @param out empty vector
  #' @param dt data table
  #' @param fill string
  #'
  #' @details
  #'
  #' @keywords AHSQC
  #' @export
  #' @examples
  #' # Not run:
  
  empty_entry_all <- function(out , dt = data, fill = ""){
    dt <- dt %>% mutate(y = "all")
    d2 <- eval(substitute(dt[,.(j = 1, y)]))
    d2[[1]] <- ahsqc_label(d2[[1]], "J")
    d2[[1]][1] <- 0
    addout <- eval(substitute(cat_entry_all(list(),j, d2)))[[1]][1:2, ]
    addout[2,] <- ""
    for(j in 1:min(length(fill),ncol(addout))) addout[2, j] <- fill[j]
    
    if(length(out)>0) addout <- addout[-1,]
    out[[length(out)+1]] <- addout
    return(out)
  }
  
  
  
  
  ##########################################
  ## start function coding here
  if(missing(date)) {
    stop("Must provide date of data pull")
  } else {
    date = as.Date(date)
  }
  if(missing(data)) stop("Provide a data table")
  if(is.character(data)) stop("data should be a data table, not a character string")
  if(format %ni% c("shiny","rmd")) stop("format must be either \"shiny\" or \"rmd\"")
  if(module %ni% c("ventral")) stop("module must be either \"ventral\". Inguinal currently not available.")
  #dt <- deparse(substitute(data))
  
  if(module == "ventral"){
    if(changes == TRUE){
      for(table in 1:9){
        if(paste0("tbl",table,".R") %in% list.files()){
          source(paste0("tbl",table,".R"), local = T)
        } else {
          if(as.Date(date) - as.Date("2019-04-02") < 0){
            assign(paste0("tbl",table),
                   eval(parse(text = get_standard_table_all(table, data = data, print=TRUE, pval = pvalue))))
          } else {
            assign(paste0("tbl",table),
                   eval(parse(text = get_standard_table_all2(table, data = data, print=TRUE, pval = pvalue))))
          }
        }
      }
    } else {
      for(table in 1:9){
        if(as.Date(date) - as.Date("2019-04-02") < 0){
          assign(paste0("tbl",table),
                 eval(parse(text = get_standard_table_all(table, data = data, print=TRUE, pval = pvalue))))    
        } else {
          assign(paste0("tbl",table),
                 eval(parse(text = get_standard_table_all2(table, data = data, print=TRUE, pval = pvalue))))  
        }
      }
    }
    
    if(format %in% "shiny"){
      return_list <- list()
      for(tbl in 1:9){
        file <- get(paste0("tbl" ,tbl))
        tbln <- file
        title <- attr(file, "title")
        names(tbln) <- tbln[1,]
        tbln[,1] <- gsub("@@","&nbsp;&nbsp;&nbsp;", tbln[,1])
        
        if(!pvalue){
          tbln <- tbln %>%
            `[`(,!lgrep(tbln %>% names, "p-value")) %>%
            rename(" " = ".1") %>%
            rename(" " = ".2")
        }
        
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
      out <- list()
      for (tbl in 1:9) {
        file <- get(paste0("tbl", tbl))
        tbln <- file
        names(tbln) <- tbln[1, ]
        tbln[, 1] <- gsub("@@", "&nbsp;&nbsp;&nbsp;", tbln[,
                                                           1])
        
        if(!pvalue){
          tbln <- tbln %>%
            `[`(,!lgrep(tbln %>% names, "p-value")) %>%
            rename(" " = ".1")%>%
            rename(" " = ".2")
        }
        
        
        ncols <- ncol(tbln)
        align <- c("l", rep("r", ncols - 1))
        if(display){
          cat("\n### Table " %|% tbl %|% ": " %|% attr(file, "title") %|%
                "\n\n")
          kable(tbln[-1, ], align = align, format = "html", row.names = FALSE,
                table.attr = "class=\"table table-condensed\"", escape = FALSE) %>%
            print
        } else{
          out[[tbl]] <- list()
          out[[tbl]][[1]] <- kable(tbln[-1, ], align = align, format = "html", 
                                   row.names = FALSE, table.attr = "class=\"table table-condensed\"", 
                                   escape = FALSE)
          out[[tbl]][[2]] <- attr(file, "title")
        }
      }
      return(invisible(out))
    }
  }
  
  # if(module == "inguinal"){
  #   if(changes == TRUE){
  #     for(table in 1:9){
  #       if(paste0("ing_tbl",table,".R") %in% list.files()){
  #         source(paste0("ing_tbl",table,".R"))
  #       } else{
  #         assign(paste0("ing_tbl",table),
  #                eval(parse(text = get_standard_table_inguinal(table, data = data, print=TRUE, pval = pvalue))))
  #       }
  #     }
  #   } else {
  #     for(table in 1:9){
  #       assign(paste0("ing_tbl",table),
  #              eval(parse(text = get_standard_table_inguinal(table, data = data, print=TRUE, pval = pvalue))))    }
  #   }
  #   
  #   if(format %in% "shiny"){
  #     return_list <- list()
  #     for(tbl in 1:9){
  #       file <- get(paste0("ing_tbl" ,tbl))
  #       tbln <- file
  #       title <- attr(tbln, "title")
  #       names(tbln) <- tbln[1,]
  #       tbln[,1] <- gsub("@@","&nbsp;&nbsp;&nbsp;", tbln[,1])
  #       
  #       ncols <- ncol(tbln)
  #       align <- c("l",rep("r", ncols - 1))
  #       
  #       if(!pvalue){
  #         tbln <- tbln %>%
  #           `[`(,!lgrep(tbln %>% names, "p-value")) %>%
  #           rename(" " = ".1")
  #       }
  #       
  #       
  #       return_list[tbl] <- kable(
  #         tbln[-1,]
  #         , align = align
  #         , format = "html"
  #         , row.names = FALSE
  #         , table.attr = "class=\"table table-condensed\""
  #         , escape = FALSE
  #         , caption = paste0("Table",tbl,": ", title)
  #       ) %>%
  #         toString
  #       
  #     }
  #     return(return_list)
  #   }
  #   if(format %in% "rmd"){
  #     for (tbl in 1:9) {
  #       file <- get(paste0("ing_tbl", tbl))
  #       tbln <- file
  #       cat("\n### Table " %|% tbl %|% ": " %|% attr(tbln, "title") %|%
  #             "\n\n")
  #       names(tbln) <- tbln[1, ]
  #       tbln <- tbln[-1,]
  #       tbln[, 1] <- gsub("@@", "&nbsp;&nbsp;&nbsp;", tbln[,
  #                                                          1])
  #       ncols <- ncol(tbln)
  #       align <- c("l", rep("r", ncols - 1))
  #       if(!pvalue){
  #         tbln <- tbln %>%
  #           `[`(,!lgrep(tbln %>% names, "p-value")) %>%
  #           rename(" " = ".1")
  #       }
  #       
  #       kable(tbln, align = align, format = "html", row.names = FALSE,
  #             table.attr = "class=\"table table-condensed\"", escape = FALSE) %>%
  #         print
  #     }
  #   }
  # }
  
}
