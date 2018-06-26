reset_labels <- function(data){

    clear_labels <- function(data2) {
      labels.list.name <- paste0(deparse(substitute(data2)), "_labels")
      labels.list <- sapply(data2, function(x) attr(x, "label"))
      
      if (is.list(data2)) {
        for (i in 1:length(data2)) class(data2[[i]]) <- setdiff(class(data2[[i]]), 'labelled')
        
        for (i in 1:length(data2)) attr(data2[[i]], "label") <- NULL
      }
      else {
        class(data2) <- setdiff(class(data2), "labelled")
        labels.list <- attr(data2, "label")
        attr(data2, "label") <- NULL
      }
      assign(labels.list.name, labels.list, envir = .GlobalEnv)
      return(data2)
    }
  
  mydat.label.temp <- label(data)
  
  data <- droplevels(data)
  
  data <- clear_labels(data)
  
  label(data) = lapply(names(mydat.label.temp), function(x) label(data[,x]) = mydat.label.temp[x])
  
  return(data)
}