#' Get AHSQC data
#'
#' Retrieves the most recent download of AHSQC data or the download from a specific date.
#' @param date NULL or character string of a date in the format "YYYY-MM-DD"
#' @param dir NULL or character string with file path to ahs-data folder.
#' @details If \code{date} = NULL, then the function will return the most recently downloaded dataset from the ahs-data folder. 
#' 
#'If \code{dir} = NULL, the function will assume that the command is being run in a working directory of the following type: ".../alfresco/Sites/[SITE NAME]/documentLibrary/code".
#' 
#' If date is a character string of a date, then the function retuns the data downloaded on that date, if it exists.  If the date does not exist in the ahs-data folder, the function throws an error.
#' @keywords AHSQC
#' @export
#' @examples
#' # Not run:
#' # d0 <- ahs_get_data()

ahs_get_data <- function(date = NULL, dir = NULL){
  if(is.null(dir)) dir <- file.path("..","..","..","ahs-data")
  
  possible_dirs <- dir(
    path = file.path(dir,"documentLibrary","data"),
    pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  )
  
  if(length(possible_dirs) == 0) stop("The data directory is empty or its path was not properly specified.")
  
  if(is.null(date)){
    # GET DATE OF RECENT DATA
    recent_dir <- sort(possible_dirs, decreasing = TRUE)[1]
  }else{
    # USE DATE SUPPLIED
    if(date %in% possible_dirs){
      recent_dir <- date
    }else{
      stop("The date provided is in the wrong format or is not available")
    }
  }
  
  data_dir <- file.path(dir, recent_dir, "data", "input")
  out <- readRDS(file.path(data_dir, "master.rds" ))
  
  message("Loading AHSQC data from ", recent_dir)
  
  return(out)
}