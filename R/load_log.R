

gen <- function(log.file, column.names) {
  # simulate a log file
  dataset <- utils::read.csv(log.file, header = TRUE, sep = ",", dec = ".")
  return(as.data.frame(dataset))
}


#' Title
#'
#' @param filenames Filenames to load
#' @param column.names Column names
#' @param verbose \code{TRUE} or \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
load_logs <- function( filenames, column.names, verbose = TRUE ) {
  time <- system.time({
    num.cores <- parallel::detectCores()-2
    set.seed(1)
    log <- parallel::mclapply(1:length(filenames), function(i,j) gen(filenames[i], column.names), mc.cores = num.cores)
    #if (verbose) utils::str(log)
  })
  if (verbose) print(time)
  return( log )
}
