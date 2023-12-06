#' Title
#'
#' @return
#' @export
#'
#' @examples
automl <- function() {
  print("Hello, world! from AutoML")
}


gen <-  function(i) {
  print(i)
  stats::rnorm(i)
}

#' Title
#'
#' @param sample Number of samples
#' @param verbose \code{TRUE} or \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
auto_discovery <- function( sample = 5, verbose = TRUE ) {
  time <- system.time({
    num.cores <- parallel::detectCores()-2
    set.seed(1)
    r <- parallel::mclapply(1:sample, function(i) gen(i), mc.cores = num.cores)
    if (verbose) utils::str(r)
  })
if (verbose) time
}



#' Title
#'
#' @param sample Number of samples
#' @param verbose \code{TRUE} or \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
auto_discovery2 <- function( sample = 5, verbose = TRUE ) {
  time <- system.time({
    num.cores <- parallel::detectCores()-2
    set.seed(1)
    r <- parallel::mclapply(1:sample, function(i) gen(i), mc.cores = num.cores)
    if (verbose) utils::str(r)
  })
  if (verbose) time
}


