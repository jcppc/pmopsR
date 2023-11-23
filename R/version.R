#' Prints the package version
#'
#' @return
#' @export
#'
#' @examples
version <- function() {
  insight::print_color(paste("\nPackage pmopsR version: ", packageVersion("pmopsR")), "green" )
  insight::print_color("\nBuilt by: João Caldeira & Bárbara Santos", "black")
  insight::print_color("\nBuilt at: Iscte - Instituto Universitário de Lisboa", "blue")
}
