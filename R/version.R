#' Print the package version and authoring details
#'
#' @return
#' @export
#'
#' @examples
version <- function() {

  version <- paste("\nPackage pmopsR version: ", utils::packageVersion("pmopsR"))
  authors <- "\nBuilt by: Jo\u00e3o Caldeira & B\u00e1rbara Santos"
  organization <-  "\nBuilt at: Iscte - Instituto Universit\u00e1rio de Lisboa\n"

  insight::print_color(version, "green" )
  insight::print_color(authors, "black")
  insight::print_color(organization, "blue")
}
