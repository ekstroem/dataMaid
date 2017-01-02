#' @title Vector of all variable classes in \code{dataMaid}
#'
#' @description Returns the names of the seven data classes for which
#' \code{dataMaid} is implemented, namely \code{"character"}, \code{"Date"},
#' \code{"factor"}, \code{"integer"}, \code{"labelled"}, \code{"logical"} and
#' \code{"numeric"}.
#'
#' @examples
#' allClasses()
#'
#' @export
allClasses <- function() {
  c("character", "Date", "factor", "integer", "labelled",
    "logical", "numeric")
}
