#' Set visual arguments for makeDataReport
#' 
#' This function is a tool for easily specifying the \code{visuals} argument of 
#' \code{\link{makeDataReport}}. Note that only a single visual function can 
#' be provided for each variable type. If more than one is supplied, only 
#' the first one is used. The default is to use a single visual function for all 
#' variable types (as specified in the argument \code{all}), but class-specific choices
#' of visual functions can also be used. Note that class-specific arguments overwrites 
#' the contents of \code{all}. Note that all available visual function options can be inspected
#' by calling \code{allVisualFunctions()}. 
#' 
#' @param character A function name (character string) to be used as the visual function for character
#' variables. If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param factor A function name (character string) to be used as the visual function for factor
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param labelled A function name (character string) to be used as the visual function for labelled
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param haven_labelled A function name (character string) to be used as the visual function for haven_labelled
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param numeric A function name (character string) to be used as the visual function for numeric
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param integer A function name (character string) to be used as the visual function for integer
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param logical  A function name (character string) to be used as the visual function for logical
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param Date A function name (character string) to be used as the visual function for Date
#' variables.  If \code{NULL} (the default) the argument is ignored and the contents of the \code{all}
#' argument is used instead.
#' 
#' @param all A function name (character string) to be used as the visual function for all
#' variables.
#' 
#' @return A list with one entry for each data class supported by \code{makeDataReport}. Each
#' entry then contains a character string with a function name that is to be called as the visual 
#' function for that variable type. 
#' 
#' @seealso \code{\link{makeDataReport}}, \code{\link{allVisualFunctions}}
#' @examples
#' #Set visual type to basicVisual for all variable types:
#'   setVisuals(all = "basicVisual")
#'      
#' #Used in a call to makeDataReport():
#' \dontrun{
#' data(toyData)
#' makeDataReport(toyData, visuals = setVisuals(all = "basicVisual"), replace = TRUE)
#' }
#'           
#' @export    
setVisuals <- function(character = NULL, 
                      factor = NULL, 
                      labelled = NULL, 
                      haven_labelled = NULL,
                      numeric = NULL, 
                      integer = NULL, 
                      logical = NULL, 
                      Date = NULL, all = "standardVisual") {
  if (is.null(character)) character <- all
  if (is.null(factor)) factor <- all
  if (is.null(labelled)) labelled <- all
  if (is.null(haven_labelled)) haven_labelled <- all
  if (is.null(numeric)) numeric <- all
  if (is.null(integer)) integer <- all
  if (is.null(logical)) logical <- all
  if (is.null(Date)) Date <- all
  
  outList <- list(character = character, factor = factor,
                  labelled = labelled, 
                  haven_labelled = haven_labelled,
                  numeric = numeric,
                  integer = integer, logical = logical,
                  Date = Date)
  
  outList <- lapply(outList, function(x) x[1])
  
  outList
}