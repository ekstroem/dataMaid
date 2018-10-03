#' Set check arguments for makeDataReport
#' 
#' This function is a tool for easily specifying the \code{checks} argument of 
#' \code{\link{makeDataReport}}. Note that all available check function options can be inspected
#' by calling \code{allCheckFunctions()}. 
#' 
#' @param character A character vector of function names to be used as checks for character
#' variables. The default options are available by calling \code{defaultCharacterChecks()}.
#' 
#' @param factor A character vector of function names to be used as checks for factor
#' variables. The default options are available by calling \code{defaultFactorChecks()}. 
#' 
#' @param labelled A character vector of function names to be used as checks for labelled
#' variables. The default options are available by calling \code{defaultLabelledChecks()}. 
#' 
#' @param haven_labelled A character vector of function names to be used as checks for haven_labelled
#' variables. The default options are available by calling \code{defaultHavenlabelledChecks()}. 
#' 
#' @param numeric A character vector of function names to be used as checks for numeric
#' variables. The default options are available by calling \code{defaultNumericChecks()}. 
#' 
#' @param integer A character vector of function names to be used as checks for integer
#' variables. The default options are available by calling \code{defaultIntegerChecks()}. 
#' 
#' @param logical  A character vector of function names to be used as checks for logical
#' variables. The default options are available by calling \code{defaultLogicalChecks()}. 
#' 
#' @param Date A character vector of function names to be used as checks for Date
#' variables. The default options are available by calling \code{defaultDateChecks()}. 
#' 
#' @param all A character vector of function names to be used as checks for all
#' variables. Note that this overrules the choices made for specific variable types by using
#' the other arguments.
#' 
#' @return A list with one entry for each data class supported by \code{makeDataReport}. Each
#' entry then contains a character vector of function names that are to be called as checks for
#' that variable type. 
#' 
#' @seealso \code{\link{makeDataReport}}, \code{\link{allCheckFunctions}},
#' \code{\link{defaultCharacterChecks}}, 
#' \code{\link{defaultFactorChecks}}, \code{\link{defaultLabelledChecks}},
#' \code{\link{defaultHavenlabelledChecks}},
#' \code{\link{defaultNumericChecks}}, \code{\link{defaultIntegerChecks}},
#' \code{\link{defaultLogicalChecks}}, \code{\link{defaultDateChecks}}
#' @examples
#' #Only identify missing values for characters, logicals and labelled variables:
#'   setChecks(character = "identifyMissing", factor = "identifyMissing", 
#'      labelled = "identifyMissing")
#'      
#' #Used in a call to makeDataReport():
#' \dontrun{
#' data(toyData)
#' makeDataReport(toyData, checks = setChecks(character = "identifyMissing", 
#'    factor = "identifyMissing", labelled = "identifyMissing"), replace = TRUE)
#' }
#'      
#' @export    
setChecks <- function(character = defaultCharacterChecks(), 
                      factor = defaultFactorChecks(), 
                      labelled = defaultLabelledChecks(), 
                      haven_labelled = defaultHavenlabelledChecks(),
                      numeric = defaultNumericChecks(), 
                      integer = defaultIntegerChecks(), 
                      logical = defaultLogicalChecks(), 
                      Date = defaultDateChecks(), all = NULL) {
  if (!is.null(all)) {
    character <- factor <- labelled <- haven_labelled <- numeric <- integer <- logical <- Date <- all
  } 
  outList <- list(character = character, factor = factor,
                  labelled = labelled, haven_labelled = haven_labelled,
                  numeric = numeric,
                  integer = integer, logical = logical,
                  Date = Date)
  outList
}