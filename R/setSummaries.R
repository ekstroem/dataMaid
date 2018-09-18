#' Set summary arguments for makeDataReport
#' 
#' This function is a tool for easily specifying the \code{summaries} argument of 
#' \code{\link{makeDataReport}}. Note that all available summary function options can be inspected
#' by calling \code{allSummaryFunctions()}. 
#' 
#' @param character A character vector of function names to be used as summaries for character
#' variables. The default options are available by calling \code{defaultCharacterSummaries()}.
#' 
#' @param factor A character vector of function names to be used as summaries for factor
#' variables. The default options are available by calling \code{defaultFactorSummaries()}. 
#' 
#' @param labelled A character vector of function names to be used as summaries for labelled
#' variables. The default options are available by calling \code{defaultLabelledSummaries()}. 
#' 
#' @param haven_labelled A character vector of function names to be used as summaries for haven_labelled
#' variables. The default options are available by calling \code{defaultHavenlabelledSummaries()}. 
#' 
#' @param numeric A character vector of function names to be used as summaries for numeric
#' variables. The default options are available by calling \code{defaultNumericSummaries()}. 
#' 
#' @param integer A character vector of function names to be used as summaries for integer
#' variables. The default options are available by calling \code{defaultIntegerSummaries()}. 
#' 
#' @param logical  A character vector of function names to be used as summaries for logical
#' variables. The default options are available by calling \code{defaultLogicalSummaries()}. 
#' 
#' @param Date A character vector of function names to be used as summaries for Date
#' variables. The default options are available by calling \code{defaultDateSummaries()}. 
#' 
#' @param all A character vector of function names to be used as summaries for all
#' variables. Note that this overrules the choices made for specific variable types by using
#' the other arguments.
#' 
#' @return A list with one entry for each data class supported by \code{makeDataReport}. Each
#' entry then contains a character vector of function names that are to be called as summaries for
#' that variable type. 
#' 
#' @seealso \code{\link{makeDataReport}}, \code{\link{allSummaryFunctions}},
#' \code{\link{defaultCharacterSummaries}}, 
#' \code{\link{defaultFactorSummaries}}, \code{\link{defaultLabelledSummaries}},
#' \code{\link{defaultHavenlabelledSummaries}},
#' \code{\link{defaultNumericSummaries}}, \code{\link{defaultIntegerSummaries}},
#' \code{\link{defaultLogicalSummaries}}, \code{\link{defaultDateSummaries}}
#' @examples
#' #Don't include central value (median/mode) summary for numerical and integer
#' #variables:
#'   setSummaries(numeric = defaultNumericSummaries(remove = "centralValue"),
#'     integer = defaultIntegerSummaries(remove = "centralValue"))
#'   
#'      
#' #Used in a call to makeDataReport():
#' \dontrun{
#' data(toyData)
#' makeDataReport(toyData, 
#'   setSummaries(numeric = defaultNumericSummaries(remove = "centralValue"),
#'     integer = defaultIntegerSummaries(remove = "centralValue")), replace = TRUE)
#' }
#'      
#' @export    
setSummaries <- function(character = defaultCharacterSummaries(), 
                      factor = defaultFactorSummaries(), 
                      labelled = defaultLabelledSummaries(), 
                      haven_labelled = defaultHavenlabelledSummaries(),
                      numeric = defaultNumericSummaries(), 
                      integer = defaultIntegerSummaries(), 
                      logical = defaultLogicalSummaries(), 
                      Date = defaultDateSummaries(), all = NULL) {
  if (!is.null(all)) {
    character <- factor <- labelled <- haven_labelled <- numeric <- integer <- logical <- Date <- all
  } 
  outList <- list(character = character, factor = factor,
                  labelled = labelled,
                  haven_labelled = haven_labelled,
                  numeric = numeric,
                  integer = integer, logical = logical,
                  Date = Date)
  outList
}