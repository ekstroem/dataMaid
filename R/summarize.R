#' @title Make summary matrix
#'
#' @description Generic shell function that produces a summary matrix for a variable (or for each
#' variable in an entire dataset), given a number of summary functions and 
#' depending on its data class.
#'
#' @param v The variable (vector) or dataset (data.frame) to be summarized.
#' 
#' @param ... Additional argument passed to data class specific methods. First and foremost,
#' this is where to supply the summary functions (see details).
#'
#' @details Summary functions are supplied using their
#' names (in character strings) in the class-specific argument, e.g.
#' \code{characterSummaries = c("countMissing", "uniqueValues")} for character variables and
#' similarly for the remaining 6 data classes (factor, Date, labelled, numeric, integer, logical).
#' Note that an overview of all available \code{summaryFunction}s can be obtained by calling
#' \code{\link{allSummaryFunctions}}. 
#'
#' The default choices of \code{summaryFunctions} are available in data class specific functions, e.g.
#' \code{defaultCharacterSummaries()} and \code{defaultNumericSummaries()}.
#'
#' A user defined summary function can be supplied using its function name. Note
#' however that it should take a vector as argument and return a list on the form
#' \code{list(feature="Feature name", result="The result")}. More details on how to construct 
#' valid summary functions are found in \code{\link{summaryFunction}}.
#'
#' @return If \code{v} is a single variable: A matrix with two columns, \code{feature} and 
#' \code{result} and one row for each summary function that was called. Character
#' strings in this matrix are escaped such that they are ready for Rmarkdown rendering.
#' 
#' If \code{v} is a full dataset: A list of matrices as described above, one for each
#' variable in the dataset.
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{allSummaryFunctions}}, 
#' \code{\link{defaultCharacterSummaries}}, \code{\link{defaultFactorSummaries}},
#' \code{\link{defaultLabelledSummaries}}, \code{\link{defaultLabelledSummaries}},
#' \code{\link{defaultNumericSummaries}}, \code{\link{defaultIntegerSummaries}},
#' \code{\link{defaultLogicalSummaries}}
#'
#' @examples
#' #Default summary for a character vector:
#'    charV <- c("a", "b", "c", "a", "a", NA, "b", "0")
#'    summarize(charV)
#'
#' #Inspect default character summary functions:
#'    defaultCharacterSummaries()
#'
#' #Define a new summary function and add it to the summary for character vectors:
#'    countZeros <- function(v, ...) {
#'      res <- length(which(v == 0))
#'      summaryResult(list(feature="No. zeros", result = res, value = res))
#'    }
#'    summarize(charV, characterSummaries = c(defaultCharacterSummaries(), "countZeros"))
#'
#'  #Does nothing, as intV is not affected by characterSummaries
#'    intV <- c(0:10)
#'    summarize(intV, characterSummaries = c(defaultCharacterSummaries(), "countZeros"))
#'
#'  #But supplying the argument for integer variables changes the summary:
#'    summarize(intV, integerSummaries = "countZeros")
#'    
#'  #Summarize a full dataset:
#'   data(cars)
#'   summarize(cars)
#'
#' @export
summarize <- function(v, ...) UseMethod("summarize")




#' Default summary functions for character variables
#'
#' Returns vector of names of default summary functions for character variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}}, \code{\link{countMissing}}, \code{\link{uniqueValues}},
#' \code{\link{centralValue}}
#'
#' @examples
#' defaultCharacterSummaries()
#'
#' @export
defaultCharacterSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                          "centralValue")


#' Default summary functions for factor variables
#'
#' Returns vector of names of default summary functions for factor variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso code{\link{variableType}}, \code{\link{countMissing}}, \code{\link{uniqueValues}},
#' \code{\link{centralValue}}
#'
#' @examples
#' defaultFactorSummaries()
#'
#' @export
defaultFactorSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                       "centralValue")


#' Default summary functions for labelled variables
#'
#' Returns vector of names of default summary functions for labelled variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultLabelledSummaries()
#'
#' @export
defaultLabelledSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                         "centralValue")


#' Default summary functions for numeric variables
#'
#' Returns vector of names of default summary functions for numeric variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}},
#' \code{\link{centralValue}}, \code{\link{quartiles}}, \code{\link{minMax}}
#'
#' @examples
#' defaultNumericSummaries()
#'
#' @export
defaultNumericSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                        "centralValue", "quartiles", "minMax")


#' Default summary functions for integer variables
#'
#' Returns vector of names of default summary functions for integer variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}},
#' \code{\link{centralValue}}, \code{\link{quartiles}}, \code{\link{minMax}}
#'
#' @examples
#' defaultIntegerSummaries()
#'
#' @export
defaultIntegerSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                        "centralValue", "quartiles", "minMax")


#' Default summary functions for logical variables
#'
#' Returns vector of names of default summary functions for logical variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultLogicalSummaries()
#'
#' @export
defaultLogicalSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                        "centralValue")



#' Default summary functions for Date variables
#'
#' Returns vector of names of default summary functions for Date variables.
#'
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{variableType}}, \code{\link{countMissing}}, \code{\link{uniqueValues}},
#' \code{\link{centralValue}}, \code{\link{minMax}}, \code{\link{quartiles}}
#'
#' @examples
#' defaultDateSummaries()
#'
#' @export
#defaultDateSummaries <- function() c("variableType")
defaultDateSummaries <- function() c("variableType", "countMissing", "uniqueValues",
                                     "centralValue", "minMax", "quartiles")



#methods for each data type

#' @export
summarize.character <- function(v, characterSummaries = defaultCharacterSummaries(), ...) {
  sumMatGenerator(v, characterSummaries)
}


#' @export
summarize.factor <- function(v, factorSummaries = defaultFactorSummaries(), ...) {
  sumMatGenerator(v, factorSummaries)
}


#' @export
summarize.labelled <- function(v, labelledSummaries = defaultLabelledSummaries(), ...) {
  sumMatGenerator(v, labelledSummaries)
}


#' @export
summarize.numeric <- function(v, numericSummaries = defaultNumericSummaries(),
                              maxDecimals = 2, ...) {
  sumMatGenerator(v, numericSummaries, maxDecimals = maxDecimals)
}


#' @export
summarize.integer <- function(v, integerSummaries = defaultIntegerSummaries(),
                              maxDecimals = 2, ...) {
  sumMatGenerator(v, integerSummaries, maxDecimals = maxDecimals)
}


#' @export
summarize.logical <- function(v, logicalSummaries = defaultLogicalSummaries(), ...) {
  sumMatGenerator(v, logicalSummaries)
}


#' @export
summarize.Date <- function(v, dateSummaries = defaultDateSummaries(),
                           maxDecimals = 0, ...) {
    sumMatGenerator(v, dateSummaries, maxDecimals = maxDecimals)
}


#' @export
summarize.data.frame <- function(v, ...) {
  lapply(v, summarize, ...)
}


##########################################Not exported below#########################################

#produces the output matrix from a summarize call. Use internally only
sumMatGenerator <- function(v, summaries, maxDecimals = NULL) {
  nFunctions <- length(summaries)
  outMat <- matrix(NA, nFunctions, 2,
                   dimnames=list(NULL, c("Feature", "Result")))
  for (i in 1:nFunctions) {
    res <- eval(call(summaries[i], v, maxDecimals = maxDecimals))
    outMat[i, "Feature"] <- res$feature
    outMat[i, "Result"] <- res$result
  }
  outMat
}


