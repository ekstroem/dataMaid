#' @title Summarize a variable
#'
#' @description Generic shell function that produces a summary of a variable (or for each
#' variable in an entire dataset), given a number of summary functions and 
#' depending on its data class.
#'
#' @param v The variable (vector) or dataset (data.frame) to be summarized.
#' 
#' @param reportstyleOutput Logical indicating whether the output should
#' be formatted for inclusion in the report (escaped matrix) or not. Defaults to not. 
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
#' @return The return value depends on the value of \code{reportstyleOutput}. 
#' 
#' If \code{reportstyleOutput = FALSE} (the default): If \code{v} is a varibale, 
#' a list of \code{summaryResult} objects, one \code{summaryResult} for each summary
#' function called on \code{v}. If \code{v} is a dataset, then \code{summarize()} returns 
#' a list of lists of \code{summaryResult} objects instead; one list for each variable
#' in \code{v}.  
#' 
#' If \code{reportstyleOutput = TRUE}: 
#' If \code{v} is a single variable: A matrix with two columns, \code{feature} and 
#' \code{result} and one row for each summary function that was called. Character
#' strings in this matrix are escaped such that they are ready for Rmarkdown rendering.
#' 
#' If \code{v} is a full dataset: A list of matrices as described above, one for each
#' variable in the dataset.
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{allSummaryFunctions}}, 
#' \code{\link{summaryResult}},
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
#'  #Summarize a variable and obtain report-style output:
#'   summarize(charV, reportstyleOutput = TRUE)
#'
#' @export
summarize <- function(v, reportstyleOutput = FALSE, ...) UseMethod("summarize")




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
summarize.character <- function(v, reportstyleOutput = FALSE,
                                characterSummaries = defaultCharacterSummaries(), 
                                ...) {
  res <- lapply(characterSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res)
  } else {
    names(res) <- characterSummaries
  }
  res
}



#' @export
summarize.factor <- function(v, reportstyleOutput = FALSE, 
                             factorSummaries = defaultFactorSummaries(), ...) {
  res <- lapply(factorSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res)
  } else {
    names(res) <- factorSummaries
  }
  res
}


#' @export
summarize.labelled <- function(v,  reportstyleOutput = FALSE, 
                               labelledSummaries = defaultLabelledSummaries(), 
                               ...) {
  res <- lapply(labelledSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res)
  } else {
    names(res) <- labelledSummaries
  }
  res
}


#' @export
summarize.numeric <- function(v,  reportstyleOutput = FALSE, 
                              numericSummaries = defaultNumericSummaries(),
                              maxDecimals = 2, ...) {
  res <- lapply(numericSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res, maxDecimals = maxDecimals)
  } else {
    names(res) <- numericSummaries
  }
  res
}


#' @export
summarize.integer <- function(v, reportstyleOutput = FALSE,
                              integerSummaries = defaultIntegerSummaries(),
                              maxDecimals = 2, ...) {
  res <- lapply(integerSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res, maxDecimals = maxDecimals)
  } else {
    names(res) <- integerSummaries
  }
  res
}


#' @export
summarize.logical <- function(v, reportstyleOutput = FALSE,
                              logicalSummaries = defaultLogicalSummaries(), ...) {
  res <- lapply(logicalSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res)
  } else {
    names(res) <- logicalSummaries
  }
  res
}


#' @export
summarize.Date <- function(v, reportstyleOutput = FALSE,
                           dateSummaries = defaultDateSummaries(),
                           maxDecimals = 0, ...) {
  res <- lapply(dateSummaries, function(x) eval(call(x, v = v)))
  if (reportstyleOutput) {
    res <- sumMatGenerator(res, maxDecimals = maxDecimals)
  } else {
    names(res) <- dateSummaries
  }
  res
}


#' @export
summarize.data.frame <- function(v, reportstyleOutput = FALSE, ...) {
  lapply(v, summarize, reportstyleOutput, ...)
}


##########################################Not exported below#########################################

#produces the output matrix from a summarize call. Use internally only
sumMatGenerator <- function(resList, maxDecimals = NULL) {
  nFunctions <- length(resList)
  outMat <- matrix(NA, nFunctions, 2,
                   dimnames=list(NULL, c("Feature", "Result")))
  for (i in 1:nFunctions) {
    outMat[i, "Feature"] <- resList[[i]]$feature
    outMat[i, "Result"] <- resList[[i]]$result
  }
  outMat
}



