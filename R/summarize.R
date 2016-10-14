#' @title Make summary matrix
#'
#' @description Generic shell function that produces a summary matrix for a variable \code{v},
#' given a number of summary functions and depending on its data class.
#'
#' @param v The variable (vector) which is to be summarized.
#' @param descriptive If TRUE, descriptive functions are added to the summary matrix
#' (see details).
#' @param ... Additional argument passed to data class specific methods. First and foremost,
#' this is where to supply the summary functions and description functions (see details).
#'
#' @details Summary functions are supplied using their
#' names (in character strings) in the class-specific argument, e.g.
#' \code{characterSummaries = c("countMissing", "uniqueValues")} for character variables and
#' similarly for the remaining 5 data classes (factor, labelled, numeric, integer, logical).
#' Secondly, additional functions can be added using the "descriptions"-arguments, e.g.
#' \code{characterDescriptions = "centralValue"}. If \code{descriptive = T} the results of
#' these functions are added to the outputted summary matrix. Note that this is nothing
#' more than a convenient way to add an extra category of summary functions to the output.
#' Summary functions and description functions are treated in the exact same way.
#'
#' The default summary- and character functions are available in data class specific functions, e.g.
#' \code{defaultCharacterSummaries()} and \code{defaultCharacterDescriptions()}, respectively.
#'
#' A user defined summary (or description) function can be supplied using its function name. Note
#' however that it should take a vector as argument and return a list on the form
#' \code{list(feature="Feature name", result="The result")}. See e.g. \code{\link{variableType}} for
#' an example of a summary function.
#'
#' @return A matrix with two columns, \code{feature} and \code{result} and one row for each
#' summary/description function that was called.
#'
#' @seealso \code{\link{defaultCharacterSummaries}}, \code{\link{defaultFactorSummaries}},
#' \code{\link{defaultLabelledSummaries}}, \code{\link{defaultLabelledSummaries}},
#' \code{\link{defaultNumericSummaries}}, \code{\link{defaultIntegerSummaries}},
#' \code{\link{defaultLogicalSummaries}},
#' \code{\link{defaultCharacterDescriptions}}, \code{\link{defaultFactorDescriptions}},
#' \code{\link{defaultLabelledDescriptions}}, \code{\link{defaultLabelledDescriptions}},
#' \code{\link{defaultNumericDescriptions}}, \code{\link{defaultIntegerDescriptions}},
#' \code{\link{defaultLogicalDescriptions}}, 
#' 
#' @examples 
#' #Default summary for a character vector: 
#'    charV <- c("a", "b", "c", "a", "a", NA, "b", "0")
#'    summarize(charV)
#'  
#' #Add default description functions:
#'    summarize(charV, descriptive = TRUE)
#' 
#' #Inspect default character summary functions:
#'    defaultCharacterSummaries()
#'    
#' #Define a new summary function and add it to the summary for character vectors:
#'    countZeros <- function(v) {
#'      res <- length(which(v == 0))
#'      list(feature="No. zeros", result = res)
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
#' @export
summarize <- function(v, descriptive = FALSE, ...) UseMethod("summarize")





#' @title Default summary functions for character variables
#' 
#' @description Returns vector of names of default summary functions for character variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultCharacterDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultCharacterSummaries()
#'
#' @export
defaultCharacterSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for character variables
#' 
#' @description Returns vector of names of default description functions for character variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultCharacterSummaries}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultCharacterDescriptions()
#'
#' @export
defaultCharacterDescriptions <- function() "centralValue"


#' @title Default summary functions for factor variables
#' 
#' @description Returns vector of names of default summary functions for factor variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultFactorDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultFactorSummaries()
#'
#' @export
defaultFactorSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for factor variables
#' 
#' @description Returns vector of names of default description functions for factor variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultFactorSummaries}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultFactorDescriptions()
#'
#' @export
defaultFactorDescriptions <- function() "centralValue"


#' @title Default summary functions for labelled variables
#' 
#' @description Returns vector of names of default summary functions for labelled variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultLabelledDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultLabelledSummaries()
#'
#' @export
defaultLabelledSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for labelled variables
#' 
#' @description Returns vector of names of default description functions for labelled variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultLabelledSummaries}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultLabelledDescriptions()
#'
#' @export
defaultLabelledDescriptions <- function() "centralValue"


#' @title Default summary functions for numeric variables
#' 
#' @description Returns vector of names of default summary functions for numeric variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultNumericDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultNumericSummaries()
#'
#' @export
defaultNumericSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for numeric variables
#' 
#' @description Returns vector of names of default description functions for numeric variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultNumericSummaries}}, \code{\link{centralValue}},
#' \code{\link{quartiles}}, \code{\link{minMax}}
#'
#' @examples
#' defaultNumericDescriptions()
#'
#' @export
defaultNumericDescriptions <- function() c("centralValue", "quartiles", "minMax")


#' @title Default summary functions for integer variables
#' 
#' @description Returns vector of names of default summary functions for integer variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultIntegerDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultIntegerSummaries()
#'
#' @export
defaultIntegerSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for integer variables
#' 
#' @description Returns vector of names of default description functions for integer variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultIntgerSummaries}}, \code{\link{centralValue}},
#' \code{\link{quartiles}}, \code{\link{minMax}}
#'
#' @examples
#' defaultIntegerDescriptions()
#'
#' @export
defaultIntegerDescriptions <- function() c("centralValue", "quartiles", "minMax")


#' @title Default summary functions for logical variables
#' 
#' @description Returns vector of names of default summary functions for logical variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultLogicalDescriptions}}, \code{\link{variableType}},
#' \code{\link{countMissing}}, \code{\link{uniqueValues}}
#'
#' @examples
#' defaultLogicalSummaries()
#'
#' @export
defaultLogicalSummaries <- function() c("variableType", "countMissing", "uniqueValues")


#' @title Default description functions for logical variables
#' 
#' @description Returns vector of names of default description functions for logical variables.  
#' 
#' @return A list of function names (as character strings).
#'
#' @seealso \code{\link{defaultLogicalSummaries}}, \code{\link{centralValue}}
#'
#' @examples
#' defaultLogicalDescriptions()
#'
#' @export
defaultLogicalDescriptions <- function() "centralValue"


#methods for each data type

#' @export
summarize.character <- function(v, descriptive = FALSE, 
                                characterSummaries = defaultCharacterSummaries(),
                                characterDescriptions = defaultCharacterDescriptions(), ...) {
  characterCalls <- if (descriptive) c(characterSummaries, characterDescriptions)
  else characterSummaries
  sumMatGenerator(v, characterCalls)
}


#' @export
summarize.factor <- function(v, descriptive = FALSE, 
                             factorSummaries = defaultFactorSummaries(),
                             factorDescriptions = defaultFactorDescriptions(), ...) {
  factorCalls <- if (descriptive) c(factorSummaries, factorDescriptions)
  else factorSummaries
  sumMatGenerator(v, factorCalls)
}


#' @export
summarize.labelled <- function(v, descriptive = FALSE, 
                               labelledSummaries = defaultLabelledSummaries(),
                               labelledDescriptions = defaultLabelledDescriptions(), ...) {
  labelledCalls <- if (descriptive) c(labelledSummaries, labelledDescriptions)
  else labelledSummaries
  sumMatGenerator(v, labelledCalls)
}


#' @export
summarize.numeric <- function(v, descriptive = FALSE, 
                              numericSummaries = defaultNumericSummaries(),
                              numericDescriptions = defaultNumericDescriptions(), ...) {
  numericCalls <- if (descriptive) c(numericSummaries, numericDescriptions)
  else numericSummaries
  sumMatGenerator(v, numericCalls)
}


#' @export
summarize.integer <- function(v, descriptive = FALSE,  
                              integerSummaries = defaultIntegerSummaries(),
                              integerDescriptions = defaultIntegerDescriptions(), ...) {
  integerCalls <- if (descriptive) c(integerSummaries, integerDescriptions)
  else integerSummaries
  sumMatGenerator(v, integerCalls)
}


#' @export
summarize.logical <- function(v, descriptive = FALSE, 
                              logicalSummaries = defaultLogicalSummaries(),
                              logicalDescriptions = defaultLogicalDescriptions(), ...) {
  logicalCalls <- if (descriptive) c(logicalSummaries, logicalDescriptions)
  else logicalSummaries
  sumMatGenerator(v, logicalCalls)
}





##########################################Not exported below#########################################

#produces the output matrix from a summarize call. Use internally only
sumMatGenerator <- function(v, summaries) {
  nFunctions <- length(summaries)
  outMat <- matrix(NA, nFunctions, 2,
                   dimnames=list(NULL, c("Feature", "Result")))
  for (i in 1:nFunctions) {
    res <- eval(call(summaries[i], v))
    outMat[i, "Feature"] <- res$feature
    outMat[i, "Result"] <- res$result
  }
  outMat
}


