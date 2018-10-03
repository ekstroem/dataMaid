#' @title A checkFunction
#'
#' @description A \code{\link{checkFunction}} to be called from
#' \code{\link{check}} for identifying numeric variables that have
#' been misclassified as categorical.
#'
#' @param v A character, factor, or (haven_)labelled variable to check.
#' 
#' @param nVals An integer determining how many unique values a variable must have
#' before it can potentially be determined to be a misclassified numeric variable. 
#' The default is \code{12}.
#'
#' @param ... Not in use.
#'
#' @return A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating the variable is suspected to be
#' a misclassified numeric variable), \code{$message} (if a problem was found,
#' the following message: "Note: The variable consists exclusively of numbers and takes
#' a lot of different values. Is it perhaps a misclassified numeric variable?",
#' otherwise "") and \code{$problemValues} (always \code{NULL}).
#'
#' @details A categorical variable is suspected to be a misclassified
#' numeric variable if it has the following two properties: First,
#' it should consist exclusively of numbers (possibly including signs
#' and decimals points). Secondly, it must have at least \code{nVals} unique values.
#' The default values of \code{nVals} is 12, which means that 
#' e.g. variables including answers on a scale from 0-10 will
#' not be recognized as misclassified numerics.
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#' @examples
#'  #Positive and negative numbers, saved as characters
#'  identifyNums(c(as.character(-9:9)))
#'
#'  #An ordinary character variable
#'  identifyNums(c("a", "b", "c", "d", "e.f", "-a", 1:100))
#'
#'
#' @importFrom stats na.omit
#' @importFrom haven as_factor
#' @export
identifyNums <- function(v, nVals = 12, ...) {
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  
  
    #note: update to haven made as_factor not work on character variables!
  if ("labelled" %in% class(v)) {
    v <- as.character(na.omit(dataMaid_as_factor(v)))
  } else v <- as.character(na.omit(v))
  
  if (length(unique(v)) < nVals) {
    return(checkResult(out))
  }
  v[v==""] <- "a" #make sure v contains no empty strings
  v <- gsub("^-{1}", "", v) #remove signs (prefixed -)
  v <- gsub("\\.{1}", "", v) #remove decimal points
  v <- gsub("[[:digit:]]", "", v) #replace numbers with empty strings
  if (sum(nchar(v)) == 0) {
    out$problem <- TRUE
    out$message <- "Note: The variable consists exclusively of numbers and takes a lot of different values. Is it perhaps a misclassified numeric variable?"
  }
  checkResult(out)
}

#' @include checkFunction.R
identifyNums <- checkFunction(identifyNums,
                              "Identify misclassified numeric or integer variables",
                              c("character", "factor", "labelled", "haven_labelled"))
