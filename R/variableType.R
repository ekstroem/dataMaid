#' @title Summary function for original class
#'
#' @description A summary type function to be called from \code{\link{summarize}}, which finds the
#' orignial class of a variable. This is just the class for all objects but those of class
#' smartNum.
#'
#' @param v A variable (vector).
#'
#' @param ... Not in use.
#'
#' @return A list with $feature: "Variable type" and
#' $result: [the (original) class of the variable].
#'
#' @seealso \code{\link{summarize}}
#'
#' @examples
#'  #For standard variables:
#'    varX <- c(rep(c(1,2,3), each=10))
#'    class(varX)
#'    variableType(varX)
#'
#'  #For smartNum variables:
#'    smartX <- smartNum(varX)
#'    class(smartX)
#'    variableType(smartX)
#'
#'  #Something with multiple classes, like ordered factor...
#'
#' @export
variableType <- function(v, ...) {
  vClass <- oClass(v)[1]
  summaryResult(list(feature="Variable type", result = vClass, value = vClass))
}

variableType <- summaryFunction(variableType, "Data class of variable",
                                c("character", "factor", "integer", "labelled", "logical",
                                  "numeric", "Date"))
