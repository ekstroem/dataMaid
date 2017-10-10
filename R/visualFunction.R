#' @title Create an object of class visualFunction
#' 
#' @description Convert a function, \code{f}, into an S3 
#' \code{visualFunction} object. This adds \code{f} to the
#' overview list returned by an \code{allVisualFunctions()} 
#' call.
#'
#' @inheritParams checkFunction
#' 
#' @param description A character string describing the visualization
#' returned by \code{f}. If \code{NULL} (the default), the name of 
#' \code{f} will be used instead. 
#' 
#' @return A function of class \code{visualFunction} which has to attributes, 
#' namely \code{classes} and \code{description}. 
#' 
#' @details \code{visualFunction} represents the functions used in 
#' \code{\link{visualize}} and \code{\link{makeDataReport}} for plotting the 
#' distributions of the variables in a dataset.
#' 
#' An example of defining a new \code{visualFunction} is given below. 
#' Note that the minimal requirements for such a function (in order for it to be 
#' compatible with \code{visualize()} and \code{makeDataReport()}) is the following 
#' input/output-structure: It must input exactly the following three arguments, 
#' namely \code{v} (a vector variable), \code{vnam} (a character string with
#' the name of the variable) and \code{doEval} (a logical). The last argument
#' is supposed to control whether the function produces a plot in the 
#' graphic device (if \code{doEval = TRUE}) or instead returns a character
#' string including \code{R} code for generating such a plot. In the latter 
#' setting, the code must be stand-alone, that is, it cannot depend on object
#' available in an environment. In practice, this will typically imply that 
#' the data variable is included in the code snip. 
#' It is not strictly necessary to implement the \code{doEval = TRUE} setting 
#' for the \code{visualFunction} to be compatible with \code{\link{makeDataReport}}, 
#' but we recommend doing it anyway such that the function can also be used 
#' interactively.
#' 
#' Note that all available \code{visualFunction}s are listed by the call
#' \code{allVisualFunctions()} and we recommed looking into these function,
#' if more knowledge about \code{visualFunction}s is required.
#' 
#' @include makeXFunction.R allClasses.R
#' 
#' @seealso \code{\link{allVisualFunctions}}, \code{\link{visualize}}, 
#' \code{\link{makeDataReport}}
#' 
#' @examples
#' #Defining a new visualFunction:
#'  mosaicVisual <- function(v, vnam, doEval) {
#'    thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
#'    if (doEval) {
#'     return(eval(thisCall))
#'    } else return(deparse(thisCall))
#'  }
#'  mosaicVisual <- visualFunction(mosaicVisual, description = "Mosaicplots from graphics",
#'                                 classes = allClasses())
#' 
#' #mosaicVisual is now included in a allVisualFunctions() call:
#'  allVisualFunctions()
#'  
#' #Create a mosaic plot:
#'  ABCvar <- c(rep("a", 10), rep("b", 20), rep("c", 5))
#'  mosaicVisual(ABCvar, "ABCvar", TRUE)
#'  
#' #Create a character string with the code for a mosaic plot:
#'  mosaicVisual(ABCvar, "ABCVar", FALSE)
#' 
#' #Extract or set description of a visualFunction:
#'  description(mosaicVisual)
#'  description(mosaicVisual) <- "A cubist version of a pie chart"
#'  description(mosaicVisual)
#'
#' 
#' @export
visualFunction <- function(f, description, classes = NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "visualFunction")
}


