#' @title Construct a visualFunction  
#'
#' @description Convert a function into class \code{visualFunction} used for creating variable 
#' visualizations \code{\link{visualize}}, typically called from \code{\link{clean}}.
#' 
#' @param f A function. In order to be a valid \code{visualFunction}, \code{f} should 
#' follow the structure outlined in details below.
#' 
#' @inheritParams summaryFunction
#' 
#' @return A function of class \code{visualFunction}, i.e. with two attributes: A description 
#' ("description") and a vector of classes which the function can be called upon ("classes"). A 
#' visualFunction always 
#' 
#' 
#'  
#' @details We recommend using the following structure for defining new \code{visualFunction}s:
#' FIND A WAY TO FIX FORMATTING HERE. GOTTA BE POSSIBLE. 
#' #' \code{
#'   myVisualFunction <- function(v, vnam, doEval) { 
#'    thisCall <- call("[the name of the function used to produce the plot]", 
#'                     v, [additional arguments to the plotting function])
#'    if (doEval) {
#'     return(eval(thisCall))
#'    } else return(deparse(thisCall)
#'   }
#'  }
#'  followed by a class change: 
#'  \code{
#'    myVisualFunction <- visualFunction(myVisualFunction, description = "[describe function]",
#'                                         classes = c([classes]))
#'  }
#'  It is not strictly necessary to follow this exact template, but the input/output structure 
#'  should be respected - in other words, the arguments are mandatory and the function needs
#'  to return a character string containing code (if \code{doEval = FALSE}) or have the 
#'  side effect of producing a plot (if \code{doEval = TRUE}). See examples below for an example
#'  of how the template can be used in practice.  
#'  
#'  Note that it is not necessary to formally change functions to be used by \code{\link{visualize}}
#'  into \code{visualFunction}s, but it is recommended, as the functions are then added to the output 
#'  of \code{allVisualFunctions()}, making it easier to gain an overview of the visualization 
#'  functions available for a \code{clean()} run.
#'        
#'
#' @seealso \code{link{allVisualFunctions}} \code{\link{visualize}} 
#' \code{\link{description}} \code{\link{classes}} 
#'   
#' @examples 
#' #Defining a new visualFunction:
#'  mosaicVisual <- function(v, vnam, doEval) {
#'    thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
#'    if (doEval) {
#'     return(eval(thisCall))
#'    } else return(deparse(thisCall))
#'  }
#'  mosaicVisual <- visualFunction(mosaicVisual, description = "mosaicplots from graphics",
#'                                 classes = c("character", "factor", "labelled", "numeric",
#'                                             "integer", "logical"))
#'
#' #Extract or set description of a visualFunction:
#'  description(mosaicVisual) 
#'  description(mosaicVisual) <- "A cubist version of a pie chart" 
#'  description(mosaicVisual)
#'  
#' #Extract or set classes of a visualFunction:
#'  classes(mosaicVisual)
#'  classes(mosaicVisual) <- "factor"
#'  classes(mosaicVisual)
#' 
#' @export
visualFunction <- function(f, description, classes = NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "visualFunction")
}


#'
#' document me!
#'
#' @export
allVisualFunctions <- function() {
  allXFunctions("visualFunction")
}


