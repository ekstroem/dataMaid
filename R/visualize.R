#'@title Produce marginal distribution plots
#'
#'@description Generic shell function that calls a plotting function in order to produce a marginal 
#'distribution plot for a variable, depending on the class of the variable.
#'
#'@param v The variable (vector) which is to be plotted.
#'@param vnam The name of the variable. This name might be printed on the plots, depending on the 
#'choice of plotting function. If not supplied, it will default to the name of \code{v}.
#'@param allVisuals The name (as a character string) of the actual plotting function to be used, no 
#'matter the class of \code{v}. See \code{details} for more details about the structure of such plotting 
#'functions. Note that this option is overwritten if a non-null class specific 
#'plotting function is supplied in \code{...} (see \code{details}).Two options are readily available, 
#'\code{\link{standardVisual}} and \code{\link{\basicVisual}}. 
#'@param doEval If TRUE, \code{visualize} returns a plot (IS THIS THE CORRECT WAY TO SAY IT?). Otherwise,
#'visualize returns a character string containing R-code for producing a plot.
#'@param ... ALLOW FOR ARGUMENTS TO BE PASSED ON TO E.G. STANDARDVISUAL AS WELL? If the plotting 
#'function to be used should differ by variable type, this can be specified by using additional arguments
#'on the form \code{characterVisual = "standardVisual"}, ..., \code{logicalVisual = "standardVisual"} for 
#'each of the 6 data classes \code{character}, \code{factor}, \code{labelled}, \code{numeric}, 
#'\code{integer} and \code{logical}.
#'
#'@details The function supplied in \code{allVisuals} should take a variable, \code{v}, and its name, 
#'\code{vnam}, and an evaluation indicator, \code{doEval}, and return a character string containing 
#'(standalone) code for producing a plot if \code{doEval = FALSE} and a plot otherwise. 
#'See e.g. \code{\link{standardVisual}} for an example of such a plotting function and the example 
#'below.
#'
#'@examples 
#'  #Standard use: Return standalone code for plotting a function:
#'    visualize(c(1:10), "Variable 1", doEval = FALSE)
#'  
#'  
#'  #Define a new visualization function and call it using visualize either 
#'  #using allVisual or a class specific argument:
#'    pieVisual <- function(v, vnam, doEval) {
#'      thisCall <- call("pie", x=table(v), main=vnam)
#'      if (doEval) {
#'        return(eval(thisCall))
#'      } else return(deparse(thisCall))
#'    }
#'    
#'  \dontrun{
#'    #use allVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", allVisuals = "pieVisual")
#'    
#'    #use characterVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", characterVisual = "pieVisual")
#'    
#'    #this will use standardVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", numericVisual = "pieVisual")
#'  }
#'    
#'    #return code for a pie chart
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", allVisuals = "pieVisual", doEval=F)
#'  
#'  \dontrun{
#'  #Produce multiple plots easily, depending on data type
#'    data(testData)
#'    testData <- testData[, c("charVar", "factorVar", "numVar", "intVar")]
#'    par(mfrow = c(2, 2))
#'    plots <- lapply(testData, function(x) visualize(x, allVisual="basicVisual", doEval=T))
#'    par(mfrow = c(1, 1))
#'  }
#'
#'@seealso \code{\link{standardVisual}}, \code{\link{basicVisual}}
#'@export
visualize <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                      doEval = FALSE, ...) UseMethod("visualize")




##########################################Not exported below#########################################


#Methods for each variable type
visualize.character <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                                doEval = FALSE, characterVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(characterVisual), allVisuals, characterVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.factor <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                             doEval = FALSE, factorVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(factorVisual), allVisuals, factorVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.labelled <- function(v, vnam = NULL, allVisuals = "standardVisual",
                               doEval = FALSE, labelledVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(labelledVisual), allVisuals, labelledVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.numeric <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                              doEval = FALSE, numericVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(numericVisual), allVisuals, numericVisual)
  eval(call(useVisual, v, vnam, doEval=doEval))
}

visualize.integer <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                              doEval = FALSE, integerVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(integerVisual), allVisuals, integerVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.logical <- function(v, vnam = NULL, allVisuals = "standardVisual",
                              doEval = FALSE, logicalVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(logicalVisual), allVisuals, logicalVisual)
  eval(call(useVisual, v, vnam, doEval))
}


