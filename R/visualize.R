#' @title Produce distribution plots
#'
#' @description Generic shell function that calls a plotting function in order to produce a marginal
#' distribution plot for a variable (or for each variable in a dataset). What type of plot is made 
#' might depend on the data class of the variable.
#'
#' @param v The variable (vector) or dataset (data.frame) which is to be plotted.
#'
#' @param vnam The name of the variable. This name might be printed on the plots, depending on the
#' choice of plotting function. If not supplied, it will default to the name of \code{v}.
#'
#' @param allVisuals The name (as a character string) of the actual plotting function to be used, no
#' matter the class of the variable. Note that if \code{v} is a single variable, this option is 
#' overwritten if a non-null class specific plotting function is supplied in \code{...} 
#' (see \emph{details} below). However, if \code{v} is a dataset, the function chosen in 
#' \code{allVisuals} will decide what plots are made.
#'
#' @param doEval A logical. If \code{TRUE} (the default), \code{visualize} has the side effect of
#' producing a plot (or multiple plots, if \code{v} is a data.frame). Otherwise,
#' visualize returns a character string containing R-code for producing the plot (or, when \code{v} is
#' a data.frame, a list of such character strings).
#'
#' @param ... Additional arguments used for class-specific choices of visual functions 
#' (see \emph{details}).
#'
#' @details Visual functions can be  supplied using their names (in character strings) in 
#' the class-specific arguments, e.g.\code{characterVisual = "standardVisual"}, ..., 
#' \code{logicalVisual = "standardVisual"} for each of the 7 data classes \code{character}, 
#' \code{Date}, \code{factor}, \code{labelled}, \code{numeric}, \code{integer} and \code{logical}. 
#' 
#' Note that an overview of all available \code{visualFunction}s can be obtained by calling
#' \code{\link{allVisualFunctions}}. 
#'
#' A user defined visual function can be supplied using its function name. Details on how 
#' to construct valid visual functions are found in \code{\link{visualFunction}}.
#' 
#' @examples
#'  #Standard use: Return standalone code for plotting a function:
#'    visualize(c(1:10), "Variable 1", doEval = FALSE)
#'
#'  #Define a new visualization function and call it using visualize either
#'  #using allVisual or a class specific argument:
#'    mosaicVisual <- function(v, vnam, doEval) {
#'      thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
#'      if (doEval) {
#'        return(eval(thisCall))
#'      } else return(deparse(thisCall))
#'    }
#'    mosaicVisual <- visualFunction(mosaicVisual, 
#'                                   description = "Mosaicplots from graphics",
#'                                   classes = allClasses())
#'
#'  \dontrun{
#'    #use allVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'        allVisuals = "mosaicVisual")
#'
#'    #use characterVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'       characterVisual = "mosaicVisual")
#'
#'    #this will use standardVisual:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'        numericVisual = "mosaicVisual")
#'  }
#'
#'    #return code for a mosaic plot
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'        allVisuals = "mosaicVisual", doEval=FALSE)
#'
#'  \dontrun{
#'  #Produce multiple plots easily by calling visualize on a full dataset:
#'    data(testData)
#'    testData2 <- testData[, c("charVar", "factorVar", "numVar", "intVar")]
#'    visualize(testData2)
#'    
#'  #When using visualize on a dataset, datatype specific arguments have no
#'  #influence:
#'    visualize(testData2, factorVisual = "basicVisual", characterVisual = "basicVisual")
#'    
#'  #But the "allVisuals" argument still works:
#'    visualize(testData2, allVisuals = "basicVisual")
#'  }
#'
#'@seealso \code{\link{standardVisual}}, \code{\link{basicVisual}}
#'@export
visualize <- function(v, vnam = NULL, allVisuals = "standardVisual",
                      doEval = TRUE, ...) UseMethod("visualize")






#Methods for each variable type

#' @export
visualize.character <- function(v, vnam = NULL, allVisuals = "standardVisual",
                                doEval = TRUE, characterVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(characterVisual), allVisuals, characterVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}

#' @export
visualize.factor <- function(v, vnam = NULL, allVisuals = "standardVisual",
                             doEval = TRUE, factorVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(factorVisual), allVisuals, factorVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}

#' @export
visualize.labelled <- function(v, vnam = NULL, allVisuals = "standardVisual",
                               doEval = TRUE, labelledVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(labelledVisual), allVisuals, labelledVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}


#' @export
visualize.numeric <- function(v, vnam = NULL, allVisuals = "standardVisual",
                              doEval = TRUE, numericVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(numericVisual), allVisuals, numericVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}

#' @export
visualize.integer <- function(v, vnam = NULL, allVisuals = "standardVisual",
                              doEval = TRUE, integerVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(integerVisual), allVisuals, integerVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}

#' @export
visualize.logical <- function(v, vnam = NULL, allVisuals = "standardVisual",
                              doEval = FALSE, logicalVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(logicalVisual), allVisuals, logicalVisual)
  eval(call(useVisual, v, vnam, doEval))
}

#' @export
visualize.Date <- function(v, vnam = NULL, allVisuals = "standardVisual",
                           doEval = TRUE, DateVisual = NULL, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- ifelse(is.null(DateVisual), allVisuals, DateVisual)
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}


#' @importFrom gridExtra marrangeGrob
#' @export 
visualize.data.frame <- function(v, vnam = NULL, allVisuals = "standardVisual", 
                                 doEval = TRUE, ...) {
  
  #note: all plots have to be of the same type, hence only the allVisuals 
  #argument is passed on to the variable level visualizations
  plots <- lapply(names(v), function(x) {visualize(v[[x]], x, allVisuals = allVisuals,
                  doEval = doEval)})
  if (doEval) {
    if (allVisuals == "standardVisual") {
      out <- marrangeGrob(plots, nrow=3, ncol=2)
      return(out)
    } 
    #note: if allVisuals != "standardVisual", the plots should have been produced as 
    #a sideeffect in the lapply above. In this case, nothing is returned.
    
  } else {
    out <- plots
    return(out)
  }
}

