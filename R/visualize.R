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
#' @param visuals A list of visual functions to use on each supported variable type. We recommend
#' using \code{\link{setVisuals}} for creating this list and refer to the documentation
#' of this function for more details. This function allows for choosing variable-type dependent 
#' visuals. However, if \code{visualize()} is called on a full dataset, all visualizations
#' must be of the same type and therefore, the \code{all} argument of \code{setVisuals} is used.
#'
#' @param doEval A logical. If \code{TRUE} (the default), \code{visualize} has the side effect of
#' producing a plot (or multiple plots, if \code{v} is a data.frame). Otherwise,
#' visualize returns a character string containing R-code for producing the plot (or, when \code{v} is
#' a data.frame, a list of such character strings).
#'
#' @param ... Additional arguments used for class-specific choices of visual functions 
#' (see \emph{details}).
#'
#' @details Visual functions can be  supplied using their names (in character strings) using
#' \code{setVisuals}. Note that only a single visual function is allowed for each variable class.
#' The default visual settings can be inspected by calling \code{setVisuals()}.  
#' An overview of all available \code{visualFunction}s can be obtained by calling
#' \code{\link{allVisualFunctions}}. 
#'
#' A user defined visual function can be supplied using its function name. Details on how 
#' to construct valid visual functions are found in \code{\link{visualFunction}}.
#' 
#' @references Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \url{https://doi.org/10.18637/jss.v090.i06}).
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
#'   #Inspect all options for visualFunctions:
#'   allVisualFunctions()
#'
#'
#'  \dontrun{
#'    #set mosaicVisual for all variable types:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'        visuals = setVisuals(all = "mosaicVisual"))
#'
#'    #set mosaicVisual only for character variables:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'       visuals = setVisuals(character = "mosaicVisual"))
#'
#'    #this will use standardVisual, as our variable is not numeric:
#'    visualize(c("1", "1", "1", "2", "2", "a"), "My variable", 
#'        visuals = setVisuals(numeric = "mosaicVisual"))
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
#'    visualize(testData2, setVisuals(character = "basicVisual", 
#'        factor = "basicVisual"))
#'    
#'  #But we can still use the "all" argument in setVisuals:
#'    visualize(testData2, visuals =  setVisuals(all = "basicVisual"))
#'  }
#'
#'@seealso \code{\link{setVisuals}}, \code{\link{allVisualFunctions}},
#'\code{\link{standardVisual}}, \code{\link{basicVisual}}
#'@export
visualize <- function(v, vnam = NULL, visuals = setVisuals(),
                      doEval = TRUE, ...) UseMethod("visualize")






#Methods for each variable type

#Catch non-supported classes, do nothing and throw a warning:
#' @export
visualize.default <-  function(v, vnam = NULL, visuals = setVisuals(),
                               doEval = TRUE, ...) {
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  useVisual <- visuals[[1]]
  eval(call(useVisual, v = v, vnam = vnam, doEval = doEval))
}


#' @export
visualize.character <- function(v, vnam = NULL, visuals = setVisuals(),
                                doEval = TRUE, characterVisual = NULL, ...) {
  if (is.null(characterVisual)) characterVisual <- visuals$character 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(characterVisual, v = v, vnam = vnam, doEval = doEval))
}

#' @export
visualize.factor <- function(v, vnam = NULL, visuals = setVisuals(),
                             doEval = TRUE, factorVisual = NULL, ...) {
  if (is.null(factorVisual)) factorVisual <- visuals$factor 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(factorVisual, v = v, vnam = vnam, doEval = doEval))}

#' @export
visualize.labelled <- function(v, vnam = NULL, visuals = setVisuals(),
                               doEval = TRUE, labelledVisual = NULL, ...) {
  if (is.null(labelledVisual)) labelledVisual <- visuals$labelled 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(labelledVisual, v = v, vnam = vnam, doEval = doEval))}

#' @export
visualize.haven_labelled <- function(v, vnam = NULL, visuals = setVisuals(),
                               doEval = TRUE, havenlabelledVisual = NULL, ...) {
  if (is.null(havenlabelledVisual)) havenlabelledVisual <- visuals$haven_labelled 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(havenlabelledVisual, v = v, vnam = vnam, doEval = doEval))}


#' @export
visualize.numeric <- function(v, vnam = NULL, visuals = setVisuals(),
                              doEval = TRUE, numericVisual = NULL, ...) {
  if (is.null(numericVisual)) numericVisual <- visuals$numeric 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(numericVisual, v = v, vnam = vnam, doEval = doEval))}

#' @export
visualize.integer <- function(v, vnam = NULL, visuals = setVisuals(),
                              doEval = TRUE, integerVisual = NULL, ...) {
  if (is.null(integerVisual)) integerVisual <- visuals$integer 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(integerVisual, v = v, vnam = vnam, doEval = doEval))}

#' @export
visualize.logical <- function(v, vnam = NULL, visuals = setVisuals(),
                              doEval = FALSE, logicalVisual = NULL, ...) {
  if (is.null(logicalVisual)) logicalVisual <- visuals$logical 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(logicalVisual, v = v, vnam = vnam, doEval = doEval))}

#' @export
visualize.Date <- function(v, vnam = NULL, visuals = setVisuals(),
                           doEval = TRUE, DateVisual = NULL, ...) {
  if (is.null(DateVisual)) DateVisual <- visuals$Date 
  if (is.null(vnam)) vnam <- deparse(substitute(v))
  eval(call(DateVisual, v = v, vnam = vnam, doEval = doEval))}


#' @importFrom gridExtra marrangeGrob
#' @export 
visualize.data.frame <- function(v, vnam = NULL, visuals = setVisuals(), 
                                 doEval = TRUE, ...) {
  
  #note: all plots have to be of the same type, hence only the first 
  #choice of visual is passed on to the variable level visualizations
  useVisuals <- setVisuals(all = visuals[[1]])
  if(length(unique(visuals)) > 1) warning(paste("When visualizing a dataset,",
                                                "all variable types must use the",
                                                "same visualization, however, more",
                                                "than one visualization was supplied.",
                                                "Only the first was used."))
  
  plots <- lapply(names(v), function(x) {visualize(v[[x]], x, visuals = useVisuals,
                  doEval = doEval)})
  if (doEval) {
    if (useVisuals[[1]] == "standardVisual") {
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

