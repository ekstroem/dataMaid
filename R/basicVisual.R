#' Produce distribution plots in the base R (graphics) style using \code{\link{plot}} and
#' \code{\link{barplot}}
#'
#' Plot the distribution of a variable, depending on its data class, using the base R
#' plotting functions. Note that \code{basicVisual} is a \code{\link{visualFunction}}, compatible with the 
#' \code{\link{visualize}} and \code{\link{makeDataReport}} functions. 
#'
#' For character, factor, logical and labelled variables, a barplot is produced. For numeric, 
#' integer or Date variables, \code{basicVisual} produces a histogram instead. Note that for
#' integer and numeric variables, all non-finite (i.e. \code{NA}, \code{NaN}, \code{Inf}) values are
#' removed prior to plotting. For character, factor, labelled and logical variables, only \code{NA}
#' values are removed.
#'
#' @inheritParams standardVisual
#'
#' @examples
#'  \dontrun{
#'  #Save a variable
#'    myVar <- c(1:10)
#'  #Plot a variable
#'    basicVisual(myVar, "MyVar")
#'
#'  #Produce code for plotting a variable
#'    basicVisual(myVar, "MyVar", doEval = FALSE)
#'  }
#' @seealso \code{\link{visualize}}, \code{\link{standardVisual}}
#'
#' @inheritParams standardVisual
#' @importFrom stats na.omit
#' @importFrom graphics plot hist plot.new text
#' @export
basicVisual <- function(v, vnam, doEval = TRUE) UseMethod("basicVisual")


#Assign methods to generic standardVisual function

#' @export
basicVisual.default <- function(v, vnam, doEval = TRUE) {
  thisCall <- call("graphicsEmptyPlot", v = v)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}
  
#' @export
basicVisual.character <- function(v, vnam, doEval = TRUE) basicVisualCFLB(v, vnam, doEval=doEval)

#' @export
basicVisual.factor <- function(v, vnam, doEval = TRUE) basicVisualCFLB(v, vnam, doEval=doEval)

#' @export
basicVisual.labelled <- function(v, vnam, doEval = TRUE) basicVisualCFLB(haven::as_factor(v), 
                                                                         vnam, doEval=doEval)

#' @export
basicVisual.numeric <- function(v, vnam, doEval = TRUE) basicVisualIN(v, vnam, doEval=doEval)

#' @export
basicVisual.integer <- function(v, vnam, doEval = TRUE) basicVisualIN(v, vnam, doEval=doEval)

#' @export
basicVisual.logical <- function(v, vnam, doEval = TRUE) basicVisualCFLB(v, vnam, doEval=doEval)

#' @export
basicVisual.Date <- function(v, vnam, doEval = TRUE) basicVisualD(v, vnam, doEval = doEval)

#Make it a visualFunction
#' @include visualFunction.R 
basicVisual <- visualFunction(basicVisual, "Histograms and barplots using graphics",
                              allClasses())




##########################################Not exported below#########################################

##character, factor, labelled and logical variables
#' importFrom stats na.omit
#' @inheritParams standardVisual
basicVisualCFLB <- function(v, vnam, doEval = TRUE) {
  v <- escapeRStyle(na.omit(v))
  if (identifyNums(v, nVals = 0)$problem) {
    v <- as.numeric(as.character(v))
  }
  v <- factor(v)
  aggrV <- table(v)
  thisCall <- call("barplot", height = aggrV, main = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
basicVisualIN <- function(v, vnam, doEval = TRUE) {
  v <- v[is.finite(v)]
  aggrV <- hist(v, plot = FALSE)
  thisCall <- call("plot", aggrV, main = vnam, col = "grey", xlab = "")
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}


#Date variables
basicVisualD <- function(v, vnam, doEval = TRUE) {
  thisCall <- call("hist", v, main = vnam, col = "grey", xlab = "", 
                   breaks = 8)
    #Note: must specify breaks in hist.Date()
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}


#Make empty plot
graphicsEmptyPlot <- function(v) {  
  vClass <- class(v)[1]
  plot.new() + text(x = 0.5, y = 0.5, offset = 0, 
                    labels = paste("No plot available for variables",
                                   "of class:", vClass))
}