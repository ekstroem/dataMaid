#' @title Produce distribution plots in the base R style using \code{\link{plot}} and
#' \code{\link{barplot}}
#'
#' @description Plot the distribution of a variable, depending on its data class, using the base R
#' plotting functions.
#'
#' @inheritParams standardVisual
#'
#' @details For character, factor, logical and labelled variables, a barplot is produced. For numeric
#' or integer variables, \code{basicVisual} produces a histogram instead. Note that for
#' integer and numeric variables, all non-finite (i.e. \code{NA}, \code{NaN}, \code{Inf}) values are
#' removed prior to plotting. For character, factor, labelled and logical variables, only \code{NA}
#' values are removed.
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
#' @export
basicVisual <- function(v, vnam, doEval = TRUE) UseMethod("basicVisual")





##########################################Not exported below#########################################

##character, factor, labelled and logical variables
#' importFrom stats na.omit
basicVisualCFLB <- function(v, vnam, doEval=TRUE) {
  v <- as.factor(v)
  thisCall <- call("plot", x=na.omit(v), main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
basicVisualIN <- function(v, vnam, doEval=TRUE) {
  v <- v[is.finite(v)]
  thisCall <- call("hist", v, main=vnam, col="grey", xlab="")
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#assign methods to generic standardVisual function
basicVisual.character <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.factor <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.labelled <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.numeric <- function(v, vnam, doEval=T) basicVisualIN(v, vnam, doEval=doEval)
basicVisual.integer <- function(v, vnam, doEval=T) basicVisualIN(v, vnam, doEval=doEval)
basicVisual.logical <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)


