#' @title Produce distribution plots using \code{\link{ggplot}} from ggplot2
#'
#' @description Plot the distribution of a variable, depending on its data class, by use of ggplot2.
#'
#' @param v The variable (vector) to be plotted.
#' @param vnam The name of the variable which will appear as the title of the plot.
#' @param doEval If TRUE, the plot itself is returned. Otherwise, the function returns
#' a character string containing standalone R code for producing the plot.
#'
#' @details For character, factor, logical and labelled variables, a barplot is produced. For numeric
#' or integer variables, \code{standardVisual} produces a histogram instead. Note that for
#' integer and numeric variables, all non-finite (i.e. \code{NA}, \code{NaN}, \code{Inf}) values are
#' removed prior to plotting. For character, factor, labelled and logical variables, only \code{NA}
#' values are removed.
#'
#' @examples
#' \dontrun{
#' #Save a variable
#' myVar <- c(1:10)
#' #Plot a variable
#' standardVisual(myVar, "MyVar")
#' #Produce code for plotting a variable
#' standardVisual(myVar, "MyVar", doEval = FALSE)
#' }
#' @seealso \code{\link{visualize}}, \code{\link{basicVisual}}
#'
#' @importFrom ggplot2 ggplot
#'
#' @export
standardVisual <- function(v, vnam, doEval = TRUE, ...) UseMethod("standardVisual")






##########################################Not exported below#########################################

#character, factor, labelled and logical variables
standardVisualCFLB <- function(v, vnam, doEval=T, ...) {
  thisCall <- call("qplot", x=na.omit(v), geom="bar", xlab="", main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
standardVisualIN <- function(v, vnam, doEval=T) {
  v <- v[is.finite(v)]
  thisCall <- call("qplot", x=na.omit(v), geom="histogram", xlab="",
                   main=vnam, bins=20)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
} #fix such that no stat_bin()-message is produced, it's annoying. And also a clever
#choice of binwidth, how does it work in hist()?


#assign methods to generic standardVisual function
standardVisual.character <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.factor <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.labelled <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.numeric <- function(v, vnam, doEval=T, smartNum=T) standardVisualIN(v, vnam,
                                                                                   doEval=doEval)
standardVisual.integer <- function(v, vnam, doEval=T, smartNum=T) standardVisualIN(v, vnam,
                                                                                   doEval=doEval)
standardVisual.logical <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
