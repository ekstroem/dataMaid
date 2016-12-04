#' Produce distribution plots using ggplot from ggplot2.
#'
#' Plot the distribution of a variable, depending on its data class, by use of ggplot2.
#'
#' For character, factor, logical and labelled variables, a barplot is produced. For numeric
#' or integer variables, \code{standardVisual} produces a histogram instead. Note that for
#' integer and numeric variables, all non-finite (i.e. \code{NA}, \code{NaN}, \code{Inf}) values are
#' removed prior to plotting. For character, factor, labelled and logical variables, only \code{NA}
#' values are removed.
#'
#' @param v The variable (vector) to be plotted.
#' @param vnam The name of the variable which will appear as the title of the plot.
#' @param doEval If TRUE, the plot itself is returned. Otherwise, the function returns
#' a character string containing standalone R code for producing the plot.
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
#' @importFrom ggplot2 qplot
#' @importFrom stats na.omit
## #' @export
standardVisual <- function(v, vnam, doEval = TRUE) UseMethod("standardVisual")

#assign methods to generic standardVisual function

#' @export
standardVisual.character <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)

#' @export
standardVisual.factor <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)

#' @export
standardVisual.labelled <- function(v, vnam, doEval = TRUE) standardVisualCFLB(haven::as_factor(v), vnam, doEval=doEval)

#' @export
standardVisual.numeric <- function(v, vnam, doEval = TRUE) standardVisualIN(v, vnam, doEval=doEval)

#' @export
standardVisual.integer <- function(v, vnam, doEval = TRUE) standardVisualIN(v, vnam, doEval=doEval)

#' @export
standardVisual.logical <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)


#' @export
standardVisual.Date <- function(v, vnam, doEval = TRUE) standardVisualD(v, vnam, doEval=doEval)





#' @export
standardVisual <- visualFunction(standardVisual, "Histograms and barplots using ggplot2")


##########################################Not exported below#########################################

#character, factor, labelled and logical variables
standardVisualCFLB <- function(v, vnam, doEval = TRUE) {
  thisCall <- call("qplot", x=na.omit(v), geom="bar", xlab="", main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
standardVisualIN <- function(v, vnam, doEval = TRUE) {
  v <- v[is.finite(v)]
  thisCall <- call("qplot", x=na.omit(v), geom="histogram", xlab="",
                   main=vnam, bins=20)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
} #fix such that no stat_bin()-message is produced, it's annoying. And also a clever
#choice of binwidth, how does it work in hist()?



#ggplot(dates, aes(x=Date, y = ..count.. ))  + geom <- bar()
# Dates
standardVisualD <- function(v, vnam, doEval = TRUE) {
      thisCall <- call("qplot", x=na.omit(v), geom="histogram", xlab="",
                   main=vnam, bins=20)

#  thisCall <- call("ggplot", data=na.omit(v), aes=geom="bar", xlab="", main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

