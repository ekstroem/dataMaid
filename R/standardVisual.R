#' Produce distribution plots using ggplot from ggplot2.
#'
#' Plot the distribution of a variable, depending on its data class, by use of ggplot2. 
#' Note that \code{standardVisual} is a \code{\link{visualFunction}}, compatible with the 
#' \code{\link{visualize}} and \code{\link{makeDataReport}} functions. 
#'
#' For character, factor, logical and (haven_)labelled variables, a barplot is produced. For numeric,
#' integer or Date variables, \code{standardVisual} produces a histogram instead. Note that for
#' integer and numeric variables, all non-finite (i.e. \code{NA}, \code{NaN}, \code{Inf}) values are
#' removed prior to plotting. For character, Date, factor, (haven_)labelled and logical variables, 
#' only \code{NA} values are removed.
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
#' 
#' #Plot a variable
#' standardVisual(myVar, "MyVar")
#' 
#' #Produce code for plotting a variable
#' standardVisual(myVar, "MyVar", doEval = FALSE)
#' }
#' @seealso \code{\link{visualize}}, \code{\link{basicVisual}}
#'
#' @importFrom ggplot2 qplot geom_bar geom_rect ylab xlab aes_string ggplot aes theme element_blank geom_text 
#' @importFrom stats na.omit
#' @export
standardVisual <- function(v, vnam, doEval = TRUE) UseMethod("standardVisual")

#assign methods to generic standardVisual function

#' @export 
standardVisual.default <- function(v, vnam, doEval = TRUE) {
  thisCall <- call("ggEmptyPlot", v = v, vnam = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#' @export
standardVisual.character <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)

#' @export
standardVisual.factor <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)

#' @export
standardVisual.labelled <- function(v, vnam, doEval = TRUE) standardVisualCFLB(dataMaid_as_factor(v), 
                                                                               vnam, doEval=doEval)
#' @export
standardVisual.haven_labelled <- function(v, vnam, doEval = TRUE) standardVisualCFLB(dataMaid_as_factor(v), 
                                                                               vnam, doEval=doEval)

#' @export
standardVisual.numeric <- function(v, vnam, doEval = TRUE) standardVisualIN(v, vnam, doEval=doEval)

#' @export
standardVisual.integer <- function(v, vnam, doEval = TRUE) standardVisualIN(v, vnam, doEval=doEval)

#' @export
standardVisual.logical <- function(v, vnam, doEval = TRUE) standardVisualCFLB(v, vnam, doEval=doEval)


#' @export
standardVisual.Date <- function(v, vnam, doEval = TRUE) standardVisualD(v, vnam, doEval=doEval)





#' @include visualFunction.R
standardVisual <- visualFunction(standardVisual, "Histograms and barplots using ggplot2",
                                 allClasses())


##########################################Not exported below#########################################

#character, factor, labelled and logical variables
standardVisualCFLB <- function(v, vnam, doEval = TRUE) {
  v <- escapeRStyle(na.omit(v))
  if (identifyNums(v, nVals = 0)$problem) {
    v <- as.numeric(as.character(v))
  }
  pf <- aggregateForBarplot(v)
  thisCall <- call("ggAggBarplot", data = pf, vnam = vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
standardVisualIN <- function(v, vnam, doEval = TRUE) {
  v <- v[is.finite(v)]
  pf <- aggregateForHistogram(v)
  thisCall <- call("ggAggHist", data = pf, vnam = vnam)
  #thisCall <- call("qplot", x=na.omit(v), geom="histogram", xlab="",
   #                main=vnam, bins=20)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
} 



# Dates
standardVisualD <- function(v, vnam, doEval = TRUE) {
  v <- na.omit(v)
  pf <- aggregateForHistogram(v)
  thisCall <- call("ggAggHist", data = pf, vnam = vnam)
#      thisCall <- call("qplot", x=na.omit(v), geom="bar", xlab="",
#                   main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}


#ggplot2 histogram based on aggregated data.
#Input should be on the format of the output of 
#aggregateForHistogram()
ggAggHist <- function(data, vnam) {
  p <- ggplot(data, aes_string(xmin = "xmin", xmax = "xmax", ymin = "ymin", ymax = "ymax")) +
    geom_rect() +
    ylab("count") +
    xlab(vnam)
  p
}

#ggplot2 barplot based on aggregated data.
#Input should be on the format of the output of 
#aggregateForBarplot()
ggAggBarplot <- function(data, vnam) {
  p <- ggplot(data, aes_string(x = "x", y = "y")) +
    geom_bar(stat = "identity") +
    ylab("count") +
    xlab(vnam)
  p
}

ggEmptyPlot <- function(v, vnam) {
  vClass <- class(v)[1]
  p <- ggplot(data.frame(x = 1, y = 1), aes_string(x = "x", y = "y")) +
    geom_text(aes(label = paste("No plot available for variables",
                                "of class:", vClass))) +
    theme(line = element_blank(),
          text = element_blank(),
          title = element_blank()) 
  p
}



