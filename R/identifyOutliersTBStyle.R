#' A checkFunction for identifying outliers Turkey Boxstole style
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer/Date variable by use of the Turkey Boxplot method (consistent witht the
#' \code{\link{boxplot}} function).
#'
#' @param v A numeric, integer or Date variable to check.
#' 
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#' 
#' @inheritParams makeDataReport
#'
#' @details Outliers are defined in the style of Turkey Boxplots (consistent with the
#' \code{\link{boxplot}} function), i.e. as values  that are smaller than the 1st quartile minus
#' the inter quartile range (IQR) or greater than  the third quartile plus the IQR. 
#' 
#' For Date variables, the calculations are done on their raw numeric format (as 
#' obtained by using \code{\link{unclass}}), after which they are translated back to Dates.
#' Note that no rounding is performed for Dates, no matter the value of \code{maxDecimals}. 
#'
#' @return A \code{\link{checkResult}} with three entires: 
#' \code{$problem} (a logical indicating whether outliers were found),
#' \code{$message} (a message describing which values are outliers) and 
#' \code{$problemValues} (the outlier values).  
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}}, 
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#' 
#' @examples
#'  identifyOutliersTBStyle(c(1:10, 200, 200, 700))
#'
#' @importFrom stats na.omit quantile
#' @export
identifyOutliersTBStyle <- function(v, nMax = 10, maxDecimals = 2) UseMethod("identifyOutliersTBStyle")


#add methods to generic identifyOutliers function
#' @export
identifyOutliersTBStyle.numeric <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersTBStyleNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliersTBStyle.integer <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersTBStyleNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliersTBStyle.Date <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersTBStyleD(v, nMax = nMax, maxDecimals = maxDecimals)
}


#make it a checkFunction
#' @include checkFunction.R
identifyOutliersTBStyle <- checkFunction(identifyOutliersTBStyle, 
                                         "Identify outliers (Turkish Boxplot style)",
                                         c("Date", "integer", "numeric"))



##########################################Not exported below#########################################

identifyOutliersTBStyleMessage <- "Note that the following possible outlier values were detected:"

##numerical and integer variables
identifyOutliersTBStyleNI <- function(v, nMax, maxDecimals) {
  res <- findOutlierTBstyle(v, maxDecimals)
  outMessage <- messageGenerator(list(problem = res$problem,
                                      problemValues = res$problemValues),
                                 message = identifyOutliersTBStyleMessage,
                                 nMax = nMax)
  checkResult(list(problem = res$problem, message = outMessage, 
                   problemValues = res$outProblemValues))
}


#Note: This is what they do in the boxplot.default function to 
#handle dates (among other things). Seems to work fine, as Dates
#have a one-to-one translation to numbers.
identifyOutliersTBStyleD <- function(v, nMax, maxDecimals) {
  v <- unclass(v)
  res <- findOutlierTBstyle(v, Inf) #rounding does nothing on Dates
  if (res$problem) { #otherwise, problemvalues are NULL
    class(res$problemValues) <-  class(res$outProblemValues) <- "Date"
  }
  outMessage <- messageGenerator(list(problem = res$problem,
                                      problemValues = res$problemValues),
                                 message = identifyOutliersTBStyleMessage,
                                 nMax = nMax)
  checkResult(list(problem = res$problem, message = outMessage, 
                   problemValues = res$outProblemValues))
}



#Find the outliers
findOutlierTBstyle <- function(v, maxDecimals) {
  v <- na.omit(v)
  qs <- quantile(v, c(0.25, 0.75))
  IQR <- qs[2] - qs[1]
  outlierPlaces <- v < qs[1]-1.5*IQR | v > qs[2]+1.5*IQR
  
  if (any(outlierPlaces)) {
    problem <- TRUE
    outProblemValues <- unique(v[outlierPlaces])
    problemValues <- round(outProblemValues, maxDecimals)
  } else {
    problem <- FALSE
    problemValues <- outProblemValues <- NULL
  }
  list(problem = problem, outProblemValues = outProblemValues,
       problemValues = problemValues)
}
