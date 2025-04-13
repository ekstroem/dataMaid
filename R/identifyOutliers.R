#' A checkFunction for identifying outliers
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a Date/numeric/integer variable.
#'
#' @param v A Date, numeric or integer variable to check.
#' 
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#' 
#' @inheritParams makeDataReport
#'
#' @details Outliers are identified based on an outlier rule that is 
#' appropriate for asymmetric data. Outliers are observations outside the range
#'
#' \deqn{Q1 - 1.5*exp(a*MC)*IQR ;  Q3 + 1.5*exp(b*MC)*IQR }
#'
#' where Q1, Q3, and IQR are the first quartile, third quartile, and
#' inter-quartile range, MC is the 'medcouple', a robust concept and
#' estimator of skewness, and a and b are appropriate constants (-4
#' and 3).  The medcouple is defined as a scaled median difference of
#' the left and right half of distribution, and hence not based on the
#' third moment as the classical skewness.
#'
#' When the data are symmetric, the measure reduces to the
#' standard outlier rule also used in Tukey Boxplots (consistent with
#' the \code{\link{boxplot}} function), i.e. as values that are
#' smaller than the 1st quartile minus the inter quartile range (IQR)
#' or greater than the third quartile plus the IQR.
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
#' \code{\link{checkFunction}}, \code{\link{checkResult}}, \code{\link[robustbase]{mc}}
#'
#' @examples
#'  identifyOutliers(c(1:10, 200, 200, 700))
#'
#' @importFrom stats na.omit quantile
#' @export
identifyOutliers <- function(v, nMax = 10, maxDecimals = 2) UseMethod("identifyOutliers")


#add methods to generic identifyOutliers function
#' @export
identifyOutliers.numeric <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliers.integer <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliers.Date <- function(v, nMax = 10, maxDecimals = 2) {
  identifyOutliersD(v, nMax = nMax, maxDecimals = maxDecimals)
}



#make it a checkFunction
#' @include checkFunction.R
identifyOutliers <- checkFunction(identifyOutliers, "Identify outliers",
                                  c("Date", "integer", "numeric"))


##########################################Not exported below#########################################

identifyOutliersMessage <- "Note that the following possible outlier values were detected:"

##numerical and integer variables
identifyOutliersNI <- function(v, nMax, maxDecimals) {
  res <- findOutliers(v, maxDecimals)
  outMessage <- messageGenerator(list(problem = res$problem,
                                      problemValues = res$problemValues),
                                 message = identifyOutliersMessage,
                                 nMax = nMax)
  checkResult(list(problem = res$problem, message = outMessage, 
                   problemValues = res$outProblemValues))
}


#Date variables
#Note: This is what they do in the boxplot.default function to 
#handle dates (among other things). Seems to work fine, as Dates
#have a one-to-one translation to numbers.
identifyOutliersD <- function(v, nMax, maxDecimals) {
  v <- unclass(v)
  res <- findOutliers(v, Inf) #rounding does nothing on Dates
  if (res$problem) { #otherwise, problemvalues are NULL
    class(res$problemValues) <-  class(res$outProblemValues) <- "Date"
  }
  outMessage <- messageGenerator(list(problem = res$problem,
                                      problemValues = res$problemValues),
                                 message = identifyOutliersMessage,
                                 nMax = nMax)
  checkResult(list(problem = res$problem, message = outMessage, 
                   problemValues = res$outProblemValues))
}


##Find the outliers
#' @importFrom robustbase mc
findOutliers <- function(v, maxDecimals) {
  v <- na.omit(v)
  qs <- quantile(v, c(0.25, 0.75))
  IQR <- qs[2] - qs[1]
  outlierPlaces <- v < qs[1]-1.5*IQR | v > qs[2]+1.5*IQR
  
  ## Medcouple adjusted area
  ## CE: This block can be removed to use standard outlier detection
  ## [Q1 – c * exp(-b * MC) * IQD, Q3 + c * exp(-a * MC) * IQD
  ## c=1.5, a=-4, b=3
  lowConst <- -4
  highConst <- 3
  MC <- robustbase::mc(v)
  if (MC<0) {
    lowConst <- -3
    highConst <- 4
  }
  outlierPlaces <- v < qs[1]-1.5*exp(lowConst*MC)*IQR | v > qs[2]+1.5*exp(highConst*MC)*IQR
  
  if (any(outlierPlaces)) {
    problem <- TRUE
    
    #only print each outlier value once:
    outProblemValues <- unique(v[outlierPlaces])
    
    problemValues <- round(outProblemValues, maxDecimals)
  } else {
    problem <- FALSE
    problemValues <- outProblemValues <- NULL
  }
  list(problem = problem, outProblemValues = outProblemValues,
       problemValues = problemValues)
}
