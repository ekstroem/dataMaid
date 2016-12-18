#' A checkFunction for identifying outliers
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable.
#'
#' @param v A numeric or integer variable to check.
#' 
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{Inf}, in whichall problematic values are included 
#' in the outputted message.
#' 
#' @inheritParams clean
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
#' @return A \code{\link{checkResult}} with three entires: 
#' \code{$problem} (a logical indicating whether outliers were found),
#' \code{$message} (a message describing which values are outliers) and 
#' \code{$problemValues} (the outlier values).  
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}}, 
#' \code{\link{checkFunction}}, \code{\link{checkResult}}, \code{\link{mc}}
#'
#' @examples
#'  identifyOutliers(c(1:10, 200, 200, 700))
#'
#' @importFrom stats na.omit quantile
#' @export
identifyOutliers <- function(v, nMax = Inf, maxDecimals = 2) UseMethod("identifyOutliers")


#add methods to generic identifyOutliers function
#' @export
identifyOutliers.numeric <- function(v, nMax = Inf, maxDecimals = 2) {
  identifyOutliersNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliers.integer <- function(v, nMax = Inf, maxDecimals = 2) {
  identifyOutliersNI(v, nMax = nMax, maxDecimals = maxDecimals)
}


#make it a checkFunction
#' @include checkFunction.R
identifyOutliers <- checkFunction(identifyOutliers, "Identify outliers",
                                  c("integer", "numeric"))


##########################################Not exported below#########################################


##numerical and integer variables
#' @importFrom robustbase mc
identifyOutliersNI <- function(v, nMax, maxDecimals) {
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
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues= problemValues),
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, 
                   problemValues = outProblemValues))
}
