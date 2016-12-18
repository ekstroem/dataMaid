#' A checkFunction for identifying outliers Turkey Boxstole style
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable by use of the Turkey Boxplot method (consistent witht the
#' \code{\link{boxplot}} function).
#'
#' @param v A numeric or integer variable to check.
#' 
#' @param nMax The maximum number of problematic values to report. Default is \code{Inf}, in which case
#' all problematic values are included in the outputted message.
#' 
#' @inheritParams clean
#'
#' @details Outliers are defined in the style of Turkey Boxplots (consistent with the
#' \code{\link{boxplot}} function), i.e. as values  that are smaller than the 1st quartile minus
#' the inter quartile range (IQR) or greater than  the third quartile plus the IQR. 
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
identifyOutliersTBStyle <- function(v, nMax = Inf, maxDecimals = 2) UseMethod("identifyOutliersTBStyle")


#add methods to generic identifyOutliers function
#' @export
identifyOutliersTBStyle.numeric <- function(v, nMax = Inf, maxDecimals = 2) {
  identifyOutliersTBStyleNI(v, nMax = nMax, maxDecimals = maxDecimals)
}

#' @export
identifyOutliersTBStyle.integer <- function(v, nMax = Inf, maxDecimals = 2) {
  identifyOutliersTBStyleNI(v, nMax = nMax, maxDecimals = maxDecimals)
}


#make it a checkFunction
#' @include checkFunction.R
identifyOutliersTBStyle <- checkFunction(identifyOutliersTBStyle, 
                                         "Identify outliers (Turkish Boxplot style)",
                                         c("integer", "numeric"))



##########################################Not exported below#########################################


##numerical and integer variables
identifyOutliersTBStyleNI <- function(v, nMax, maxDecimals) {
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
  outMessage <- messageGenerator(list(problem = problem,
                                      problemValues = problemValues),
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, 
                   problemValues = outProblemValues))
}
