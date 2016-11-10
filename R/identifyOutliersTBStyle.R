#' A checkFunction for identifying outliers Turkey Boxstole style
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable by use of the Turkey Boxplot method (consistent witht the
#' \code{\link{boxplot}} function).
#'
#' @param v A character or factor variable to check
#' @param nMax The maximum number of problematic values to report. Default is \code{Inf}, in which case
#' all problematic values are included in the outputtet message.
#'
#' @details Outliers are defined in the style of Turkey Boxplots (consistent with the
#' \code{\link{boxplot}} function), i.e. as values  that are smaller than the 1st quartile minus
#' the inter quartile range (IQR) or greater than  the third quartile plus the IQR. 
#'
#' @return A list with two elements, $problem: TRUE if any outliers were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were outliers. Note that outlier values
#' are only printed once (even if they appear multiple times) and that they are ordered.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#'
#' @examples
#'  identifyOutliers(c(1:10, 200, 200, 700))
#'
#' @importFrom stats na.omit quantile
#' @export
identifyOutliersTBStyle <- function(v, nMax = Inf) UseMethod("identifyOutliersTBStyle")


#add methods to generic identifyOutliers function
#' @export
identifyOutliersTBStyle.numeric <- function(v, nMax = Inf) identifyOutliersTBStyleNI(v, nMax = nMax)

#' @export
identifyOutliersTBStyle.integer <- function(v, nMax = Inf) identifyOutliersTBStyleNI(v, nMax = nMax)


#make it a checkFunction
identifyOutliersTBStyle <- checkFunction(identifyOutliersTBStyle, 
                                         "Identify outliers (Turkish Boxplot style)")



#######################
##########################################Not exported below#########################################


##numerical and integer variables
#' @importFrom robustbase mc
identifyOutliersTBStyleNI <- function(v, nMax) {
  v <- na.omit(v)
  qs <- quantile(v, c(0.25, 0.75))
  IQR <- qs[2] - qs[1]
  outlierPlaces <- v < qs[1]-1.5*IQR | v > qs[2]+1.5*IQR
  
  if (any(outlierPlaces)) {
    problem <- TRUE
    problemValues <- unique(v[outlierPlaces])
  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 nMax = nMax)
  list(problem=problem, message=outMessage)
}
