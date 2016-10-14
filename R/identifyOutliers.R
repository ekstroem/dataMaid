#' @title A checkFunction for identifying outliers
#'
#' @description A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable.
#'
#' @param v A character or factor variable to check
#'
#' @details Outliers are defined in the style of Turkey Boxplots (consistent with the
#' \code{\link{boxplot}} function), i.e. as values  that are smaller than the 1st quartile minus
#' the inter quartile range (IQR) or greater than  the third quartile plus the IQR.
#'
#' @return A list with two elements, $problem: TRUE if any outliers were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were outliers. Note that outlier values
#' are printed the number of times they appear, but sorted alphabetically.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#'
#' @examples
#'  identifyOutliers(c(1:10, 200, 200, 700))
#'
#' @importFrom stats na.omit quantile
#' @export
identifyOutliers <- function(v) UseMethod("identifyOutliers")
identifyOutliers <- checkFunction(identifyOutliers, "Identify outliers")


#add methods to generic identifyOutliers function
#' @export
identifyOutliers.numeric <- function(v) identifyOutliersNI(v)

#' @export
identifyOutliers.integer <- function(v) identifyOutliersNI(v)





##########################################Not exported below#########################################


#numerical and integer variables
identifyOutliersNI <- function(v) {
  v <- na.omit(v)
  qs <- quantile(v, c(0.25, 0.75))
  IQR <- qs[2] - qs[1]
  outlierPlaces <- v < qs[1]-1.5*IQR | v > qs[2]+1.5*IQR
  if (any(outlierPlaces)) {
    problem <- TRUE
    problemValues <- v[outlierPlaces] #if outlier value occurs multiple times,
    #it will be printed multiple times
  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 "identifyOutliers")
  list(problem=problem, message=outMessage)
}
