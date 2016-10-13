#' @title A checkFunction for identifying outliers
#' 
#' @description A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable.
#' 
#' @param v A character or factor variable to check
#' 
#' @details DOES LATEX WORK HERE? IT SHOULD, SHOULDN'T IT?
#' Outliers are defined in the style of Turkey Boxplots (consistent with the 
#' \code{boxplot} function), i.e. as values $v$ such that $v < Q_1 - 1.5 \cdot \text{IQR}$ or 
#' $v > Q_3 + 1.5 \cdot \text{IQR}$ where $Q_1$, $Q_3$ and $\text{IQR} is the 1st quartile,
#' 3rd quartile and inter quartile range, respectively.  
#' 
#' @return A list with two elements, $problem: TRUE if any outliers were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were outliers. Note that outlier values
#' are printed the number of times they appear, but sorted alphabetically.
#' 
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#' 
#' @example 
#'  identifyOutliers(c(1:10, 200, 200, 700))
#' 
#' @importFrom stats na.omit quantile
#' @export
identifyOutliers <- function(v) UseMethod("identifyOutliers")
identifyOutliers <- checkFunction(identifyOutliers, "Identify outliers")




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

#add methods to generic identifyOutliers function
identifyOutliers.numeric <- function(v) identifyOutliersNI(v)
identifyOutliers.integer <- function(v) identifyOutliersNI(v)

