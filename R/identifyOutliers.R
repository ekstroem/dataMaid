#' A checkFunction for identifying outliers
#'
#' A checkFunction to be called from \code{\link{check}} that identifies outlier values
#' in a numeric/integer variable.
#'
#' @param v A character or factor variable to check
#' @param nMax The maximum number of problematic values to report. Default is \code{Inf}, in which case
#' all problematic values are included in the outputtet message.
#' @inheritsParams clean 
#'
#' @details Outliers are defined in the style of Turkey Boxplots (consistent with the
#' \code{\link{boxplot}} function), i.e. as values  that are smaller than the 1st quartile minus
#' the inter quartile range (IQR) or greater than  the third quartile plus the IQR. NOT TRUE(?)
#'
#' @return A list with two elements, $problem: TRUE if any outliers were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were outliers. Note that outlier values
#' are printed the number of times they appear, but sorted alphabetically. NOT TRUE(?)
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
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
identifyOutliers <- checkFunction(identifyOutliers, "Identify outliers")


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
    problemValues <- v[outlierPlaces]
    
    #only print each outlier value once (and round them):
    problemValues <- round(unique(problemValues), maxDecimals)
    
    ## if outlier value occurs multiple times,
    ## it will be printed multiple times

    ## CE: Alternatively print it fewer times but with a multiplier
    ## Gives two problems: 1) values become text so sorted lexicographically (not nice), and 2) values and multiplers are part of the same string which is also not that nice
    ## xx <- table(problemValues)
    ## problemValues <- paste0(attributes(xx)$dimnames[[1]], " (x ", xx, ")")

  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 nMax = nMax)
  list(problem=problem, message=outMessage)
}
