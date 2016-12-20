#' Example data to show the features of clean
#'
#' A dataset of constructed data used to as test bed when identifying potential errors in a data frame.
#'
#' @name testData
#' @docType data
#' @format A data frame with 15 rows and 14 variables.
#' \describe{
#'    \item{charVar}{a simple factor}
#'    \item{factorVar}{a factor with a "special" level 999}
#'    \item{numVar}{a numeric vector}
#'    \item{intVar}{an integer vector}
#'    \item{boolVar}{a logical vector}
#'    \item{keyVar}{a factor}
#'    \item{emptyVar}{a numeric vector where all the elements are identical}
#'    \item{_joeVar}{a numeric vector}
#'    \item{jack__var}{a numeric vector}
#'    \item{numOutlierVar}{a numeric vector with a possible outlier}
#'    \item{smartNumVar}{a numeric vector}
#'    \item{cprVar}{a factor with levels in the format of the Danish CPR number (Social security number)}
#'    \item{cprKeyVar}{a factor with levels in the format of the Danish CPR number}
#'    \item{miscodedMissingVar}{a factor with levels corresponding to various misssing codes}
#' }
#' @source Artificial data
#' @keywords datasets
#' @examples
#' data(testData)
#'
NULL


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds. The variables are as follows:
#'
#' \itemize{
#'   \item price. price in US dollars (\$326--\$18,823)
#'   \item carat. weight of the diamond (0.2--5.01)
#'   ...
#' }
#'
#' @format A data frame with 53940 rows and 10 variables
#' @source \url{http://www.diamondse.info/}
"toyData"
