#' Example data to show the features of clean
#'
#' A dataset of constructed data used to as test bed when identifying potential errors in a data frame.
#'
#' @docType data
#' @keywords datasets
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
"testData"
