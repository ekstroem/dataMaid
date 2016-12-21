#' Exteneded example data to test the features of cleanR
#'
#' A dataset of constructed data used as test bed when using \code{cleanR} for identifying 
#' potential errors in a dataset.
#'
#' @name testData
#' @docType data
#' @format A data frame with 15 rows and 15 variables.
#' \describe{
#'    \item{charVar}{A character vector with a single missing observation.}
#'    \item{factorVar}{A factor vector with a miscoded missing observation, \code{999}.}
#'    \item{numVar}{A numeric vector}
#'    \item{intVar}{An integer vector}
#'    \item{boolVar}{A logical vector with three missing observations.}
#'    \item{keyVar}{A character vector with unique codes for each observation.}
#'    \item{emptyVar}{A numeric vector where all entries are identical.}
#'    \item{numOutlierVar}{A numeric vector with a possible outlier (\code{100}).}
#'    \item{smartNumVar}{A numeric vector that takes only two different values.}
#'    \item{cprVar}{A character vector with levels in the format of Danish CPR numbers 
#'    (social security numbers).}
#'    \item{cprKeyVar}{A character vector with levels in the format of Danish CPR numbers
#'    (social security numbers) with unique levels for each observation.}
#'    \item{miscodedMissingVar}{A character vector with levels corresponding to 
#'    various miscoded (non-\code{NA}) misssing codes.}
#'    \item{misclassifiedNumVar}{A misclassified factor variable, where every level
#'    is a number and a many (12) different levels are in use.}
#'    \item{dateVar}{A Date vector.}
#'    \item{labelledVar}{A labelled vector with two missing observations.}
#' }
#' @source Artificial data
#' @keywords datasets
#' @examples
#' data(testData)
#'
NULL


#' Small example data to show the features of cleanR
#'
#' An artificial dataset, intended for presenting the key features of \code{cleanR}, which is a
#' toolset for identifying potential errors in a dataset.
#'
#' @name toyData
#' @docType data
#' @format A \code{data.frame} with 15 rows and 6 variables.
#' \describe{
#'    \item{var1}{A factor variable with two levels (\code{"red"} and \code{"blue"}) and a few 
#'    (correctly coded)  missing observations.}
#'    \item{var2}{A numeric variable with one obvious outlier value (\code{82}), two miscoded 
#'    missing values (\code{999} and \code{NaN}) and a few correctly coded missing values.}
#'    \item{var3}{A factor variable where two of the levels (\code{"other"} and \code{"OTHER"} 
#'    are the same word with different case settings. Moreover, the variable includes a Stata-style
#'    miscoded missing value (\code{"."})}. 
#'    \item{var4}{A numeric variable (random draws from a standard normal distribution).}
#'    \item{var5}{A factor variable with unique codes for each observation (a character string
#'    with a number between 1 and 15), i.e. a key variable.}
#'    \item{var6}{A factor variable that has the same level (\code{"Irrelevant"}) for all 
#'    observations, i.e. a empty variable.}
#' }
#' @source Artificial data
#' @keywords datasets
#' @examples
#' data(toyData)
#'
NULL
