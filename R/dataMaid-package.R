#' Exteneded example data to test the features of dataMaid
#'
#' A dataset of constructed data used as test bed when using \code{dataMaid} for identifying
#' potential errors in a dataset.
#'
#' @format A data frame with 15 rows and 14 variables.
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
#' @examples
#' data(testData)
#'
"testData"


#' Small example data to show the features of dataMaid
#'
#' An artificial dataset, intended for presenting the key features of \code{dataMaid}, which is a
#' toolset for identifying potential errors in a dataset.
#'
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
#' @examples
#' data(toyData)
#'
"toyData"


#' Example data with zero-inflated variables
#'
#' An artificial dataset, intended for presenting the extended features of \code{dataMaid},
#' which is a toolset for identifying potential errors in a dataset.
#'
#' @format A \code{data.frame} with 300 observations on the following 6 variables.
#'   \describe{
#'     \item{\code{addresses}}{a factor with fictitious US addresses}
#'     \item{\code{binomial}}{a numeric vector with a binomial distributed variable}
#'     \item{\code{poisson}}{a numeric vector with a Poisson distributed variable}
#'     \item{\code{gauss}}{a numeric vector with a Gaussian distributed variable}
#'     \item{\code{zigauss}}{a numeric vector with a zero-inflated Gaussian distributed variable}
#'     \item{\code{bpinteraction}}{a factor with interactions between binomial and poisson values}
#'  }
#'
#' @source Artificial data
#' @examples
#'
#' \dontrun{
#' isID <- function(v, nMax = NULL, ...) {
#'   out <- list(problem = FALSE, message = "")
#'   if (class(v) %in% c("character", "factor", "labelled", "numeric", "integer")) {
#'     v <- as.character(v)
#'     lengths <- nchar(v)
#'     if (all(lengths > 10) & length(unique(lengths)) == 1) {
#'       out$problem <- TRUE
#'       out$message <- "Warning: This variable seems to contain ID codes!"
#'     }
#'   }
#'   out
#' }
#'
#'
#' countZeros <- function(v, ...) {
#'   res <- length(which(v == 0))
#'   summaryResult(list(feature = "No. zeros", result = res, value = res))
#' }
#' countZeros <- summaryFunction(countZeros, description = "Count number of zeros",
#'                               classes = allClasses())
#' summarize(toyData, numericSummaries = c(defaultNumericSummaries()))
#'
#'
#' mosaicVisual <- function(v, vnam, doEval) {
#'   thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
#'   if (doEval) {
#'     return(eval(thisCall))
#'   } else return(deparse(thisCall))
#' }
#' mosaicVisual <- visualFunction(mosaicVisual,
#'                                description = "Mosaic plots using graphics",
#'                                classes = allClasses())
#'
#' identifyColons <- function(v, nMax = Inf, ... ) {
#'   v <- unique(na.omit(v))
#'   problemMessage <- "Note: The following values include colons:"
#'   problem <- FALSE
#'   problemValues <- NULL
#'
#'   problemValues <- v[sapply(gregexpr("[[:xdigit:]]:[[:xdigit:]]", v),
#'                             function(x) all(x != -1))]
#'
#'   if (length(problemValues) > 0) {
#'     problem <- TRUE
#'   }
#'
#'   problemStatus <- list(problem = problem,
#'                         problemValues = problemValues)
#'   outMessage <- messageGenerator(problemStatus, problemMessage, nMax)
#'
#'   checkResult(list(problem = problem,
#'                    message = outMessage,
#'                    problemValues = problemValues))
#' }
#'
#' identifyColons <- checkFunction(identifyColons,
#'                                description = "Identify non-suffixed nor -prefixed colons",
#'                                classes = c("character", "factor", "labelled"))
#' clean(exampleData, replace = TRUE,
#'       preChecks = c("isKey", "isEmpty", "isID"),
#'       allVisuals = "mosaicVisual",
#'       characterSummaries = c(defaultCharacterSummaries(), "countZeros"),
#'       factorSummaries = c(defaultFactorSummaries(), "countZeros"),
#'       labelledSummaries = c(defaultLabelledSummaries(), "countZeros"),
#'       numericSummaries = c(defaultNumericSummaries(), "countZeros"),
#'       integerSummaries = c(defaultIntegerSummaries(), "countZeros"),
#'       characterChecks = c(defaultCharacterChecks(), "identifyColons"),
#'       factorChecks = c(defaultFactorChecks(), "identifyColons"),
#'       labelledCheck = c(defaultLabelledChecks(), "identifyColons"))
#'
#'
#'
#' }
#'
"exampleData"
