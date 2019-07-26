#' Semi-artificial data about the US presidents
#'
#' A dataset with information about the first 45 US presidents as well as a 46th
#' person, who is not a US president. The dataset was constructed to show the capabilities
#' of \code{dataMaid} and therefore, it has been constructed to include errors and miscodings.
#' Each observation in the dataset corresponds to a person. The dataset uses the 
#' non-standard class \code{Name} which is simply an attribute that has been added to 
#' two variables in order to show how \code{dataMaid} handles non-supported classes. 
#'
#' @format A data frame with 46 rows and 11 variables.
#' \describe{
#'    \item{lastName}{A \code{Name} type variable containing the last name of the president.}
#'    \item{firstName}{A \code{Name} type variable containing the first name of the president.}
#'    \item{orderOfPresidency}{A factor variable indicating the order of the presidents (with George Washington
#'    as number 1 and Donald Trump as number 45).}
#'    \item{birthday}{A Date variable with the birthday of the president}
#'    \item{stateOfBirth}{A character variable with the state in which the president was born.}
#'    \item{assassinationAttempt}{A numeric variable indicating whether there was an assassination 
#'    attempt (\code{1}) or not (\code{0}) on the president.}
#'    \item{sex}{A factor variable with the sex of the president.}
#'    \item{ethnicity}{A factor variable with the ethnicity of the president.}
#'    \item{presidencyYears}{A numeric variable with the duration of the presidency, in years.}
#'    \item{ageAtInauguration}{A character variable with the age at inauguration.}
#'    \item{favoriteNumber}{A \code{complex} type variable with a fictional favorite number for
#'    each president.}
#' }
#' @references Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \url{https://doi.org/10.18637/jss.v090.i06}).
#' @source Artificial dataset constructed based on the US president dataset available from 
#' \href{http://www.data-explorer.com/data}{Data Explorer}.
#' @examples
#' data(presidentData)
#'
"presidentData"


#' Semi-artificial data about the US presidents (extended version)
#'
#' A dataset with information about the first 45 US presidents as well as a 46th
#' person, who is not a US president, and a duplicate of one of the 45 actual presidents. 
#' The dataset was constructed to show the capabilities
#' of \code{dataMaid} and therefore, it has been constructed to include errors and miscodings.
#' Each observation in the dataset corresponds to a person. The dataset uses the 
#' non-standard class \code{Name} which is simply an attribute that has been added to 
#' two variables in order to show how \code{dataMaid} handles non-supported classes. Note that the dataset
#' is an extended and more error-filled version of the dataset \code{presidentData} which is 
#' also included in the package. 
#'
#' @format A data frame with 47 rows and 15 variables.
#' \describe{
#'    \item{lastName}{A \code{Name} type variable containing the last name of the president.}
#'    \item{firstName}{A \code{Name} type variable containing the first name of the president.}
#'    \item{orderOfPresidency}{A factor variable indicating the order of the presidents (with George Washington
#'    as number 1 and Donald Trump as number 45).}
#'    \item{birthday}{A Date variable with the birthday of the president.}
#'    \item{dateOfDeath}{A Date variable with the date of the president's death.}
#'    \item{stateOfBirth}{A character variable with the state in which the president was born.}
#'    \item{party}{A charcter variable with the party to which the president was associated.}
#'    \item{presidencyBeginDate}{A Date variable with the date of inauguration of the president.}
#'    \item{presidencyEndDate}{A Date variable with the date at which the presidency ends.}
#'    \item{assassinationAttempt}{A numeric variable indicating whether there was an assassination 
#'    attempt (\code{1}) or not (\code{0}) on the president.}
#'    \item{sex}{A factor variable with the sex of the president.}
#'    \item{ethnicity}{A factor variable with the ethnicity of the president.}
#'    \item{presidencyYears}{A numeric variable with the duration of the presidency, in years.}
#'    \item{ageAtInauguration}{A character variable with the age at inauguration.}
#'    \item{favoriteNumber}{A \code{complex} type variable with a fictional favorite number for
#'    each president.}
#' }
#' @source Artificial dataset constructed based on the US president dataset available from 
#' \href{http://www.data-explorer.com/data}{Data Explorer}.
#' @references Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \url{https://doi.org/10.18637/jss.v090.i06}).
#' @examples
#' data(bigPresidentData)
#'
"bigPresidentData"


#' Semi-artificial data about masterpieces of art
#'
#' A dataset with information about 200 painting and their painters.
#' Each observation in the dataset corresponds to a painting. A single artificial variable, 
#' namely an artist ID variable, has been included. Otherwise the information should 
#' be truthful. 
#'
#' @format A data frame with 200 rows and 11 variables.
#' \describe{
#'    \item{ArtistID}{A unique ID used for cataloging the artists (fictional).}
#'    \item{ArtistName}{The name of the artist.}
#'    \item{NoOfMiddlenames}{The number of middlenames the artist has.}
#'    \item{Title}{The title of the painting.}
#'    \item{Year}{The approximate year in which the painting was made.}
#'    \item{Location}{The current location of the painting.}
#'    \item{Continent}{The continent of the current location of the painting.}
#'    \item{Width}{The width of the painting, in centimeters.}
#'    \item{Height}{The height of the painting, in centimers.}
#'    \item{Media}{The media/materials of the painting.}
#'    \item{Movement}{The artistic movement(s) the painting belongs to.}
#'}
#' @source Semi-artificial dataset constructed based on the Master Works of Art dataset available from 
#' \href{http://www.data-explorer.com/data}{Data Explorer}.
#' @examples
#' data(artData)
#'
#' @importFrom htmltools includeHTML
"artData"


#' Extended example data to test the features of dataMaid
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
#'    \item{pill}{A factor variable with two levels (\code{"red"} and \code{"blue"}) and a few
#'    (correctly coded) missing observations. This represents the colour of a pill.}
#'    \item{events}{A numeric variable with one obvious outlier value (\code{82}), two miscoded
#'    missing values (\code{999} and \code{NaN}) and a few correctly coded missing values. The number of previous events.}
#'    \item{region}{A factor variable where two of the levels (\code{"other"} and \code{"OTHER"}
#'    are the same word with different case settings. Moreover, the variable includes a Stata-style
#'    miscoded missing value (\code{"."}). Used to represent geographical regions or treatment centers.}.
#'    \item{change}{A numeric variable (random draws from a standard normal distribution). Representing a change in a measured variable.}
#'    \item{id}{A factor variable with unique codes for each observation (a character string
#'    with a number between 1 and 15), i.e. a key variable.}
#'    \item{spotifysong}{A factor variable that has the same level (\code{"Irrelevant"}) for all
#'    observations, i.e. a empty variable. The latest song played on Spotify.}
#' }
#' @source Artificial data
#' @references Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \url{https://doi.org/10.18637/jss.v090.i06}).
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
#' makeDataReport(exampleData, replace = TRUE,
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
