#Functions called from clean.R for performing cleaning steps/checks
#TO DO: structure: functions that are to check whether a problem is present
#             for each variable return a checkRes object, consisting of a
#             boolean and a message text (possibly empty) (=> define class checkRes)
#TO DO: Consider: Should the "v <- na.omit(v)" command at the beginning of
#       some (all?) checking functions be moved to the check-functions
#       themselves? I.e. do we ever need to consider missing values when checking?
#   And similarly: Should the labelled-vector unpacking also just be done once?
#TO DO: What should check() output? Maybe a list of checkRes-objects, and then
#       I should also implement a print() method for checkRes-lists to be used in clean()

#General comments:
# arguments: v is a variable (column) from a data.frame, i.e. a vector
# note: labelled is Wickham's class from the haven/hmisc packages
# note: Function names are suffixed with an indicator of the data types it
#       is to be called on. N is numerical, I is integer, F is factor,
#       L is labeled (Wickham), C is character. The ordering of suffix
#       letters is arbitrary. All suffixed functions are purely internal
#       and their calling is controlled using s3 methods


##########################################

####Check#####

#' Perform a check of potential errors in a data frame
#'
#' Runs a set of validation checks to check a vector for potential errors.  performs checking steps according to user input and/or data type of the inputted variable.
#'
#' @param v the vector to be checked
#' @param \dots other arguments that are passed on the to checking functions
#' @return An list object of class "checked" summarizing the result from the check of the check. As a minimum the returned class should contain the following elements
#' \itemize{
#'   \item{"name"}{The name of the check}
#'   \item{"description"}{Slightly more information about the check}
#'   \item{"problem"}{An integer giving an error code. 0 means no potential errors were identified}
#'   \item{"message"}{A string giving summary information in R markdown format about the results}
#' }
#' @author Anne Helby Petersen \email{ahpe@@sund.ku.dk} and Claus Thorn Ekstrom \email{ekstrom@@sund.ku.dk}
#' @seealso \code{\link{clean}}
#' @keywords misc
#' @examples
#'
#' x <- 1:5
#' check(x)
#'
#' # Annoyingly coded missing as 99
#' y <- c(rnorm(100), rep(99, 10))
#'
#' @export
check <- function(v, ...) UseMethod("check")

#characterChecks <- function() {
#    list(identifyMissing, identifyWhitespace, identifyLoners, identifyCaseIssues)
#}

#factorChecks <- function() {
#list(identifyMissing,
#     identifyWhitespace,
#     identifyLoners,
#     identifyCaseIssues)
#}

#Overwriting Claus' version so that it is consistent with what I did in
#visualize and summarize. Either change everything to Claus' method
#or keep my version everywhere.

#check.character <- function(v, characterChecks=NULL, ...) {
#    lapply(characterChecks, function( runthis ) { runthis(v, ...) })
#}

#check.factor <- function(v, factorChecks=NULL, ...) {
#    lapply(factorChecks, function( runthis ) { runthis(v, ...) })
#}



#' @export
check.character <- function(v, characterChecks=c("identifyMissing",
                                                 "identifyWhitespace",
                                                 "identifyLoners",
                                                 "identifyCaseIssues"), ...) {
  lapply(characterChecks, function(x) eval(call(x, v)))
}

#' @export
check.factor <- function(v, factorChecks=c("identifyMissing",
                                           "identifyWhitespace",
                                           "identifyLoners",
                                           "identifyCaseIssues"), ...) {
  lapply(factorChecks, function(x) eval(call(x, v)))
}

#' @export
check.labelled <- function(v, labelledChecks=c("identifyMissing",
                                               "identifyWhitespace"), ...) {
  lapply(labelledChecks, function(x) eval(call(x, v)))
}

#' @export
check.numeric <- function(v, numericChecks=c("identifyMissing",
                                             "identifyOutliers"), ...) {
  lapply(numericChecks, function(x) eval(call(x, v)))
}

#' @export
check.integer <- function(v, integerChecks=c("identifyMissing",
                                             "identifyOutliers"), ...) {
  lapply(integerChecks, function(x) eval(call(x, v)))
}


#NOTE: we don't actually do any logical checks...


#' @export
check.logical <- function(v, logicalChecks=c("identifyMissing"), ...) {
  lapply(logicalChecks, function(x) eval(call(x, v)))
}





##########################################


####General functions to be used in multiple checking functions below##########


#Produce a message using the internal output from a checking-function,
#i.e. a named list(problem=...(boolean), problemValues=...(string vector))
#NOTE: 3 slashes escapes the espaced string [\"] such that it is printed correctly
#(and not intepreted) in markdown.
messageGenerator <- function(problemStatus, check) {
  messages <- list(identifyMiss =
                     "The following suspected missing value codes enter as regular values:",
                   identifyWhitespace =
                     "The following values appear with prefixed or suffixed white space:",
                   identifyOutliers =
                     "Note that the following possible outlier values were detected:",
                   identifyLoners =
                     "Note that the following levels have at most five observations:",
                   identifyCaseIssues =
                     "Note that there might be case problems with the following levels:")
  ifelse(problemStatus$problem,
         paste(messages[[check]], paste(paste("\\\"", sort(problemStatus$problemValues), "\\\"", sep=""),
                                        collapse=", ")),
         "")
}

#"unpack" a vector of class labelled (Wickham) by concatenating the code values
#with the labels
unpackLabelled <- function(v) {
  c(as.character(v), attributes(v)$labels)
}


###########################################


####IdentifyMiss#####
identifyMissing <- function(v) UseMethod("identifyMissing")

#Identify candidate missing values that are interpreted as
#"regular" values in a variable.
#NOTE: I fill out missStrs manually to avoid having to run the same
#       bit of code each time identifyMiss is called. Better way to do it?


#Deal with 9, 99, 999, ... by converting to characterstring. Not
#very elegant, find better way.
#Takes a vector and returns the values that consist
#of more than one character/digit and consist only of 9's, e.g.
#99, 999, 999999 (but NOT 9).
#update: also works for other characters than 9, then it finds all
#occurances that consist of only that character, excluding a single
#occurance, e.g. just "9" or "" or "a", but identifying "99", "   ", "aaa"
identifyMissRepChar <- function(v, char) {
  v <- as.character(v)
  char <- as.character(char)
  v[v==char] <- "char"
  v[v==""] <- "empty"
  problems <- unlist(lapply(lapply(strsplit(v, ""), "==", char), all))
  if (any(problems)) {
    vals <- unique(v[problems])
  } else vals <- NULL
  vals
}


#Should only be used on numeric/integer vectors. 
#returns 9 if 9 seems to act as a missing value indicator
#in the variable
identifyMissNine <- function(v) {
  nines <- v==9
  if (!any(nines)) return(NULL)
  if (max(v)>9) return(NULL)
  if (length(unique(v))>8) return(NULL)
  9
}

#factor and character variables
identifyMissingCF <- function(v) {
  v <- na.omit(v)
  problem <- F
  problemValues <- NULL

  missStrs <- c(".", "", "nan", "NaN", "NAN", "na", "NA", "Na", "Inf", "inf",
                "-Inf", "-inf", "-", " ", "9")
  missStrsOcc <- intersect(v, missStrs) #what potential missing value strings occur?

  missNinesOcc <- identifyMissRepChar(v, "9") #what 99, 999, ... strings occur?
  missSpaceOcc <- identifyMissRepChar(v, " ") #what "  ", "   ", ... strings occur?

  allProblemOcc <- c(missStrsOcc, missNinesOcc, missSpaceOcc)

  if (length(allProblemOcc) > 0) {
    problemValues <- allProblemOcc
    problem <- T
  }
 # return(problemValues)###REMOVE ME
  outMessage <- messageGenerator(list(problem=problem, problemValues=problemValues),
                                 check="identifyMiss")
  list(problem=problem, message=outMessage)
}

#labbeled variables
identifyMissingL <- function(v) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyMissingCF(v)
}

#numerical and integer variables
identifyMissingNI <- function(v) {
  v <- na.omit(v)
  problem <- F
  problemValues <- NULL

  missNinesOcc <- identifyMissRepChar(v, "9") #what 99, 999, ... values occur?
  missNaNOcc <- unique(v[which(!is.finite(v))]) #what NaN, Inf, ... values occur?
  missNineOcc <- identifyMissNine(v) #returns 9 if 9 seems to act as missing value
  allProblemOcc <- c(missNinesOcc, missNaNOcc, missNineOcc)

  if (length(allProblemOcc) > 0) {
    problemValues <- allProblemOcc
    problem <- T
  }
  outMessage <- messageGenerator(list(problem=problem, problemValues=problemValues),
                                 check="identifyMiss")
  list(problem=problem, message=outMessage)
}

#logical (B = boolean) variables
#Note: for v to have class logical, only T, F and NA can occur, thus there are
#no potentially miscoded missing values.
#Note: this function catches variables that only consist of NA's, thus it is
#safe to omit NAs in the other functions, without risking obtaining an empty vector
identifyMissingB <- function(v) {
  list(problem=F, message="")
}

#Add methods to generic identifyMiss
identifyMissing.character <- function(v) identifyMissingCF(v)
identifyMissing.factor <- function(v) identifyMissingCF(v)
identifyMissing.labelled <- function(v) identifyMissingL(v)
identifyMissing.numeric <- function(v) identifyMissingNI(v)
identifyMissing.integer <- function(v) identifyMissingNI(v)
identifyMissing.logical <- function(v) identifyMissingB(v)



################################################################


####IdentifyWhitespace#####
identifyWhitespace <- function(v) UseMethod("identifyWhitespace")

#Identify values that begin or end with one or more whitespaces for
#character, factor and labelled variabled.


#character variables
identifyWhitespaceC <- function(v) {
  v <- na.omit(v)
  wsPrefixPlaces <- sapply(v, substr, 1, 1) == " "
  wsSuffixPlaces <- sapply(v, function(x) {tail(strsplit(x, "")[[1]],1)}) == " "
  allWsPlaces <- wsPrefixPlaces | wsSuffixPlaces
  if (any(allWsPlaces)) {
    problem <- T
    problemValues <- unique(v[allWsPlaces])
  } else {
    problem <- F
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 "identifyWhitespace")
  list(problem=problem, message=outMessage)
}

#factor variables
identifyWhitespaceF <- function(v) {
  identifyWhitespaceC(as.character(v))
}

#labelled variables
identifyWhitespaceL <- function(v) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyWhiteSpaceC(v)
}


#add methods to generic identifyWhitespace function
identifyWhitespace.character <- function(v) identifyWhitespaceC(v)
identifyWhitespace.factor <- function(v) identifyWhitespaceF(v)
identifyWhitespace.labelled <- function(v) identifyWhitespaceL(v)




#########################################################

####IdentifyOutliers#####
identifyOutliers <- function(v, ...) UseMethod("identifyOutliers")

#Identify values in numeric or integer variables that are considered to be
#outliers, i.e. smaller than the 1st quartile minus 1.5*IQR or
#larger than the 3rd quartile plus 1.5*IQR (Turkey Boxplot style)
#NOTE: Other outlier definitions? Multiple outlier definitions?
#NOTE: Add identifyOutliers function for (some) labelled variables?

#numerical and integer variables
identifyOutliersNI <- function(v) {
  v <- na.omit(v)
  qs <- quantile(v, c(0.25, 0.75))
  IQR <- qs[2] - qs[1]
  outlierPlaces <- v < qs[1]-1.5*IQR | v > qs[2]+1.5*IQR
  if (any(outlierPlaces)) {
    problem <- T
    problemValues <- v[outlierPlaces] #if outlier value occurs multiple times,
                                      #it will be printed multiple times
  } else {
    problem <- F
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



###############################################################

####IdentifyLoners#####
identifyLoners <- function(v, ...) UseMethod("identifyLoners")

#For character/factor variables, identify values that only have a
#very low number of observations, as these categories might be
#problematic when conducting an analysis. Unused factor levels are
#not considered "loners". "Loners" have 5 or less observations.
#(corresponding to the chi2-test rule of thumb)
#NOTE: Different (or just more) loner definition(s)?
#NOTE: Proper term for "loner"?
#NOTE: identifyLoner for integer/numerical variables suspected to be categorical?
#NOTE: identifyLoner for labelled variables?


#factor variables
identifyLonersF <- function(v) {
  vLev <- levels(v)
  v <- factor(na.omit(v)) #drop unused levels
  lonerOcc <- vLev[which(table(v) <=5)]
  if (length(lonerOcc) > 0) {
    problem <- T
    problemValues <- lonerOcc
  } else {
    problem <- F
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 "identifyLoners")
  list(problem=problem, message=outMessage)
}

#character variables
identifyLonersC <- function(v) {
  v <- factor(v)
  identifyLonersF(v)
}


#add methods to generic identifyLoners function
identifyLoners.factor <- function(v) identifyLonersF(v)
identifyLoners.character <- function(v) identifyLonersC(v)



#########################################################

####IdentifyCaseIssues#####
identifyCaseIssues <- function(v, ...) UseMethod("identifyCaseIssues")

#Identify whether the same levels appear multiple times in a factor
#or character variable, using different case settings.
#NOTE: Add labelled method?


#character variable
identifyCaseIssuesC <- function(v) {
  v <- na.omit(v)
  vLevs <- unique(v)
  vLevsLower <- tolower(vLevs)
  problemOcc <- vLevs[which(duplicated(vLevsLower) | duplicated(vLevsLower, fromLast=T))]
  if (length(problemOcc) > 0) {
    problem <- T
    problemValues <- sort(problemOcc)
  } else {
    problem <- F
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 "identifyCaseIssues")
  list(problem=problem, message=outMessage)
}


#factor variable
identifyCaseIssuesF <- function(v) {
  v <- as.character(v)
  identifyCaseIssuesC(v)
}


#add methods to generic identifyCaseIssues function
identifyCaseIssues.character <- function(v) identifyCaseIssuesC(v)
identifyCaseIssues.factor <- function(v) identifyCaseIssuesF(v)

