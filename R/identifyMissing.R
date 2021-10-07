#' A checkFunction for identifying miscoded missing values.
#'
#' A checkFunction to be called from \code{\link{check}} that identifies values that
#' appear to be miscoded missing values.
#'
#' @param v A variable to check.
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#' @param ... Not in use.
#'
#' @details \code{identifyMissing} tries to identify common choices of missing values outside of the
#' R standard (\code{NA}). These include special words (NaN and Inf (no matter the cases)),
#' one or more -9/9's (e.g. 999, "99", -9, "-99"), one ore more -8/8's (e.g. -8, 888, -8888),
#' Stata style missing values (commencing with ".") and other character strings
#' ("", " ", "-", "NA" miscoded as character). If the variable is numeric/integer or a
#' character/factor variable consisting only of numbers and with more than 11 different values,
#' the numeric miscoded missing values (999, 888, -99, -8 etc.) are
#' only recognized as miscoded missing if they are maximum or minimum, respectively, and the distance
#' between the second largest/smallest value and this maximum/minimum value is greater than one.
#'
#' @return A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating whether midcoded missing values where found),
#' \code{$message} (a message describing which values in \code{v} were suspected to be
#' miscoded missing values), and \code{$problemValues} (the problematic values
#' in their original format). Note that Only unique problematic values
#' are listed and that they are presented in alphabetical order.
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#'
#' @examples
#' ##data(testData)
#' ##testData$miscodedMissingVar
#' ##identifyMissing(testData$miscodedMissingVar)
#'
#' #Identify miscoded numeric missing values
#' v1 <- c(1:15, 99)
#' v2 <- c(v1, 98)
#' v3 <- c(-999, v2, 9999)
#' identifyMissing(v1)
#' identifyMissing(v2)
#' identifyMissing(v3)
#' identifyMissing(factor(v3))
#'
#' @importFrom stats na.omit
#' @export
identifyMissing <- function(v, nMax = 10, ...) UseMethod("identifyMissing")


#Add methods to generic identifyMiss
#' @export
identifyMissing.character <- function(v, nMax = 10, ...) identifyMissingCF(v, nMax = nMax)
#' @export
identifyMissing.factor <- function(v, nMax = 10, ...) identifyMissingCF(v, nMax = nMax)
#' @export
identifyMissing.labelled <- function(v, nMax = 10, ...) identifyMissingL(v, nMax = nMax)
#' @export
identifyMissing.haven_labelled <- function(v, nMax = 10, ...) identifyMissingL(v, nMax = nMax)
#' @export
identifyMissing.numeric <- function(v, nMax = 10, ...) {
  identifyMissingNI(v, nMax = nMax, ...)
}
#' @export
identifyMissing.integer <- function(v, nMax = 10, ...) {
  identifyMissingNI(v, nMax = nMax, ...)
}
#' @export
identifyMissing.logical <- function(v, nMax = 10, ...) identifyMissingB(v, nMax = nMax)
#' @export
identifyMissing.Date <- function(v, nMax = 10, ...) identifyMissingD(v, nMax = nMax)


#make it a checkFunction
#' @include checkFunction.R
identifyMissing <- checkFunction(identifyMissing, "Identify miscoded missing values", allClasses())

##                                 setdiff(allClasses(), "Date"))

##########################################Not exported below#########################################

identifyMissingMessage <- "The following suspected missing value codes enter as regular values:"

#NOTE: I fill out missStrs manually to avoid having to run the same
#bit of code each time identifyMiss is called. Better way to do it?

#Deal with 9, 99, 999, ... by converting to characterstring. Not
#very elegant, find better way.
#Takes a vector and returns the values that consist
#of more than one character/digit and consist only of "char", e.g.
#99, 999, 999999, 9 when char = 9. Note that ignoreFirst
#removes 9 (a single occurrence of "char") from the outputted vector.
#If prefix != NULL, the function looks for repeated entries after
#removing the prefix. If e.g. prefix = "-" and char = 8,
#values like -88, -8, -8888 will be identified (if present).
identifyMissRepChar <- function(v, char, prefix=NULL, ignoreFirst = FALSE) {
  originalClass <- class(v)
  v <- as.character(v)
  char <- as.character(char)
  if (!is.null(prefix)) {
    v <- v[substr(v, 1, 1) == prefix]
    v <- sub(paste("^", prefix, "{1}", sep=""), "", v)
  }
    #should a single occurrence count?
  if (ignoreFirst) v[v==char] <- "char"

  v[v==""] <- "empty"
  problems <- unlist(lapply(lapply(strsplit(v, ""), "==", char), all))
  if (any(problems)) {
    if (!is.null(prefix)) {
      vals <- paste(prefix, v[problems], sep="")
    } else vals <- v[problems]
    vals <- unique(vals)
    class(vals) <- originalClass
  } else vals <- NULL
  vals
}


#For numeric/integer variables only!
#Identify occurences of num in v. If e.g. num is 8,
#both 8, 88, 888, ...,  and -8, -88, ...,
#are identified. However, an occurrence only "counts" if it
#is the min/max value of the variable and it is seperated from
#the second biggest (smallest) value by at least 1.
identifyMissNumber <- function(v, num, allOcc = TRUE, alreadyUniqueSorted=FALSE) {
  if (!alreadyUniqueSorted)
      v <- sort(unique(v))
  posProblemVals <- identifyMissRepChar(v, num)
  negProblemVals <- identifyMissRepChar(v, num, prefix="-")
  if (allOcc) {
    return(c(posProblemVals, negProblemVals))
  } else {
    nV <- length(v)
    if (nV >=2) {
      maxV <- v[nV]
      maxV2 <- v[(nV-1)]
      minV <- v[1]
      minV2 <- v[2]
    } else {
      maxV <- maxV2 <- minV <- minV2 <- v[1]
    }
    if (!is.null(posProblemVals)) {
      if (maxV > maxV2 + 1) {
        posProblemVals <- intersect(posProblemVals, maxV)
      } else posProblemVals <- NULL
    }
    if (!is.null(negProblemVals)) {
      if (minV < minV2 - 1) {
        negProblemVals <- intersect(negProblemVals, minV)
      } else negProblemVals <- NULL
    }
    return(union(posProblemVals, negProblemVals))
  }
}



#factor and character variables
identifyMissingCF <- function(v, nMax) {
    v <- na.omit(as.character(v)) #as.character in order to be able to combine
                                  #factor levels below without unwanted conversions
    problem <- FALSE
    problemValues <- NULL

    #what potential missing value strings occur?
    missStrs <- c("", "nan", "NaN", "NAN", "na", "NA", "Na", "Inf", "inf",
                  "-Inf", "-inf", "-")
    missStrsOcc <- intersect(v, missStrs)

    #what "  ", "   ", ... strings occur?
    missSpaceOcc <- identifyMissRepChar(v, " ")

    ## STATA-style: .something describes a "something" type of missing value
    missDotPrefixOcc <- unique(v[substr(v, 1, 1) == "."])

    #Only look at the part of v that is not suspected to be miscoded missing
    v <- v[!(v %in% c(missStrsOcc, missSpaceOcc, missDotPrefixOcc))]

    #Numeric miscoded missing values, method depending on whether v seems to be
    #numeric (though categorical)
    if (identifyNums(v)$problem) {
      v <- as.numeric(v)
      missAllNinesOcc <- identifyMissNumber(v, 9, FALSE)
      missAllEightsOcc <- identifyMissNumber(v, 8, FALSE)
    } else {
      #what 9, -9, 99, -99, 999, -999, ... strings occur?
      missAllNinesOcc <- identifyMissNumber(v, 9, TRUE)

      #what 8, 88, 888, ... strings occur?
      missAllEightsOcc <- identifyMissNumber(v, 8, TRUE)
    }

    allProblemOcc <- c(missStrsOcc, missAllNinesOcc, missAllEightsOcc,
                       missSpaceOcc, missDotPrefixOcc)

    if (length(allProblemOcc) > 0) {
        problemValues <- allProblemOcc
        problem <- TRUE
    }

    outMessage <- messageGenerator(list(problem = problem,
                                        problemValues = problemValues),
                                   message = identifyMissingMessage,
                                   nMax = nMax)
    checkResult(list(problem = problem, message = outMessage,
                     problemValues = problemValues))
}

#labelled variables
identifyMissingL <- function(v, nMax) {
  v <- na.omit(dataMaid_as_factor(v))
  identifyMissingCF(v, nMax = nMax)
}

#numerical and integer variables
identifyMissingNI <- function(v, nMax, maxDecimals) {
  v <- v[!is.na(v) | is.nan(v)]
  problem <- FALSE

  #two types of problemvalues, so that one can
  #be rounded
  problemValues <- outProblemValues <- NULL

  finiteInd <- is.finite(v)

  #what 99, -9, -999, 9, ... values occur?
  missNinesOcc <- identifyMissNumber(v[finiteInd], 9, FALSE)

  #what -8, -88, 8, 888, ... values occur?
  missEightsOcc <- identifyMissNumber(v[finiteInd], 8, FALSE)

  #what NaN, Inf, ... values occur?
  missNaNOcc <- unique(v[!finiteInd])

  allProblemOcc <- c(missNinesOcc, missEightsOcc, missNaNOcc)

  if (length(allProblemOcc) > 0) {
    outProblemValues <- allProblemOcc
    problemValues <- round(allProblemOcc, maxDecimals)
    problem <- TRUE
  }
  outMessage <- messageGenerator(list(problem = problem,
                                      problemValues = problemValues),
                                 message = identifyMissingMessage,
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage,
                   problemValues = outProblemValues))
}

#logical (B = boolean) variables
#Note: for v to have class logical, only T, F and NA can occur, thus there are
#no potentially miscoded missing values.
#Note: this function catches variables that only consist of NA's, thus it is
#safe to omit NAs in the other functions, without risking obtaining an empty vector
identifyMissingB <- function(v, nMax) {
  checkResult(list(problem = FALSE, message = "", problemValues = NULL))
}


#Date variables
identifyMissingD <- function(v, nMax) {
  v <- v[!is.na(v)]
  problem <- FALSE

  problemValues <- outProblemValues <- NULL

  ## Extract the year
  problemOcc <- v[as.numeric(format(v, "%Y"))==9999]

  if (length(problemOcc)) {
      problem <- TRUE
      problemValues <- problemOcc
      outProblemValues <- problemOcc
  }

  outMessage <- messageGenerator(list(problem = problem,
                                      problemValues = problemValues),
                                 message = "The year for the following dates look suspicious",
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage,
                   problemValues = outProblemValues))
}

