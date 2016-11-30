#' A checkFunction for identifying miscoded missing values.
#'
#' A checkFunction to be called from \code{\link{check}} that identifies values that
#' appear to be miscoded missing values.
#'
#' @param v A variable to check.
#' @param nMax The maximum number of problematic values to report. Default is \code{Inf}, in which case
#' all problematic values are included in the outputtet message.
#' @param ... Not in use.
#'
#' @details \code{identifyMissing} tries to identify common choices of missing values outside of the
#' R standards. These include special words (NaN and Inf (no matter the cases)), one or more -9/9's
#' (e.g. 999, "99", -9, "-99"), one ore more -8/8's (e.g. -8, 888, -8888), STATA style missing values
#' (commencing with ".") and other character strings ("", " ", "-", "NA" miscoded as character). If 
#' the variable is numeric/integer or a character/factor variable consisting only of numbers and with
#' more than 11 different values, the numeric miscoded missing values (999, 888, -99, -8 etc.) are 
#' only recognized as miscoded missing if they are max or min, respectively, and the distance 
#' between the second largest/smallest value and this max/min value is greater than one. 
#'
#' @return A list with two elements, $problem: TRUE if any miscoded missing values were found,
#' FALSE otherwise, and $message A message describing which values in \code{v} were miscoded missing
#' values. Note that only unique values are listed and that they appear in alphabetical order.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
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
identifyMissing <- function(v, nMax = Inf, ...) UseMethod("identifyMissing")


#Add methods to generic identifyMiss
#' @export
identifyMissing.character <- function(v, nMax = Inf, ...) identifyMissingCF(v, nMax = nMax)
#' @export
identifyMissing.factor <- function(v, nMax = Inf, ...) identifyMissingCF(v, nMax = nMax)
#' @export
identifyMissing.labelled <- function(v, nMax = Inf, ...) identifyMissingL(v, nMax = nMax)
#' @export
identifyMissing.numeric <- function(v, nMax = Inf, ...) {
  identifyMissingNI(v, nMax = nMax, ...)
}
#' @export
identifyMissing.integer <- function(v, nMax = Inf, ...) {
  identifyMissingNI(v, nMax = nMax, ...)
}
#' @export
identifyMissing.logical <- function(v, nMax = Inf, ...) identifyMissingB(v, nMax = nMax)


#make it a checkFunction
identifyMissing <- checkFunction(identifyMissing, "Identify miscoded missing values")

##########################################Not exported below#########################################



#NOTE: I fill out missStrs manually to avoid having to run the same
#bit of code each time identifyMiss is called. Better way to do it?

#Deal with 9, 99, 999, ... by converting to characterstring. Not
#very elegant, find better way.
#Takes a vector and returns the values that consist
#of more than one character/digit and consist only of 9's, e.g.
#99, 999, 999999 (but NOT 9).
#update: also works for other characters than 9, then it finds all
#occurances that consist of only that character, excluding a single
#occurance, e.g. just "9" or "" or "a", but identifying "99", "   ", "aaa"
identifyMissRepChar <- function(v, char, prefix=NULL, ignoreFirst = FALSE) {
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
  } else vals <- NULL
  vals
}


#Should only be used on numeric/integer vectors.
#returns 9 if 9 seems to act as a missing value indicator
#in the variable
#identifyMissNine <- function(v) {
#  nines <- v == 9
#  if (!any(nines)) return(NULL)
#  if (max(v)>9) return(NULL)
#  if (length(unique(v))>8) return(NULL)
#  9
#}

#For numeric/integer variables only!
#Identify occurences of num in v. If e.g. num is 8, both 8, 88, 888, ...,  and -8, -88, ...,
#are identified. However, an occurrence only "counts" if it is the min/max value of the variable
#and it is seperated from the next biggest (smallest) value by at least 1. 
identifyMissNumber <- function(v, num, allOcc = TRUE) {
  #browser()
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
    #numeric
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

    outMessage <- messageGenerator(list(problem = problem, problemValues = problemValues),
                                   nMax = nMax)
    checkResult(list(problem = problem, message = outMessage, problemValues = problemValues))
}

#labbeled variables
identifyMissingL <- function(v, nMax) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyMissingCF(v, nMax = nMax)
}

#numerical and integer variables
identifyMissingNI <- function(v, nMax, maxDecimals) {
  v <- na.omit(v)
  problem <- FALSE
  problemValues <- NULL
  finiteInd <- is.finite(v)

  missNinesOcc <- identifyMissNumber(v[finiteInd], 9, FALSE) #what 99, -9, -999, 9, ... values occur?
  missEightsOcc <- identifyMissNumber(v[finiteInd], 8, FALSE) #what -8, -88, 8, 888, ... values occur?
  missNaNOcc <- unique(v[!finiteInd]) #what NaN, Inf, ... values occur?
  allProblemOcc <- c(missNinesOcc, missEightsOcc, missNaNOcc)

  if (length(allProblemOcc) > 0) {
    problemValues <- round(allProblemOcc, maxDecimals)
    problem <- TRUE
  }
  outMessage <- messageGenerator(list(problem = problem, problemValues = problemValues),
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, problemValues = problemValues))
}

#logical (B = boolean) variables
#Note: for v to have class logical, only T, F and NA can occur, thus there are
#no potentially miscoded missing values.
#Note: this function catches variables that only consist of NA's, thus it is
#safe to omit NAs in the other functions, without risking obtaining an empty vector
identifyMissingB <- function(v, nMax) {
  checkResult(list(problem = FALSE, message = "", problemValues = NULL))
}
