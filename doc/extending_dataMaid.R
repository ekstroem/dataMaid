## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)
library(dataMaid)
Sys.setenv(TZ="Europe/Copenhagen")  ## Set time zone to prevent warnings
Sys.getenv("TZ")

## ----allClasses----------------------------------------------------------
allClasses()

## ----defineCountZeros----------------------------------------------------
countZeros <- function(v, ...) {
  val <- length(which(v == 0))
  summaryResult(list(feature = "No. zeros", result = val, value = val))
}

## ----exampleCountZeros---------------------------------------------------
#Called on a numeric variable
countZeros(c(rep(0, 5), 1:100))

#Called on a character variable
countZeros(c(rep(0, 5), letters))

## ----makeCountZerosSumFun------------------------------------------------
countZeros <- summaryFunction(countZeros,
  description = "Count number of zeros",
  classes = c("character", "factor", "integer",
              "labelled", "numeric"))

## ----meanSummaryGeneric--------------------------------------------------
meanSummary <- function(v, maxDecimals = 2) {
  UseMethod("meanSummary")
}

## ----meanSummaryNoMethod, error = TRUE-----------------------------------
meanSummary(1)

## ----meanSummaryHelperDef------------------------------------------------
meanSummaryHelper <- function(v, maxDecimals) {
  #remove missing observations
  v <- na.omit(v)
  
  #compute mean and store "raw" output in `val`
  val <- mean(v)
  
  #store printable (rounded) output in `res`
  res <- round(val, maxDecimals)
  
  #output summaryResult
  summaryResult(list(feature = "Mean", result = res, value  = val))
}

## ----meanSummaryAssignMethods--------------------------------------------
#logical
meanSummary.logical <- function(v, maxDecimals = 2) {
  meanSummaryHelper(v, maxDecimals)
}

#numeric
meanSummary.numeric <- function(v, maxDecimals = 2) {
  meanSummaryHelper(v, maxDecimals)
}

#integer
meanSummary.integer <- function(v, maxDecimals = 2) {
  meanSummaryHelper(v, maxDecimals)
}

## ----meanSummaryExample, error = TRUE------------------------------------
#called on a numeric variable (supported)
meanSummary(rnorm(100))

#called on a character variable - produces error as there is
#no method for characters
meanSummary(letters)

## ----meanSummaryAsSummaryFunction----------------------------------------
meanSummary <- summaryFunction(meanSummary,  
                               description = "Compute arithmetic mean")

## ----allSummFunctions, collapse = TRUE-----------------------------------
allSummaryFunctions()

## ----mosiacplotExample---------------------------------------------------
#construct a character variable by sampling 100 values that are 
#either "a" (probability 0.3) or "b" (probability 0.7):
x <- sample(c("a", "b"), size = 100, replace = TRUE,
                   prob = c(0.3, 0.7))

#draw a mosaic plot of the distribution:
mosaicplot(table(x))

## ----defineMosaicVis-----------------------------------------------------
mosaicVisual <- function(v, vnam, doEval) {
  #Define a (unevaluated) call to mosaicplot
  thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
  
  #if doEval is TRUE, evaluate the call, thereby producing a plot
  #if doEval is FALSE, return the deparsed call
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

## ----exampleMosVis-------------------------------------------------------
mosaicVisual(x, "Variable x", doEval = FALSE)

## ----makeMosVisVisFun----------------------------------------------------
mosaicVisual <- visualFunction(mosaicVisual,
                               description = "Mosaic plots using graphics",
                               classes = setdiff(allClasses(), 
                                                 c("numeric", 
                                                   "integer", 
                                                   "Date")))

## ----definePrettierHistHelper--------------------------------------------
library(ggplot2)

prettierHistHelper <- function(v, vnam) {
  #define a ggplot2 histogram 
  p <- ggplot(data.frame(v = v), aes(x = v)) + 
          geom_histogram(col = "white", bins = 20) +
          xlab(vnam)
  
  #return the plot
  p
}

## ----prettierHistHelperExample-------------------------------------------
prettierHistHelper(rnorm(100), "Standard normal variable")

## ----definePrettierHist--------------------------------------------------
#define visualFunction-style prettierHist()-function
prettierHist <- function(v, vnam, doEval = TRUE) {
  #define the call
  thisCall <- call("prettierHistHelper", v = v, vnam = vnam)
  
  #evaluate or deparse
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#Make it a proper visualFunction:
prettierHist <- visualFunction(prettierHist, 
    description = "ggplot2 style histogram with contours",
    classes = c("numeric", "integer", "logical", "Date"))

## ----showAllVisFunc------------------------------------------------------
allVisualFunctions()

## ----defineIsID----------------------------------------------------------
isID <- function(v, nMax = NULL, ...) {
  #define minimal output. Note that this is not a
  #proper checkResult
  out <- list(problem = FALSE, message = "")
  
  #only perform check if the variable is neither a logical nor a Date
  if (class(v) %in% setdiff(allClasses(), c("logical", "Date"))) {
    
    #count the number of characters in each entry of v
    v <- as.character(v)
    lengths <- c(nchar(v))
    
    #check if all entries of v have at least 8 characters
    #and whether they all have the same length. If so,
    #flag as a problem.
    if (all(lengths >= 8) & length(unique(lengths)) == 1) {
      out$problem <- TRUE
      out$message <- "Warning: This variable seems to contain ID codes."
    }
  }

  #return result of the check
  out
}

## ----isIDexample---------------------------------------------------------
#define 9-character ID variable:
idVar <- c("1234-1233", "9221-0289",
           "9831-1201", "6722-1243")

#check for ID resemblance for the ID variable
isID(idVar)

#check for ID resemblance for a non-ID variable
isID(rnorm(10))


## ----a1------------------------------------------------------------------
identifyColons <- function(v, nMax = Inf, ... ) {
  #remove duplicates (for speed) and missing values:
  v <- unique(na.omit(v))
  
  #Define the message displayed if a problem is found:
  problemMessage <- "Note that the following values include colons:"
  
  #Initialize the problem indicator (`problem`) and 
  #the faulty values (`problemValues`)
  problem <- FALSE
  problemValues <- NULL
  
  #Identify values in v that has the structure: First something (.), 
  #then a colon (:), and then something again (.), i.e. values with
  #non-trailing colons: 
  problemValues <- v[sapply(gregexpr(".:.", v),
                            function(x) all(x != -1))]

  #If any problem values are identified, set the problem indicator
  #accordingly
  if (length(problemValues) > 0) {
    problem <- TRUE
  }

  #Combine the problem indicator and the problem values
  #into a problem status object that can be passed to 
  #the messageGenerator() helper function that will
  #make sure the outputted message is properly escaped
  #for inclusion in the dataMaid report
  problemStatus <- list(problem = problem, 
                        problemValues = problemValues)
  outMessage <- messageGenerator(problemStatus, problemMessage, nMax)

  #Output a checkResult with the problem, the escaped
  #message and the raw problem values.
  checkResult(list(problem = problem,
                   message = outMessage,
                   problemValues = problemValues))
}

## ----identifyColonsIsCheckFunction---------------------------------------
identifyColons <- checkFunction(identifyColons,
    description = "Identify non-trailing colons",
    classes = c("character", "factor", "labelled"))

## ----identifyColonsPosExample--------------------------------------------
#define a variable as an interaction between between two factors:
iaVar <- factor(c("a", "b", "a", "c")):factor(c(1, 2, 3, 4))

#Check iaVar for colons:
identifyColons(iaVar)

## ----identifyColonsNegExample--------------------------------------------
identifyColons(letters)

## ----headArtData---------------------------------------------------------
data(artData)
head(artData, 5)

## ----makeReport, message  = FALSE, warning = FALSE, eval=rmarkdown::pandoc_available()----
makeDataReport(artData,
  #add extra precheck function         
  preChecks = c("isKey", "isSingular", "isSupported", "isID"),
      
  #Add the extra summaries - countZeros() for character, factor, 
  #integer, labelled and numeric variables and meanSummary() for integer, 
  #numeric and logical variables:
  summaries = setSummaries(
    character = defaultCharacterSummaries(add = "countZeros"),
    factor = defaultFactorSummaries(add = "countZeros"),
    labelled = defaultLabelledSummaries(add = "countZeros"),
    numeric = defaultNumericSummaries(add = c("countZeros", "meanSummary")),
    integer = defaultIntegerSummaries(add = c("countZeros", "meanSummary")),
    logical = defaultLogicalSummaries(add =  c("meanSummary"))
  ),
  
  #choose mosaicVisual() for categorical variables,
  #prettierHist() for all others:
  visuals = setVisuals(
    factor = "mosaicVisual",
    numeric = "prettierHist",
    integer = "prettierHist", 
    Date = "prettierHist"
  ), 
      
  #Add the new checkFunction, identifyColons, for character, factor and 
  #labelled variables:
  checks = setChecks(
    character = defaultCharacterChecks(add = "identifyColons"),
    factor = defaultFactorChecks(add = "identifyColons"),
    labelled = defaultLabelledChecks(add = "identifyColons")
  ),
  
  #overwrite old versions of the report, render to html and don't 
  #open the html file automatically:
  replace = TRUE, 
  output = "html",
  open = FALSE
)

## ----includeReport, eval=rmarkdown::pandoc_available()-------------------
htmltools::includeHTML("dataMaid_artData.html")

