#GENERAL COMMENTS/TO DO:
#QUESTION: We want to do the same thing in some functions,
#           no matter the data class.
#           Should I implement a generic function + class specific
#           functions anyway for readility/comparibility with the
#           rest of the code?
#           Relevant functions: variableType, countMissing

#IDEA: Make it easier for users to append their own functions
#     to the standard functions so that they don't have to write
#     it all out


###################################################################

#Make matrix containing summary information about a variable v,
#depending on its class and possibly user supplied arguments
#summary-functions must take a vector v and return a
#list(feature=..., result=...) object.
summarize <- function(v, descriptive=F, ...) UseMethod("summarize")

#NOTE: default descriptions can still be accesssed using defaultCharacterDescriptions() etc.,
#but they are not available as arguments to clean() directly, but should be passed in ...


#produces the output matrix from a summarize call
#use internally only
sumMatGenerator <- function(v, summaries) {
  nFunctions <- length(summaries)
  outMat <- matrix(NA, nFunctions, 2,
                   dimnames=list(NULL, c("Feature", "Result")))
  for (i in 1:nFunctions) {
    res <- eval(call(summaries[i], v))
    outMat[i, "Feature"] <- res$feature
    outMat[i, "Result"] <- res$result
  }
  outMat
}


defaultCharacterSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultCharacterDescriptions <- function() "centralValue"

summarize.character <- function(v, characterSummaries = defaultCharacterSummaries(),
                                characterDescriptions = defaultCharacterDescriptions(),
                                descriptive=F, ...) {
  characterCalls <- if (descriptive) c(characterSummaries, characterDescriptions)
                    else characterSummaries
  sumMatGenerator(v, characterCalls)
}


defaultFactorSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultFactorDescriptions <- function() "centralValue"

summarize.factor <- function(v, factorSummaries = defaultFactorSummaries(),
                             factorDescriptions = defaultFactorDescriptions(),
                             descriptive=F, ...) {
  factorCalls <- if (descriptive) c(factorSummaries, factorDescriptions)
                 else factorSummaries
  sumMatGenerator(v, factorCalls)
}


defaultLabelledSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultLabelledDescriptions <- function() "centralValue"

summarize.labelled <- function(v, labelledSummaries = defaultLabelledSummaries(),
                               labelledDescriptions = defaultLabelledDescriptions(),
                               descriptive=F, ...) {
  labelledCalls <- if (descriptive) c(labelledSummaries, labelledDescriptions)
                   else labelledSummaries
  sumMatGenerator(v, labelledCalls)
}


defaultNumericSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultNumericDescriptions <- function() c("centralValue", "quartiles", "minMax")

summarize.numeric <- function(v, numericSummaries = defaultNumericSummaries(),
                              numericDescriptions = defaultNumericDescriptions(),
                              descriptive=F, ...) {
  numericCalls <- if (descriptive) c(numericSummaries, numericDescriptions)
                  else numericSummaries
  sumMatGenerator(v, numericCalls)
}


defaultIntegerSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultIntegerDescriptions <- function() c("centralValue", "quartiles", "minMax")

summarize.integer <- function(v, integerSummaries = defaultIntegerSummaries(),
                              integerDescriptions = defaultIntegerDescriptions(),
                              descriptive=F, ...) {
  integerCalls <- if (descriptive) c(integerSummaries, integerDescriptions)
                  else integerSummaries
  sumMatGenerator(v, integerCalls)
}


defaultLogicalSummaries <- function() c("variableType", "countMissing", "uniqueValues")
defaultLogicalDescriptions <- function() "centralValue"

summarize.logical <- function(v, logicalSummaries = defaultLogicalSummaries(),
                              logicalDescriptions = defaultLogicalDescriptions(),
                              descriptive=F, ...) {
  logicalCalls <- if (descriptive) c(logicalSummaries, logicalDescriptions)
                  else logicalSummaries
  sumMatGenerator(v, logicalCalls)
}


####################################################################

#####variableType######

#Returns the class of the variable, as registered in R.

variableType <- function(v) {
  list(feature="Variable type", result=oClass(v))
}
  #oClass is defined in clean.R - is this a problem?


####################################################################

#####countMissing######

#Counts the number of missing values (coded as NA) in the variable

countMissing <- function(v) {
  noMissing <- sum(is.na(v))
  percentMissing <- round(100*noMissing/length(v),2)
  list(feature="No. missing obs." ,
       result=paste(noMissing, " (", percentMissing," %)", sep=""))
}

####################################################################

#####uniqueValues######

#Counts the number of unique (non-NA) values taken by a variable

uniqueValues <- function(v) UseMethod("uniqueValues")

#' @importFrom stats na.omit
uniqueValuesCFLBI <- function(v) {
  noUnique <- length(unique(na.omit(v)))
  list(feature="No. unique values", result=noUnique)
}

uniqueValuesN <- function(v) {
  out <- uniqueValuesCFLBI(v)
  #check for NaNs
  if (any(is.nan(v))) out$result <- out$result + 1
  out
}


#assign methods to generic uniqueValues function
uniqueValues.character <- function(v) uniqueValuesCFLBI(v)
uniqueValues.factor <- function(v) uniqueValuesCFLBI(v)
uniqueValues.labelled <- function(v) uniqueValuesCFLBI(v) #?PROBLEM?
uniqueValues.numeric <- function(v) uniqueValuesN(v)
uniqueValues.integer <- function(v) uniqueValuesCFLBI(v)
uniqueValues.logical <- function(v) uniqueValuesCFLBI(v)

####################################################################

#Optinal descriptive functions below here

descriptiveFunctions <- c("centralValue", "minMax", "quartiles")


#####centralValue#####

centralValue <- function(v) UseMethod("centralValue")

#Find the central value (mode for categorical, median for
#numeric variables) of a variable, ignoring NA and NaN values
#for numeric/integer variables and ignoring NA for
#character/factor variables

#logical variables
centralValueB <- function(v) {
  vCats <- unique(v)
  vMode <- vCats[which.max(table(v, exclude=NULL))][1]
  list(feature="Mode", result=paste("\"", vMode, "\"", sep=""))
}

#character and factor variables
#' @importFrom stats median
centralValueCF <- function(v) {
  centralValueB(na.omit(v))
}

#labelled variables
centralValueL <- function(v) {
  #PLACE HOLDER
  list(feature="Mode", result="?labelled?")
}

#integer and numeric variables

#''
#' @importFrom stats median
centralValueIN <- function(v) {
  v <- na.omit(v)
  list(feature="Median", result=median(v))
}


#assign methods to generic centralValue function
centralValue.character <- function(v) centralValueCF(v)
centralValue.factor <- function(v) centralValueCF(v)
centralValue.labelled <- function(v) centralValueL(v)
centralValue.numeric <- function(v) centralValueIN(v)
centralValue.integer <- function(v) centralValueIN(v)
centralValue.logical <- function(v) centralValueB(v)





#######minMax#########

#returns min and max of v. Should only be used on numeric/integer
#variables. CONTROL THIS?

minMax <- function(v) {
  v <- na.omit(v) #maybe keep Inf's?
  list(feature="Min. and max.", result=paste(min(v), max(v), sep=", "))
}


#######quartiles#########

#returns 1st and 3rd quartiles of v. Should only be used on numeric/integer
#variables. CONTROL THIS?

quartiles <- function(v) {
  v <- na.omit(v) #maybe keep Inf's?
  list(feature="1st and 3rd quartiles", result = paste(quantile(v, c(0.25, 0.75)), collapse=", "))
}




