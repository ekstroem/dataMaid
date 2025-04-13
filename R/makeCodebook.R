#' Produce a data codebook
#'
#' Make a data codebook that summarizes the contents of a dataset.
#' The result is saved to an R markdown file which can be
#' rendered into an easy-to-read codebook in pdf, html or word formats. 
#'
#' @param data The dataset to be checked. This dataset should be of class \code{data.frame},
#' \code{tibble} or \code{matrix}. If it is of class \code{matrix}, it will be converted to a
#' \code{data.frame}.
#' 
#' @param vol Extra text string or numeric that is appended on the end of the output
#' file name(s). For example, if the dataset is called "myData", no file argument is
#'  supplied and \code{vol=2}, the output file will be called "codebook_myData2.Rmd"
#' 
#' @param reportTitle A text string. If supplied, this will be the printed title of the
#' report. If left unspecified, the title with the name of the supplied dataset.
#'
#' @param file The filename of the outputted rmarkdown (.Rmd) file.
#' If set to \code{NULL} (the default), the filename will be the name of \code{data}
#' prefixed with "codebook_", if this qualifies as a valid file name (e.g. no special
#' characters allowed). Otherwise, \code{makeCodebook()} tries to create a valid filename by
#' substituting illegal characters. Note that a valid file is of type .Rmd, hence all
#' filenames should have a ".Rmd"-suffix.
#'
#' @param ... Additional parameters passed to \code{makeDataReport}.
#' 
#' @references Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \doi{10.18637/jss.v090.i06}).
#' @export
makeCodebook <- function(data, vol="", reportTitle=NULL, file=NULL, ...) {

    
    dfname <- deparse(substitute(data))


    Call <- match.call(expand.dots = TRUE)

    Call$reportTitle <- ifelse(is.null(reportTitle), paste0("Codebook for ", dfname), reportTitle)
    Call$file <- ifelse(is.null(file), normalizeFileName(paste0("codebook_", dfname, vol, ".Rmd")), file)
    Call$codebook <- TRUE
#    Call$mode <- ifelse(is.null(Call[["mode"]]), c("summarize", "visualize", "check"), Call[["mode"]])
#Note: ifelse() can only return objects of length 1. Code above returns first element of vector ("summarize")
    if (is.null(Call[["mode"]])) {
      Call$mode <- c("summarize", "visualize", "check")
    } 
    if (is.null(Call[["checks"]])) {
      Call$checks <- setChecks(
        character = "showAllFactorLevels",
        factor = "showAllFactorLevels",
        labelled = "showAllFactorLevels",
        haven_labelled = "showAllFactorLevels",
        numeric=NULL,
        integer=NULL,
        Date=NULL,
        logical=NULL)
    }
#    Call$checks <- ifelse(is.null(Call[["checks"]]), setChecks(
#                                                         character = list("showAllFactorLevels"),
#                                                         factor = list("showAllFactorLevels"),
#                                                         labelled = list("showAllFactorLevels"),
#                                                         numeric=NULL,
#                                                         integer=NULL,
#                                                         Date=NULL,
#                                                         logical=NULL
#                                                     ), Call[["checks"]])
    Call$maxProbVals <- ifelse(is.null(Call[["maxProbVals"]]), Inf, Call[["maxProbVals"]])
    Call$listChecks <-  ifelse(is.null(Call[["listChecks"]]), FALSE, Call[["listChecks"]])
    Call$smartNum <- ifelse(is.null(Call[["smartNum"]]), FALSE, Call[["smartNum"]])


    Call[[1]] <- quote(dataMaid::makeDataReport)
    eval.parent(Call)

    
}



showAllFactorLevels <- function(v, ...) {
    
    ## Define the message displayed if a problem is found:
    problemMessage <- "Observed factor levels:"
    
    ## Initialize the problem indicator (`problem`) and 
    ## the faulty values (`problemValues`)
    problem <- FALSE
    problemValues <- levels(v)

    ## If any problem values are identified, set the problem indicator
    ## accordingly
    if (length(problemValues) > 0) {
        problem <- TRUE
    } else {
        if ("character" %in% class(v)) {
            problemValues <- sort(unique(v))
            problem <- TRUE
        } else {
            if (any(c("labelled", "haven_labelled") %in% class(v))) {
                problemValues <- names(attr(v, "labels", exact=TRUE))
                problem <- TRUE
            }
        }
    }
    
    ## Combine the problem indicator and the problem values
    ## into a problem status object that can be passed to 
        ## the messageGenerator() helper function that will
    ## make sure the outputted message is properly escaped
    ## for inclusion in the dataMaid report
    problemStatus <- list(problem = problem, 
                          problemValues = problemValues)
    outMessage <- messageGenerator(problemStatus, problemMessage, ...)
    
    ## Output a checkResult with the problem, the escaped
    ## message and the raw problem values.
    checkResult(list(problem = problem,
                     message = outMessage,
                     problemValues = problemValues))        
}

#showAllFactorLevels <- checkFunction(showAllFactorLevels,
#                                     description = "All levels of factor/character",
#                                     classes = c("character", "factor", "labelled"))
#}
