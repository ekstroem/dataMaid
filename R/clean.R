#' Produce a data cleaning overview document (deprecated version)
#'
#' NOTE: This function has been replaced by \code{\link{makeDataReport}}. The current 
#' function is no longer updated and it is only included for backwards compatability. 
#' 
#' Run a set of class-specific validation checks to check the
#' variables in a dataset for potential errors.  Performs checking
#' steps according to user input and/or data type of the inputted
#' variable.  The checks are saved to an R markdown file which can
#' rendered into an easy-to-read document.  This document also
#' includes summaries and visualizations of each variable in the
#' dataset.
#'
#' For each variable, a set of pre-check (controlled by the
#' \code{preChecks} argument) is first run and then then a battery of
#' functions are applied depending on the variable class.  For each
#' variable type the summarize/visualize/check functions are applied
#' and and the results are written to an R markdown file.
#'
#' @param data The dataset to be checked. This dataset should be of class \code{data.frame},
#' \code{tibble} or \code{matrix}. If it is of classs \code{matrix}, it will be converted to a
#' \code{data.frame}.
#'
#' @param output
#' Output format. Options are \code{"pdf"} (the default), and \code{"html"}
#'
#' @param render Should the output file be rendered (defaults to \code{TRUE}),
#' i.e. should a pdf/html document be generated and saved to the disc?
#'
#' @param useVar Variables to clean. If \code{NULL} (the default), all variables in \code{data}
#' are included. If a vector of variable names is supplied, only the variables in \code{data} that are
#' also in \code{useVar} are included in the data cleaning overview document.
#'
#' @param ordering Choose the ordering of the variables in the variable presentation. The options
#' are "asIs" (ordering as in the dataset) and "alphabetical" (alphabetical order).
#'
#' @param onlyProblematic A logical. If \code{TRUE}, only the variables flagged as
#' problematic in the check step will be included in the variable list.
#'
#' @param labelled_as A string explaining the way to handle labelled vectors.
#' Currently \code{"factor"} (the default) is the only possibility. This means that labelled
#' variables that appear factor-like (by having a non-\code{NULL} \code{labels}-attribute) will
#' be treated as factors, while other labelled variables will be treated as whatever base
#' variable class they inherit from.
#'
#'
#' @param mode Vector of tasks to perform among the three categories "summarize", "visualize" and "check".
#' The default, \code{c("summarize", "visualize", "check")}, implies that all three steps are
#' performed. The steps selected in \code{mode} will be performed for each variable in
#' \code{data} and their results are presented in the second part of the outputtet data cleaning
#' overview document. The "summarize" step is responsible for creating the summary table,
#' the "visualize" step is responsible for creating the plot and the "check" step is responsible
#' for performing checks on the variable and printing the results if any problems are found.
#'
#' @param characterChecks A vector of the names of error-checking functions to apply to
#' character vectors.
#'
#' @param factorChecks A vector of the names of error-checking functions to apply to
#' integer vectors.
#'
#' @param labelledChecks A vector of the names of error-checking functions to apply to
#' character vectors.
#'
#' @param integerChecks A vector of the names of error-checking functions to apply to
#' integer vectors.
#'
#' @param numericChecks A vector of the names of error-checking functions to apply to
#' numeric vectors.
#'
#' @param logicalChecks A vector of the names of error-checking functions to apply to
#' logical vectors.
#'
#' @param dateChecks A vector of the names of error-checking functions to apply to
#' Date vectors.
#'
#' @param allChecks Vector of function names that should be used as check-functions
#' for all variable types. Note that this argument overwrites the arguments
#' \code{characterChekcs}, \code{factorChecks}, etc.
#'
#' @param smartNum If \code{TRUE} (the default), numeric and integer variables with
#' less than 5 unique values are treated as factor variables in the checking,
#' visualization and summary steps, and a message notifying the reader of this is
#' printed in the data summary.
#'
#' @param preChecks Vector of function names for check functions used in the pre-check stage.
#' The pre-check stage consists of variable checks that should be performed before the
#' summary/visualization/checking step. If any of these checks find problems, the variable
#' will not be summarized nor visualized nor checked.
#'
#' @param file The filename of the outputted rmarkdown (.Rmd) file.
#' If set to \code{NULL} (the default), the filename will be the name of \code{data}
#' prefixed with "dataMaid_", if this qualifies as a valid file name (e.g. no special
#' characters allowed). Otherwise, \code{clean()} tries to create a valid filename by
#' substituing illegal characters. Note that a valid file is of type .Rmd, hence all
#' filenames should have a ".Rmd"-suffix.
#'
#' @param replace If \code{FALSE} (the default), an error is thrown if one of the files
#' that we are about to be created (.Rmd overview file and possible also a .html or .pdf
#' file) already exist. If \code{TRUE}, no checks are performed and files on disc thus
#' might be overwritten.
#'
#' @param vol Extra text string or numeric that is appended on the end of the output
#' file name(s). For example, if the dataset is called "myData", no file argument is
#'  supplied and \code{vol=2}, the output file will be called "dataMaid_myData2.Rmd"
#'
#' @param characterSummaries A vector of the names of summary functions to apply to
#' character vectors.
#'
#' @param factorSummaries A vector of the names of summary functions to apply to
#' factor vectors.
#'
#' @param labelledSummaries A vector of the names of summary functions to apply to
#' labelled vectors.
#'
#' @param numericSummaries  A vector of the names of summary functions to apply to
#' numeric vectors.
#'
#' @param integerSummaries A vector of the names of summary functions to apply to
#' integer vectors.
#'
#' @param logicalSummaries A vector of the names of summary functions to apply to
#' logical vectors.
#'
#' @param dateSummaries  A vector of the names of summary functions to apply to
#' Date vectors.
#'
#' @param allSummaries Vector of function names that should be used as summary
#' functions for all variable types. Note that this argument overwrites the arguments
#' \code{characterSummaries}, \code{factorSummaries}, etc.
#'
#' @param allVisuals A single function name. This funtion name is called for
#' creating the plots for each variable in the "visualize" step. The default,
#' \code{"standardVisual"} thus calls the \code{\link{visualFunction}}
#' \code{\link{standardVisual}} for each variable in \code{data}.
#'
#' @param standAlone A logical. If \code{TRUE}, the document begins with a
#' markdown YAML preamble such that it can be rendered as a stand alone rmarkdown
#' file, e.g. by calling \code{\link{render}}. If \code{FALSE}, this preamble is removed.
#' Moreover, no matter the input to the \code{render} argument, the document will now
#' not be rendered, as it has no preamble.
#'
#' @param twoCol A logical. Should the results from the \emph{summarize} and \emph{visualize}
#' steps be presented in two columns? Defaults to \code{TRUE}.
#'
#' @param quiet A logical. If \code{TRUE} (the default), only a few messages
#' are printed to the screen as \code{clean} runs. If \code{FALSE}, no messages are
#' suppressed. The third option, \code{silent}, renders the function completely
#' silent, such that only fatal errors are printed.
#'
#' @param openResult A logical. If \code{TRUE} (the default), the last file produced
#' by \code{clean} is automatically opened by the end of the function run. This
#' means that if \code{render = TRUE}, the rendered pdf or html file is opened, while
#' if \code{render = FALSE}, the .Rmd file is opened.
#'
#' @param listChecks A logical. Controls whether what checks that were used for each
#' possible variable type are summarized in the output. Defaults to \code{TRUE}.
#'
#' @param maxProbVals A positive integer or \code{Inf}. Maximum number of unique
#' values printed from check-functions. In the case of \code{Inf}, all problematic 
#' values are printed. Defaults to \code{10}.
#'
#' @param maxDecimals A positive integer or \code{Inf}. Number of decimals used when
#' printing numerical values in the data summary and in problematic values from the
#' data checks. If \code{Inf}, no rounding is performed.
#'
#' @param addSummaryTable A logical. If \code{TRUE} (the default), a summary table
#' of the variable checks is added between the Data Cleaning Summary and the
#' Variable List.
#'
#' @param reportTitle A text string. If supplied, this will be the printed title of the
#' report. If left unspecified, the title with the name of the supplied dataset.
#'
#' @param treatXasY A list that indicates how non-standard variable classes should be treated.
#' This parameter allows you to include variables that are not of class \code{factor}, \code{character}, 
#' \code{labelled}, \code{numeric}, \code{integer}, \code{logical} nor \code{Date} (or a class
#' that inherits from any of these classes). The names of the list are the new classes and the entries
#' are the names of the class, they should be treated as. If \code{clean()} should e.g. treat variables of 
#' class \code{raw} as characters and variables of class \code{complex} as numeric, you should put
#' \code{treatXasY = list(raw = "character", complex = "numeric")}.
#'
#' @param \dots FIX ME-------- Other arguments that are passed on the to precheck,
#' checking, summary and visualization functions.WHAT ARGUMENTS ARE RELEVANT TO MENTION
#'  HERE?  ---------- FIX ME
#'
#' @return The function does not return anything. Its side effect (the production
#' of a data cleaning overview document) is the reason for running the function.
#'
#' @examples
#' data(testData)
#' data(toyData)
#'
#' check(toyData)
#'
#'  \dontrun{
#' DF <- data.frame(x = 1:15)
#' clean(DF)
#' }
#'
#' \dontrun{
#' data(testData)
#' clean(testData)
#' }
#'
#' # Overwrite any existing files generated by clean
#' \dontrun{
#' clean(testData, replace=TRUE)
#' }
#'
#' # Only include problematic variables in the output document
#' \dontrun{
#' clean(testData, replace=TRUE, onlyProblematic=TRUE)
#' }
#'
#' # Add user defined check-function to the checks performed on character variables:
#' # Here we add functionality to search for the string wally (ignoring case)
#' \dontrun{
#' wheresWally <- function(v, ...) {
#'      res <- grepl("wally", v, ignore.case=TRUE)
#'      problem <- any(res)
#'      message <- "Wally was found in these data"
#'      checkResult(list(problem = problem,
#'                       message = message,
#'                       problemValues = v[res]))
#' }
#'
#' wheresWally <- checkFunction(wheresWally,
#'                              description = "Search for the string 'wally' ignoring case",
#'                              classes = c("character")
#'                              )
#' # Add the newly defined function to the list of checks used for characters.
#' clean(testData, characterChecks=c(defaultCharacterChecks(), "wheresWally"),
#'       replace=TRUE)
#' }
#' 
#' #Handle non-supported variable classes using treatXasY: treat raw as character and
#' #treat complex as numeric. We also add a list variable, but as lists are not 
#' #handled through treatXasY, this variable will be caught in the preChecks and skipped:
#' \dontrun{
#' toyData$rawVar <- as.raw(c(1:14, 1))
#' toyData$compVar <- c(1:14, 1) + 2i
#' toyData$listVar <- as.list(c(1:14, 1))
#' clean(toyData, replace  = TRUE, treatXasY = list(raw = "character", complex = "numeric"))
#' }
#'
#' @importFrom methods is
#' @importFrom pander pander_return panderOptions pandoc.table.return
#' @importFrom tools file_ext
#' @importFrom utils packageVersion sessionInfo capture.output packageDescription
#' @importFrom magrittr %>%
#' @export
clean <- function(data, output=c("pdf", "html"), render=TRUE,
                  useVar=NULL, ordering=c("asIs", "alphabetical"), onlyProblematic=FALSE,
                  labelled_as=c("factor"),
                  mode=c("summarize", "visualize", "check"),
                  smartNum=TRUE, preChecks=c("isKey", "isSingular", "isSupported"),
                  file=NULL, replace=FALSE, vol="",
                  standAlone=TRUE, twoCol=TRUE,
                  quiet = TRUE,
                  openResult=TRUE,
                  characterChecks = defaultCharacterChecks(),
                  factorChecks = defaultFactorChecks(),
                  labelledChecks = defaultLabelledChecks(),
                  numericChecks = defaultNumericChecks(),
                  integerChecks = defaultIntegerChecks(),
                  logicalChecks = defaultLogicalChecks(),
                  dateChecks = defaultDateChecks(),
                  allChecks = NULL,
                  characterSummaries = defaultCharacterSummaries(),
                  factorSummaries = defaultFactorSummaries(),
                  labelledSummaries = defaultLabelledSummaries(),
                  numericSummaries = defaultNumericSummaries(),
                  integerSummaries = defaultIntegerSummaries(),
                  logicalSummaries = defaultLogicalSummaries(),
                  dateSummaries = defaultDateSummaries(),
                  allSummaries = NULL,
                  allVisuals = "standardVisual",
                  listChecks = TRUE,
                  maxProbVals = 10,
                  maxDecimals = 2,
                  addSummaryTable = TRUE,
                  reportTitle = NULL,
                  treatXasY = NULL,
                  ...) {

    .Deprecated("makeDataReport")
##    warning(paste("Please note that clean() has been replaced by makeDataReport().",
##                 "This means that clean() is no longer maintained,",
##                  "and we therefore recommend all users to use makeDataReport() instead.",
##                 "This function will be removed from the package in the future."))
  
    ## Store the original call
    orig.call <- match.call()

    ## Start by doing a few sanity checks of the input
    if (! (is(data, "data.frame") )) {
        ## tibble is automatically a data frame
        if (is.matrix(data)) {
            data <- as.data.frame(data)
        } else stop("clean requires a data.frame, tibble or matrix as input")
    }

    #Check treatXasY argument
      ## Supported variable classes 
      allClasses <- c("character", "factor", "labelled", "numeric", "integer", 
                      "logical", "Date")
      if (!is.null(treatXasY)) {
        if (!is.list(treatXasY)) {
          warning("The supplied treatXasY argument was invalid and therefore, it was ignored.")
          treatXasY <- NULL
        } else if (!all(unlist(treatXasY) %in% allClasses)) {
          probPl <- !(unlist(treatXasY) %in% allClasses)
          warning(paste("The treatXasY argument specified for: ",
                        paste(names(treatXasY)[probPl], 
                              "variables to be treated as",
                              unlist(treatXasY[probPl]), 
                                     collapse =", "),
                        ". But the right hand side classes ",
                        "are not supported by dataMaid and therefore ",
                        "entries of treatXasY are ignored.", sep = ""))
          treatXasY[probPl] <- NULL
        }
      }
      
      
    #handle quiet argument
    if (identical(quiet, "silent")) {
      silent <- TRUE
      quiet <- TRUE
    } else {
      silent <- FALSE

      #perhaps check if quiet argument is valid (i.e. TRUE/FALSE) here?
    }
    if (silent) nagUser <- FALSE


    ##Match arguments
    ordering <- match.arg(ordering)
    output <- match.arg(output)
    labelled_as <- match.arg(labelled_as)
      #quiet <- match.arg(quiet)

    ## Extract the dataframe name
    dfname <- deparse(substitute(data))

    the_lhs <- function() {
        parents <- lapply(sys.frames(), parent.env)

        is_magrittr_env <- vapply(parents, identical, logical(1), y = environment(`%>%`))
        
        if (any(is_magrittr_env)) {
            deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
        }
    }


    ## Now if data are added as part of a magrittr pipe then use this "fix"
    if (dfname==".") {
        dfname <- the_lhs()
    }
  
    #If standAlone is FALSE, the document obviously shouldn't be rendered
    if (!standAlone) render <- FALSE

    ##########################################################################################
    #######Secret arguments that were removed for the users but are still implemented#########
    ##########################################################################################

    #If we would ever want (Windows) users not to be nagged
    nagUser <- TRUE

    ##########################################################################################
    ##########################################################################################
    ##########################################################################################

    ## What variables should be used?
    if (!is.null(useVar)) {
        ## The line below is probably not efficient if we have large datasets and want to extract many variables
        ### o <- o[, useVar, drop=FALSE]  #warning here if this doesn't work + overwrite stuff?
        ## Instead run through the dataframe and NULL the variables to exclude?
        ##If we really want to do this, we should probably do it using data.table but there is no way
        ##around creating a local copy of o, as we do NOT want to change the version of o in the
        ##global environment.

      data <- data[, useVar, drop=FALSE]  #warning here if this doesn't work + overwrite stuff?

      ###this does not work, it produces an error!:########################
      #o[names(o)[! names(o) %in% useVar]] <- NULL
      #####################################################################
    }

    ## Background variables
    nvariables <- ncol(data)
    if (ordering == "alphabetical") {
        index <- order(names(data))
    } else index <- 1:nvariables
    n <- nrow(data)
    vnames <- names(data)
    dots <- list(...)

    ## Set the output file name if input is NULL or not Rmd
    if (is.null(file)) {
      if (substr(dfname, 1, 11) == "data.frame(") {
        file <- paste0("dataMaid_unnamedData", vol, ".Rmd")
      } else file <- normalizeFileName(paste0("dataMaid_", dfname, vol, ".Rmd"))
    } else {
      originalFile <- file
      faultyExt <- FALSE
      faultyName <- FALSE
      fileExt <- tolower(tools::file_ext(file))
      ncFN <- nchar(file)
      #if (tolower(substr(file, nchar(file)-3, nchar(file))) != ".rmd") {
      if (fileExt != "rmd") {
        file <- paste(tools::file_path_sans_ext(file), ".Rmd", sep="")
        faultyExt <- TRUE
      }

      ###ADD HERE: deal with e.g. "joe..rmd" or ".rmd"#####
      #if (substr(file, ncFN-4, ncFN-4) == ".") {
      #  file <- paste(substr(file, 1, ncFN-5))
      #  faultyExt <- TRUE
      #}
      #####################################################


      ############PROBLEM: THIS FUNCTION DOES NOT CATCH EVERYTHING WE NEED TO CATCH. WRITE
      ############NEW FUNCTION!###########################################################
      normalizedFile <- normalizeFileName(file)
      ####################################################################################
      ####################################################################################

      if (normalizedFile != file) {
        file <- normalizedFile
        faultyName <- TRUE
      }
      if (!silent && (faultyExt || faultyName)) {
        faultyExtMessage <- "a faulty file extension (not .Rmd)"
        faultyNameMessage <- "reserved characters not allowed in file names"
        message(paste("The supplied file name included",
                      ifelse(faultyExt, faultyExtMessage, ""),
                      ifelse(faultyExt & faultyName, "and", ""),
                      ifelse(faultyName, faultyNameMessage, ""),
                      "and therefore, it was changed from", originalFile,
                      "into", paste(file, ".", sep="")))
      }
    }

    outOutput <- output #copy of output for file extension generation
                        #Note: Changing output itself will cause problems as we need to know
                        #whether we are making a pdf or html .rmd file

    if (!render) outOutput <- "Rmd"

    outFile <- paste0(substring(file, 1, nchar(file)-4), ".", outOutput)
                  #outFile is the file we might want to open at the end. Should be consistent
                  #with the user's choice of output (NOT just .rmd).

    ## check if we are about to overwrite a file
    fileExists <- file.exists(file)
    outFileExists <- file.exists(outFile)

      #if (!replace %in% c("never", "onlyCleanR") && (fileExists || outFileExists)) {
    if (replace) {
        unlink(file)
    } else if (fileExists | outFileExists) {
     # if (replace=="never") {
        if (fileExists & outFileExists) problemFiles <- paste(file, "and", outFile)
        if (fileExists & !outFileExists) problemFiles <- file
        if (!fileExists & outFileExists) problemFiles <- outFile
        stop(paste("The file name(s) to be used by dataMaid,", paste(problemFiles, ",", sep=""),
                   "is(are) already in use.",
                   "We recommend trying one of the following solutions: \n",
                   "- rename your dataMaid output file using the \"file\" option \n",
                   "- Add a volume number to your file name using the \"vol\" option \n",
                   "- check that you do not want to keep the original file and if so,",
                   "use clean() with argument replace = TRUE"))
    }

    #Check if [fileName]_vListTmp.txt already exists and if so, try to
    #make a different temporary file for writing variable results to
    OK <- FALSE
    maxTries <- 101
    i <- 1
    addOns <- c("", 1:100)
    vListFileName <- paste(substring(file, 1, nchar(file)-4),
                           "_vListTmp", sep = "")
    while (!OK & i <= maxTries) {
      OK <- !file.exists(paste(vListFileName, addOns[i], ".txt", sep = ""))
      i <- i + 1
    }
    if (!OK) {
      stop(paste("No unused file names were available for producing a",
                 "temporary file for clean(). Please clean up your",
                 "working directory for files starting with",
                 vListFileName, "and try again."))
    } else vListFileName <- paste(vListFileName, addOns[i-1], ".txt", sep = "")



      #if (replace=="onlyCleanR") {
      #  fileProblem <- F
      #  outputFileProblem <- F
      #
      #  if (fileExists) {
      #    l12 <- readLines(file, 2, warn=FALSE)
      #    if (!identical(l12, c("---", "dataMaid: yes"))) fileProblem <- T
      #  }
      #  if (outFileExists) {
      #    #############################################
      #    #check if pdf/html was produced by dataMaid....
      #    #############################################
      #  }
      #
      #  if (fileProblem & outFileProblem) problemFiles <- paste(file, "and", outFile)
      #  if (fileProblem & !outFileProblem) problemFiles <- file
      #  if (!fileproblem & outFileProblem) problemFiles <- outFile
      #
      #  if (fileProblem || outFileProblem) {
      #    stop(paste("The file name(s) to be used by dataMaid,", paste(problemFiles, ",", sep=""),
      #               "are already in use and the files do not look like they were produced by dataMaid.",
      #               "We recommend trying one of the following solutions: \n",
      #               "- rename your dataMaid output files using the \"file\" option \n",
      #               "- Add a volume number to your file name using the \"vol\" option \n",
      #              "- check that you do not want to keep the original file and if so,",
      #               "use dataMaid with replace = \"always\""))
      #  }
      #}
      #}



    ## Figure out which classes of output that the user requests.
    ## By default we want both checks, graphics, and summarize.
    doCheck <- "check" %in% mode
    doVisualize <- "visualize" %in% mode
    doSummarize <- "summarize" %in% mode

    if (!doCheck & !doVisualize & !doSummarize & !silent) {
        warning("Note that no proper arguments were supplied to \"mode\" - no data cleaning performed")
    } #rewrite warning message

    ## Disregard the twocolumn option if we're only asking for one of visualize and summarize
    ## If output is not html or pdf then drop the twoCol option too
    if (!doVisualize || !doSummarize) twoCol <- FALSE

    #allChecks overwrites other check-options
    if (!is.null(allChecks)) {
      characterChecks <- factorChecks <- labelledChecks <- allChecks
      numericChecks <- integerChecks <- logicalChecks <- allChecks
    }

    #allSummaries overwrites other summary-options
    if (!is.null(allSummaries)) {
      characterSummaries <- factorSummaries <- labelledSummaries <- allSummaries
      numericSummaries <- integerSummaries <- logicalSummaries <- allSummaries
    }

   # if (!(output %in% c("html", "pdf"))) twoCol <- FALSE
    #what is this line supposed to do and when will it happen?

    ## make tables left-aligned and allow for 6 columns
    oldPanderOptions <- pander::panderOptions() # Used to restore towards the end
    ## panderOptions("table.alignment.default", "left")
    pander::panderOptions('table.alignment.default', 'center')  ## XXX CE only one of these two
    pander::panderOptions("table.split.table", Inf)
    pander::panderOptions("table.split.cells", Inf)
    pander::panderOptions('table.alignment.rownames', 'left')

    changedPanderOptions <- c("table.alignment.default", "table.split.table",
                              "table.split.cells", "table.alignment.rownames")

    ##
    ## Below comes a bunch of helper functions for writing the output
    ##
    writer <- function(x, ..., outfile=fileConn, sep="\n") {
        cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
    }

    chunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "warning=FALSE"), label=NULL) {
        writer(paste0("```{r ", ifelse(is.null(label), ", ", paste0(label, ", ")),
                      paste0(options, collapse=", "), "}"),
               outfile = outfile)
        writer(x, ..., outfile = outfile)
        writer("```", outfile = outfile)
    }

    fig.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "fig.width=4",
                                                      "fig.height=3", "message=FALSE",
                                                      "warning=FALSE"), label=NULL) {
        chunk.wrapper(x, outfile=outfile, options=options, label=label)
            #I get an error when label stuff is there
    }

    secretChunk.wrapper <- function(x, ..., outfile=fileConn, options=c("echo=FALSE", "include=FALSE",
                                                              "warning=FALSE", "message=FALSE",
                                                              "error=FALSE"), label=NULL) {
        chunk.wrapper(x, outfile=outfile, options=options, label=label)
    }

    ## outputty sets the output type
    twoCols.wrapper <- function(text, figure, outfile=fileConn, outputty=output, label=NULL) {
        if (outputty=="pdf") { #note: does NOT work if there is a linebreak between the two
                                        #minipage environments!
            writer("\\bminione", outfile = outfile)
            writer(text, outfile = outfile)
            writer("\\emini", outfile = outfile)
            writer("\\bminitwo", outfile = outfile)
            fig.wrapper(figure, label=label, outfile = outfile)
            writer("\\emini", outfile = outfile)
        }
        if (outputty=="html") {
            writer("<div class = \"row\">", outfile = outfile)
            writer("<div class = \"col-lg-8\">", outfile = outfile)
            writer(text, outfile = outfile)
            writer("</div>", outfile = outfile)
            writer("<div class = \"col-lg-4\">", outfile = outfile)
            fig.wrapper(figure, label=label, outfile = outfile)
            writer("</div>", outfile = outfile)
            writer("</div>", outfile = outfile)
        }
        writer("\n", outfile = outfile)
    }


    ## Open file connections
    fileConn <- file(file, "w") #for main document
    vListConn <- file(vListFileName, "w")

    ## Title of the report
    if (is.null(reportTitle)) reportTitle <- dfname


    ## This part is wrapped in a try call to ensure that the connection is closed even if something
    ## breaks down when running the code.
    try({

    ## write YAML preamble
    writer("---")
    writer("dataMaid: yes")
    if (standAlone) {
        writer(paste("title:", reportTitle))
        writer("subtitle: \"Autogenerated data summary from dataMaid\"")
        writer("date: \"`r Sys.time()`\"")
        if (output=="pdf") {
            writer("output: pdf_document")
            writer("documentclass: report")
            writer("header-includes:")
            writer("  - \\renewcommand{\\chaptername}{Part}")
            writer("  - \\newcommand{\\fullline}{\\noindent\\makebox[\\linewidth]{\\rule{\\textwidth}{0.4pt}}}")
            if (twoCol) {
                writer("  - \\newcommand{\\bminione}{\\begin{minipage}{0.75 \\textwidth}}")
                writer("  - \\newcommand{\\bminitwo}{\\begin{minipage}{0.25 \\textwidth}}")
                writer("  - \\newcommand{\\emini}{\\end{minipage}}")
            }
        }
        if (output=="html") writer("output: html_document")

    }
    writer("---")


    ## include packages as a first chunk
    secretChunk.wrapper("library(ggplot2)\nlibrary(pander)")

    ## Title
    writer("# Data cleaning summary")
    writer("The dataset examined has the following dimensions:")

    ## Print data frame summary
    sumMat <- matrix(c("Number of rows", "Number of variables",
                       n, nvariables), 2,
                     dimnames= list(NULL, c("Feature", "Result")))
    writer(pander::pander_return(sumMat, justify="lr"))


    ## if useVar options are chosen, they are printed accordingly
    if (!is.null(useVar)) {
        writer(paste("\n* Only the following variables in", dfname, "were cleaned:",
                     paste(vnames, collapse=", ")))
    }
    ## And the user is informed if we only show problematic variables
    if (onlyProblematic) {
        writer("\n* Only variables that were deemed potentially problematic are included in this summary.")
    }
    writer("\n")


   #browser()

   ## List the checking that were used for each possible variable type
   if (listChecks) {
    # if ("characterChecks" %in% names(dots)) cChecks <- dots$characterChecks
    # else cChecks <- eval(formals(check.character)$characterChecks)
    #
    # if ("factorChecks" %in% names(dots)) fChecks <- dots$factorChecks
    # else fChecks <- eval(formals(check.factor)$factorChecks)
    #
    # if ("labelledChecks" %in% names(dots)) lChecks <- dots$labelledChecks
    # else lChecks <- eval(formals(check.labelled)$labelledChecks)
    #
    # if ("numericChecks" %in% names(dots)) nChecks <- dots$numericChecks
    # else nChecks <- eval(formals(check.numeric)$numericChecks)
    #
    # if ("integerChecks" %in% names(dots)) iChecks <- dots$integerChecks
    # else iChecks <- eval(formals(check.integer)$integerChecks)
    #
    # if ("logicalChecks" %in% names(dots)) bChecks <- dots$logicalChecks
    # else bChecks <- eval(formals(check.logical)$logicalChecks)

     everyCheck <- union(characterChecks, c(factorChecks, labelledChecks, numericChecks,
                                           integerChecks, logicalChecks, dateChecks))
     checkMat <- matrix("", length(everyCheck), 7, #6: number of different variable types
                        dimnames=list(everyCheck, c("character", "factor", "labelled",
                                                   "numeric", "integer", "logical", "Date")))
     y <- ifelse(output == "pdf", "$\\times$", "&times;")
     checkMat[characterChecks, "character"] <- y
     checkMat[factorChecks, "factor"] <- y
     checkMat[labelledChecks, "labelled"] <- y
     checkMat[numericChecks, "numeric"] <- y
     checkMat[integerChecks, "integer"] <- y
     checkMat[logicalChecks, "logical"] <- y
     checkMat[dateChecks, "Date"] <- y

    rownames(checkMat) <- sapply(rownames(checkMat), function(x) description(get(x)))


    writer("### Checks performed")
    writer("The following variable checks were performed, depending on the data type of each variable:")
    writer(pander::pandoc.table.return(checkMat, justify="lccccccc",
                               emphasize.rownames=FALSE)) #allows for centering in this table only
    writer("\n")
    if (maxDecimals != Inf) {
      writer(paste("Please note that all numerical values in the following have been rounded to",
                   maxDecimals, "decimals."))
      writer("\n")
    }
 }


    ## This part is wrapped in a try call to ensure that the connection is closed even if something
    ## breaks down when running the code.
   try({

       ## allRes contains the summary table
       allRes <- data.frame(variable = vnames[index],
                            name = rep(NA, nvariables),
                            vClass = rep(NA, nvariables),
                            distinctVals = rep(NA, nvariables),
                            missingPct = rep(NA, nvariables),
                            problems = rep("", nvariables),
                            stringsAsFactors = FALSE)

    ## List of variables
    writer("# Variable list", outfile = vListConn)
    
   

    for (idx in index) {
        
        #Initialize variables
        extraMessages <- list(do=FALSE, messages=NULL)
        skip <- FALSE
        problems <- FALSE
        preCheckProblems <- FALSE

        ## Choose variable
        v <- data[[idx]]
        vnam <- vnames[idx]
        
      #  browser()

        ## Check if variable is key/empty
        preCheckRes <- lapply(preChecks, function(x) eval(call(x, v)))
        preCheckProblems <- sapply(preCheckRes, function(x) x$problem)
        preCheckMessages <- sapply(preCheckRes, function(x) x$message)
        
        ## Deal with non-supported classes whose handling is 
        ## specified in treatXasY
        userSuppVar <- FALSE
        if ("isSupported" %in% preChecks && 
            preCheckProblems[which(preChecks == "isSupported")] &&  
            !is.null(treatXasY)) {
          vClasses <- class(v)
          firstUSClass <- vClasses[vClasses %in% names(treatXasY)][1]
          if (!is.na(firstUSClass)) {
            attr(v, "orginalClass") <- vClasses[1]
            class(v) <- treatXasY[[firstUSClass]]
            preCheckProblems[which(preChecks == "isSupported")] <- FALSE
            preCheckMessages[which(preChecks == "isSupported")] <- ""
            userSuppVar <- TRUE
          }
        }

        ## Deal with labelled variables: If they don't have any labels and
        ## they inherit from a base class, treat them as that base class
        if (labelled_as == "factor" & !userSuppVar) {
          v <- doCheckLabs(v)
          if ("fakeLabelled" %in% class(v)) {
            extraMessages$do <- TRUE
            extraMessages$messages <- c(extraMessages$messages,
                                        paste("Note that this variable is treated as a",
                                        class(v)[2],
                                        "variable below, rather than a labelled variable,",
                                        "as it contains no label information."))
          }
        }

        ## use smartNum
        if (smartNum & !("fakeLabelled" %in% class(v)) & !userSuppVar & 
            any(class(v) %in% c("numeric", "integer"))) {
            v <- doSmartNum(v, ...)
            if ("smartNum" %in% class(v)) {
                extraMessages$do <- TRUE
                extraMessages$messages <- c(extraMessages$messages,
                                            "Note that this variable is treated as a factor variable below, as it only takes a few unique values.")
            }
        }



        ## Make checks
        if (doCheck && !any(preCheckProblems)) {
          #if (vnam == "numOutlierVar") browser()
          checkRes <- check(v, characterChecks = characterChecks,
                            factorChecks = factorChecks,
                            labelledChecks = labelledChecks,
                            numericChecks = numericChecks,
                            integerChecks = integerChecks,
                            logicalChecks = logicalChecks,
                            dateChecks = dateChecks,
                            nMax = maxProbVals,
                            maxDecimals = maxDecimals, ...)
          problems <- sapply(checkRes, function(x) x[[1]]) #maybe change to index by name?
        }

        #Update problem status in results overview
        if (any(c(problems, preCheckProblems)))  {
          allRes$problems[allRes$variable == vnam] <- y
        }

        ## skip non problem-causing variables
        if (onlyProblematic && (!any(preCheckProblems) && !any(problems))) skip <- TRUE

        ## Now print out the information if the variable isn't skipped
        if (!skip) {
             ## Variable name
            printable_name <- gsub("_", "\\\\_", vnam)
                  #writer("## **", printable_name, "**\n", outfile = vListConn)
            writer("## ", printable_name, "\n", outfile = vListConn) #** makes linking complicated

            #Fill out name, vClass and missingPct entries in the results overview
            allRes$name[allRes$variable == vnam] <- paste("[", printable_name, "]", sep = "")
            allRes$vClass[allRes$variable == vnam] <- oClass(v)[1]
            allRes$missingPct[allRes$variable == vnam] <- paste(format(round(100*mean(is.na(v)),2),
                                                                       nsmall = 2), "%")
            allRes$distinctVals[allRes$variable == vnam] <- length(unique(v))
            
            ## If the variable has label information the print that below
            if ("label" %in% attributes(v)$names)
                writer("*",attr(v, "label"), "*\n", outfile = vListConn)  # Write label

            ## write result of key/empty check
            if (any(preCheckProblems)) {
                writer(paste("* ", preCheckMessages[preCheckProblems], "\n", collapse=" \n ", sep=""),
                       outfile = vListConn)
            } else {

              ## write extra messages if any
              if (extraMessages$do) writer(paste("* ", extraMessages$messages, "\n", collapse=" \n ",
                                                 sep=""),
                                           outfile = vListConn)

              ## make Summary table
              if (doSummarize) sumTable <- pander::pander_return(summarize(v,
                                                                 reportstyleOutput = TRUE,
                                                                 characterSummaries = characterSummaries,
                                                                 factorSummaries = factorSummaries,
                                                                 labelledSummaries = labelledSummaries,
                                                                 numericSummaries = numericSummaries,
                                                                 integerSummaries = integerSummaries,
                                                                 logicalSummaries = logicalSummaries,
                                                                 dateSummaries = dateSummaries,
                                                                 maxDecimals = maxDecimals, ...),
                                                                 justify="lr")
              #if (doSummarize) sumTable <- pandoc.table.return(summarize(v, ...))
                                        #exactly the same result as with pander_return()

              ## Label information
              ## Right now we are not doing anything besides wirint the label above



              ## make Visualization
              if (doVisualize) visual <- visualize(v, vnam, doEval=FALSE, allVisuals = allVisuals, ...)

              ## Chunkname should avoid spaces and periods
              chunk_name <- paste0("Var-", idx, "-", gsub("[_:. ]", "-", vnam))

              ## add visualization + summary results to output file
              if (twoCol) {
                twoCols.wrapper(sumTable, visual, label=chunk_name, outfile = vListConn)
              } else {
                if (doSummarize) writer(sumTable, outfile = vListConn)
                if (doVisualize) fig.wrapper(visual, label=chunk_name, outfile = vListConn)
                writer("\n")
              }

              ## add check results to file
              if (doCheck) {
                if (any(problems)) {
                  #browser()
                  messages <- sapply(checkRes, function(x) x[[2]])[problems] #maybe index by name instead?
                  for (i in 1:length(messages)) {
                    writer(paste0("- ", messages[i], " \n"), outfile = vListConn)

                    ###Why did we use to have this line here? Do we need pander stuff ever?###
                    #writer(paste0("- ", pander::pander_return(messages[i])))
                    ##########################################################################
                  }
                }
              }
            }

            writer("\n", outfile = vListConn)
            if (output=="html") writer("---\n", outfile = vListConn)
            if (output=="pdf") writer("\\fullline\n", outfile = vListConn)

            ## Add garbage collection. Should help with memory problems.
            ## Removed for now
            ## if (garbageCollection) secretChunk.wrapper("gc(verbose=FALSE)")
        }

    }
   }) #end inner try (vListConn)

    #Close VarList file
    flush(vListConn)
    close(vListConn)

    #Add variable summary table
   if (addSummaryTable) {
      writer("# Summary table")

      #remove skipped variabled (e.g. due to onlyProblematic = TRUE) and
      #drop variable with original variable names (not formatted for printing)
      allRes <- na.omit(allRes)[, -1]
      rownames(allRes) <- 1:nrow(allRes) #note: necessary, as pander prints
                                         #non-trivial row names as a column
                                         #and data.frame subsetting creates
                                         #rownames like c(1, 2, 4, 5, 9)...

      #Add names used for printing
      names(allRes) <- c("", "Variable class", "# unique values", "Missing observations",
                         "Any problems?")

      writer(pander::pandoc.table.return(allRes, justify="llrrc"))
      writer("\n")
    }



    #Write variable list file into parent .Rmd file and delete the temporary file afterwards
    writer(scan(vListFileName, what = "character", sep = "\n",
                blank.lines.skip = FALSE, quiet = TRUE))
    unlink(vListFileName)


    ## This could be wrapped in a tryCatch for those rather weird situations where the package is not installed.
    ## But it is indeed rather obscure.

    ## Misc meta information
    writer("\n")
    
    writer("Report generation information:\n")
    writer(" *  Created by ", whoami::fullname() , ".\n")
    writer(" *  Report creation time: ", format(Sys.time(), "%a %b %d %Y %H:%M:%S"),"\n")

    ## Part of this was lifted from devtools

    getdate <- function (desc) {
      if (!is.null(desc$`Date/Publication`)) {
          date <- desc$`Date/Publication`
      }
      else if (!is.null(desc$Built)) {
          built <- strsplit(desc$Built, "; ")[[1]]
          date <- built[3]
      }
      else {
          date <- NA_character_
      }
      as.character(as.Date(strptime(date, "%Y-%m-%d")))
    }
    getpkgsource <- function(desc) {
    if (!is.null(desc$GithubSHA1)) {
        str <- paste0("Github (", desc$GithubUsername, "/", desc$GithubRepo, 
            "@", substr(desc$GithubSHA1, 1, 7), ")")
    }
    else if (!is.null(desc$RemoteType)) {
        remote_type <- desc$RemoteType
        if (!is.null(desc$RemoteUsername) && (!is.null(desc$RemoteRepo))) {
            user_repo <- paste0(desc$RemoteUsername, "/", desc$RemoteRepo)
        }
        else {
            user_repo <- NULL
        }
        if (!is.null(desc$RemoteSha)) {
            sha <- paste0("@", substr(desc$RemoteSha, 1, 7))
        }
        else {
            sha <- NULL
        }
        if (!is.null(user_repo) || !is.null(sha)) {
            user_repo_and_sha <- paste0(" (", user_repo, sha, 
                ")")
        }
        else {
            user_repo_and_sha <- NULL
        }
        str <- paste0(remote_type, user_repo_and_sha)
    }
    else if (!is.null(desc$Repository)) {
        repo <- desc$Repository
        if (!is.null(desc$Built)) {
            built <- strsplit(desc$Built, "; ")[[1]]
            ver <- sub("$R ", "", built[1])
            repo <- paste0(repo, " (", ver, ")")
        }
        repo
    }
    else if (!is.null(desc$biocViews)) {
        "Bioconductor"
    }
    else {
        "local"
    }
}    
        
    
    desc <- lapply("dataMaid", packageDescription, lib.loc = NULL)
    version <- vapply(desc, function(x) x$Version, character(1))
    pkgdate <- vapply(desc, getdate, character(1))
    pkgsource <- vapply(desc, getpkgsource, character(1))
    
    writer(" *  dataMaid v", version, " [Pkg: ", pkgdate, " from ", pkgsource, "]\n")
    sessioninfo <- sessionInfo()
    writer(" *  ", sessioninfo[[1]]$version.string, ".\n")
    writer(" *  Platform: ", sessioninfo[[2]], "(", sessioninfo[[4]], ").\n")
    writer(" *  Function call: `", capture.output(orig.call), "`\n")
    

    }) ## Now we should not write anything more to the file - End try.
       ## Maybe include the rest of the steps in the try? As of now, we render and open
       ## files with no contents if mistakes were found along the way...

    ## Force flush and close connection
    flush(fileConn)
    close(fileConn)


    #Make panderOptions as they were
    for(i in 1:length(changedPanderOptions)) {
      optName <- changedPanderOptions[i]
      panderOptions(optName, oldPanderOptions[[optName]])
    }



    if (output %in% c("html", "pdf") && render) {
      ##is it possible to close the file clean_data.pdf/html if it is open such
      ###that no access permission issues can occur?
      ###or maybe just check if it is open and then not try and render.
      #fileName <- paste(substring(fileName, 1, nchar(fileName)-4), ".",
      #                  output, sep="")
        if (!silent) {
            message("Data cleaning is finished. Please wait while your output file is being rendered.")
        }
        if (nagUser && output=="pdf" && identical(as.character(Sys.info()["sysname"]),"Windows")) {
            message(paste("\n Is", outFile,
                          "open on your computer? Please close it as fast as possible to avoid problems! \n"))
        }
        render(file, quiet = quiet)
    }

    ## if (output=="screen") {
    ##    unlink(file) #delete rmd
    ## }



    if (!quiet) { #whoops - version 1 only makes sense for windows, doesn't it?
                  #does version 2 work on mac/linux?
                  #also: problems if people supply their own file paths using the "file"-argument?
                  #print(paste("Data cleaning was succesful. Find your results in", ###version 1
                  #     paste(getwd(), "/", fileName, sep="")))
        message(paste("Data cleaning was succesful. Find your results in", ###version 2
                      #path.expand(paste("~/", outFile, sep=""))))  #doesn't work
                      paste(getwd(), "/", outFile, sep="")))
                      #to do: make into link so that the user can just click it and open the file.
                      #must be possible, debug() does interactive stuff..
                      #CHECK: Does this work on mac? linux?

                      #awkward if openResult==T? What should we write instead in that case?
                      #also feels awkward if no message is printed in that case (in case the user e.g.
                      #accidentially shuts down the pdf/html/rmd-file.)
    }

    if (openResult) pander::openFileInOS(outFile)     # system(paste("open", outFile))
}


#################################################################################################
##################################Not exported below#############################################
#################################################################################################


#Check if a numeric/integer variable has less than maxLevel unique
#values. If so, the variable is changed into a smartNum object.
#Note that smartNum inherits from the factor class, so if
#the user does not supply specific smartNum methods, they will
#match factor methods.
#Note: maxLevels is not an argument of clean(), but it can be passed
#through "...".
doSmartNum <- function(v, maxLevels = 5, ...) {
 if (length(unique(na.omit(v))) <= maxLevels) v <- smartNum(v)
 v
}


#Replaces characters that are not allowed in file names with "_".
normalizeFileName <- function(fileName, replaceChar = "_") {
  forbidChar <- "[^-_.[:alnum:]]" #note: "^" is "not"
                #Note: I have to allow blankspaces
                #if people want their file placed in a
                #folder with a blankspace name :(
  nName <- nchar(fileName)
  slashPlaces <- c(gregexpr("/", fileName)[[1]])
  if (!any(slashPlaces[[1]] == -1)) { #slash found
    lastSlash <- slashPlaces[length(slashPlaces)]
    justFile <- substr(fileName, lastSlash + 1, nName)
    filePath <- substr(fileName, 1, lastSlash)
    out <- paste(filePath, gsub(forbidChar, replaceChar, justFile), sep = "")
  } else {
    out <- gsub(forbidChar, replaceChar, fileName)
  }
  out
}


#Check if a labelled variable has any labels
#'@importFrom haven is.labelled
doCheckLabs <- function(v) {
 # browser()
  if (!is.labelled(v)) return(v)
  cV <- class(v)
  if (length(cV) > 1) {
    if (!is.null(attr(v, "labels"))) return(v)
    class(v) <- c("fakeLabelled", setdiff(class(v), "labelled"))
    attr(v, "originalClass") <- "labelled"
    return(v)
  }
  return(v)
}




