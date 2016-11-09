#' Perform a check of potential errors in a data frame
#'
#' Runs a set of validation checks to check the variables in a data frame for potential errors.
#' Performs checking steps according to user input and/or data type of the inputted variable.
#' The checks are saved to an R markdown file which can rendered into an easy-to-read document.
#'
#' @param data the dataset to be checked [LIST ALL ALLOWED OBJECT CLASSES, INCLUDING data.frame, 
#' matrix, data.table, ... (more?)].
#' @param output Output format. Options are "markdown" (the default), "pdf", "html", and "screen".
#' All but the "screen" option produces an R markdown file which can be rendered.
#' The "screen" option prints a small summary on the screen.
#' @param render Should the output file be rendered (defaults to TRUE)? This argument has no
#' impact unless the output is "html" or "pdf" in which case the R markdown file is rendered to
#' produce the corresponding file.
#' @param useVar Variables to clean. If NULL (the default) then all variables in the data.frame
#' o are included. If a vector of variable names is included then only the variables in o that are
#' also part of useVar are checked.
#' @param ordering Choose the ordering of the variables in the data presentation. The options
#' are "asIs" (ordering as in the dataset) and "alphabetical" (alphabetical order).
#' @param onlyProblematic A logical. Set to TRUE if only the potentially problematic variables should be listed.
#' @param mode Vector of tasks to perform among the three categories "summarize", "visualize" and "check".
#' Note that... SOMETHING ABOUT HOW THE FUNCTIONS CALLED IN EACH PART ARE CONTROLLED.
#' @param characterChecks a list of error-checking functions to apply to character vectors
#' @param factorChecks a list of error-checking functions to apply to integer vectors
#' @param labelledChecks a list of error-checking functions to apply to character vectors
#' @param integerChecks a list of error-checking functions to apply to integer vectors
#' @param numericChecks a list of error-checking functions to apply to integer vectors
#' @param logicalChecks a list of error-checking functions to apply to integer vectors
#' @param allChecks Vector of function names that should be used as check-functions for all variable types.
#' See ???? for more details OR SOMETHING? Note that this option overwrites the options characterChekcs,
#' factorChecks, etc.
#' @param smartNum If TRUE, numeric and integer variables with less than maxLevels (defaults to 5) unique
#' values are treated as factor variables in the checking, visualization and summary functions. A
#' message is printed in the data summary as well.
#' @param preChecks Variable checks that are performed before the summary/visualization/checking step. If
#' any of these checks find problems, the variable will not be summarized nor visualized nor checked.
#' @param file The filename of output file. If set to NULL (the default) then the filename will be
#' the name of the data frame prefixed with "cleanR_". Note that a valid file is of type rmd, hence all
#' filenames should have a ".Rmd"-suffix.
#' @param replace If FALSE (the default) an error is thrown if one of the files that we are about to write to
#' already exists. If TRUE no checks are performed.
#' @param vol Extra text string that is appended on the end of the output file name(s). For example, if the data
#' set is called "myData", no file argument is supplied and vol="2", the output file will be called
#' "cleanR_myData2.Rmd"
#' @param characterSummaries STUFF
#' @param factorSummaries STUFF
#' @param labelledSummaries STUFF
#' @param numericSummaries STUFF
#' @param integerSummaries STUFF
#' @param logicalSummaries STUFF
#' @param allSummaries Vector of function names that should be used as summary-functions for all variable types.
#' See ???? for more details OR SOMETHING? Note that this option overwrites the options charachterSummaries,
#' factorSummaries, etc.
#' @param allVisuals STUFF. Default: "standardVisual".
#' @param standAlone If TRUE, the document begins with a markdown preamble such that it
#' can be rendered as a stand alone R markdown file.
#' @param twoCol Should the results be presented in two columns (if output is "html" or "pdf")? Defaults to TRUE.
#' @param quiet If \code{TRUE} (the default), only a few messages is printed to the screen as \code{clean} runs.
#' If \code{FALSE} no messages are suppressed. The third option, \code{silent}, renders the function
#' completely silent such that only fatal errors are printed.
#' @param openResult If TRUE, the file produced by clean() is automatically opened by the end of
#' the function run.
#' @param nagUser Remove at some point
#' @param checkDetails MAYBE ALSO IMPLEMENT THIS?: If TRUE, details about each check function are added
#' to the document (if available)
#' @param garbageCollection A logical. If TRUE (the default) then garbage collection code is added to
#' the R markdown file that is output. This is useful for larger dataset to prevent memory problems.
#' @param maxProbVals Maximum number of unique values printed from check-functions (integer > 0). 
#' Defaults to \code{Inf}, which means that all values are printed.
#' @param \dots other arguments that are passed on the to precheck, checking, summary and visualization functions
#' @return The function does not return anything. It's side effect (the production of the Rmd file summary)
#' is the reason for running the function.
#' @author Anne H. Petersen \email{ahpe@@sund.ku.dk} and Claus Thorn Ekstrom \email{ekstrom@@sund.ku.dk}
#' @seealso \code{\link{clean}}
#' @keywords misc
#' @examples
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
#'#Add user defined check-function to the checks performed on character variables:
#' \dontrun{
#' characterFoo <- function(v) {
#'  if (substr(substitute(v), 1, 1) == "_") {
#'    out <- list(problem=TRUE, message="Note that the variable name begins with \\_")
#'  } else out <- list(problem=FALSE, message="")
#'  out
#' }
#' class(characterFoo) <- "checkFunction"
#' attr(characterFoo, "description") <- "I really hate underscores"
#' clean(testData, characterChecks=c(defaultCharacterChecks(), "characterFoo"))
#' }
#'
#' @importFrom methods is
#' @importFrom pander pander_return panderOptions pandoc.table.return
#' @importFrom tools file_ext 
#' @export
clean <- function(data,
                  output=c("pdf", "html", "screen"), render=TRUE,
                      #note: output cannot just be markdown. Either it's pdf-markdown or html-markdown.
                      #what files are produced is controlled using render.
                  useVar=NULL, ordering=c("asIs", "alphabetical"), onlyProblematic=FALSE,
                  mode=c("summarize", "visualize", "check"),
                  smartNum=TRUE, preChecks=c("isKey", "isEmpty"),
                  file=NULL, replace=FALSE, vol="",
                  standAlone=TRUE, twoCol=TRUE,
                  quiet = TRUE,
                  openResult=TRUE,
                  nagUser=TRUE,
                  checkDetails=FALSE,
                  characterChecks = defaultCharacterChecks(),
                  factorChecks = defaultFactorChecks(),
                  labelledChecks = defaultLabelledChecks(),
                  numericChecks = defaultNumericChecks(),
                  integerChecks = defaultIntegerChecks(),
                  logicalChecks = defaultLogicalChecks(),
                  allChecks = NULL,
                  characterSummaries = defaultCharacterSummaries(),
                  factorSummaries = defaultFactorSummaries(),
                  labelledSummaries = defaultLabelledSummaries(),
                  numericSummaries = defaultNumericSummaries(),
                  integerSummaries = defaultIntegerSummaries(),
                  logicalSummaries = defaultLogicalSummaries(),
                  allSummaries = NULL,
                  allVisuals = "standardVisual",
                  garbageCollection=TRUE,
                  listChecks = TRUE,
                  brag=FALSE, #remove me
                  maxProbVals = Inf,
                  ...) {

    ## Start by doing a few sanity checks of the input
    if (! (is(data, "data.frame") )) {
        ## tibble is automatically a data frame
        if (is.matrix(data)) {
            data <- as.data.frame(data)
        } else stop("clean requires a data.frame, tibble or matrix as input")
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
      #quiet <- match.arg(quiet)

    ## Extract the dataframe name
    dfname <- deparse(substitute(data))

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
        file <- paste0("cleanR_unnamedData", vol, ".Rmd")
      } else file <- normalizeFileName(paste0("cleanR_", dfname, vol, ".Rmd"))
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

    ## The name of the R markdown file that is output
    #outFile <- paste0(substring(file, 1, nchar(file)-4), ".Rmd")


################################################################################################
###ALSO: check that vol and file and dataname produces a valid file name (no strange characters)
################################################################################################

    fileExists <- file.exists(file)
    outFileExists <- file.exists(outFile)


    ## check if we are about to overwrite a file
      #if (!replace %in% c("never", "onlyCleanR") && (fileExists || outFileExists)) {
    if (replace) {
        unlink(file)
    } else if (fileExists | outFileExists) {
     # if (replace=="never") {
        if (fileExists & outFileExists) problemFiles <- paste(file, "and", outFile)
        if (fileExists & !outFileExists) problemFiles <- file
        if (!fileExists & outFileExists) problemFiles <- outFile
        stop(paste("The file name(s) to be used by cleanR,", paste(problemFiles, ",", sep=""),
                   "is(are) already in use.",
                   "We recommend trying one of the following solutions: \n",
                   "- rename your cleanR output file using the \"file\" option \n",
                   "- Add a volume number to your file name using the \"vol\" option \n",
                   "- check that you do not want to keep the original file and if so,",
                   "use cleanR with replace = TRUE"))
    }

      #if (replace=="onlyCleanR") {
      #  fileProblem <- F
      #  outputFileProblem <- F
      #
      #  if (fileExists) {
      #    l12 <- readLines(file, 2, warn=FALSE)
      #    if (!identical(l12, c("---", "cleanR: yes"))) fileProblem <- T
      #  }
      #  if (outFileExists) {
      #    #############################################
      #    #check if pdf/html was produced by cleanR....
      #    #############################################
      #  }
      #
      #  if (fileProblem & outFileProblem) problemFiles <- paste(file, "and", outFile)
      #  if (fileProblem & !outFileProblem) problemFiles <- file
      #  if (!fileproblem & outFileProblem) problemFiles <- outFile
      #
      #  if (fileProblem || outFileProblem) {
      #    stop(paste("The file name(s) to be used by cleanR,", paste(problemFiles, ",", sep=""),
      #               "are already in use and the files do not look like they were produced by cleanR.",
      #               "We recommend trying one of the following solutions: \n",
      #               "- rename your cleanR output files using the \"file\" option \n",
      #               "- Add a volume number to your file name using the \"vol\" option \n",
      #              "- check that you do not want to keep the original file and if so,",
      #               "use cleanR with replace = \"always\""))
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
    oldPanderOptions <- panderOptions() # Used to restore towards the end
    ## panderOptions("table.alignment.default", "left")
    panderOptions('table.alignment.default', 'center')  ## XXX CE only one of these two
    panderOptions("table.split.table", Inf)
    panderOptions("table.split.cells", Inf)
    panderOptions('table.alignment.rownames', 'left')

    ##
    ## Below comes a bunch of helper functions for writing the output
    ##
    writer <- function(x, ..., outfile=file, sep="\n") {
        cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
    }

    chunk.wrapper <- function(x, ..., outfile=file, options=c("echo=FALSE", "warning=FALSE")) {
        writer(paste("```{r", paste(options, collapse=", "), "}"))
        writer(x, ..., outfile=outfile)
        writer("```")
    }

    fig.wrapper <- function(x, ..., outfile=file, options=c("echo=FALSE", "fig.width=4",
                                                      "fig.height=3", "message=FALSE",
                                                      "warning=FALSE")) {
        chunk.wrapper(x, outfile=outfile, options=options)
    }

    secretChunk.wrapper <- function(x, ..., outfile=file, options=c("echo=FALSE", "include=FALSE",
                                                              "warning=FALSE", "message=FALSE",
                                                              "error=FALSE")) {
        chunk.wrapper(x, outfile=outfile, options=options)
    }

    ## outputty sets the output type
    twoCols.wrapper <- function(text, figure, outfile=file, outputty=output) {
        if (outputty=="pdf") { #note: does NOT work if there is a linebreak between the two
                                        #minipage environments!
            writer("\\bminione")
            writer(text)
            writer("\\emini")
            writer("\\bminitwo")
            fig.wrapper(figure)
            writer("\\emini")
        }
        if (outputty=="html") {
            writer("<div class = \"row\">")
            writer("<div class = \"col-lg-8\">")
            writer(text)
            writer("</div>")
            writer("<div class = \"col-lg-4\">")
            fig.wrapper(figure)
        writer("</div>")
            writer("</div>")
        }
        writer("\n")
    }




    ## write YAML preamble
    writer("---")
    writer("cleanR: yes")
    if (standAlone & !(identical(output, "screen"))) {
        writer(paste("title:", dfname))
        writer("subtitle: \"Autogenerated data summary from cleanR\"")
        writer("date: \"`r Sys.Date()`\"")
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
    secretChunk.wrapper("library(ggplot2)\nlibrary(stringi)\nlibrary(pander)")

    ## Title
    writer("# Data cleaning summary")
    writer("The dataset examined has the following dimensions:")

    ## Print data frame summary
    sumMat <- matrix(c("Number of rows", "Number of variables",
                       n, nvariables), 2,
                     dimnames= list(NULL, c("Feature", "Result")))
    writer(pander_return(sumMat, justify="lr"))


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
                                           integerChecks, logicalChecks))
     checkMat <- matrix("", length(everyCheck), 6, #6: number of different variable types
                        dimnames=list(everyCheck, c("character", "factor", "labelled",
                                                   "numeric", "integer", "logical")))
     y <- "$\\times$"
     checkMat[characterChecks, "character"] <- y
     checkMat[factorChecks, "factor"] <- y
     checkMat[labelledChecks, "labelled"] <- y
     checkMat[numericChecks, "numeric"] <- y
     checkMat[integerChecks, "integer"] <- y
     checkMat[logicalChecks, "logical"] <- y

    rownames(checkMat) <- sapply(rownames(checkMat), function(x) description(get(x)))


    writer("### Checks performed")
    writer("The following variable checks were performed, depending on the data type of each variable:")
    writer(pandoc.table.return(checkMat, justify="lcccccc",
                               emphasize.rownames=FALSE)) #allows for centering in this table only
    writer("\n")
   }


    ## List of variables
    writer("# Variable list")

    for (idx in index) {
        extraMessages <- list(do=FALSE, messages=NULL)
        skip <- FALSE
        problems <- FALSE

        ## How to order the variables
        v <- data[[idx]]
        vnam <- vnames[idx]

        ## Check if variable is key/empty
        preCheckRes <- lapply(preChecks, function(x) eval(call(x, v)))
        preCheckProblems <- sapply(preCheckRes, function(x) x$problem)
        preCheckMessages <- sapply(preCheckRes, function(x) x$message)

        ## use smartNum
        if (smartNum & any(class(v) %in% c("numeric", "integer"))) {
            v <- doSmartNum(v, ...)
            if ("smartNum" %in% class(v)) {
                extraMessages$do <- TRUE
                extraMessages$messages <- c(extraMessages$messages,
                                            "Note that this variable is treated as a factor variable below, as it only takes a few unique values.")
            }
        }

        ## Make checks
        if (doCheck && !any(preCheckProblems)) {
          checkRes <- check(v, characterChecks = characterChecks,
                            factorChecks = factorChecks,
                            labelledChecks = labelledChecks,
                            numericChecks = numericChecks,
                            integerChecks = integerChecks,
                            logicalChecks = logicalChecks, nMax = maxProbVals, ...)
          problems <- sapply(checkRes, function(x) x[[1]])
        }

        ## skip non problem-causing variables
        if (onlyProblematic && (!any(preCheckProblems) && !any(problems))) skip <- TRUE


        ## Now print out the information if the variable isn't skipped
        if (!skip) {

            ## Variable name
            writer("## **", gsub("_", "\\\\_", vnam), "**\n")

            ## If the variable has label information the print that below
            if ("label" %in% attributes(v)$names)
                writer("*",attr(v, "label"), "*\n")  # Write label

            ## write result of key/empty check
            if (any(preCheckProblems)) {
                writer(paste("* ", preCheckMessages[preCheckProblems], "\n", collapse=" \n ", sep=""))
            } else {

                ## write extra messages if any
                if (extraMessages$do) writer(paste("* ", extraMessages$messages, "\n", collapse=" \n ",
                                                   sep=""))



                ## make Summary table
                if (doSummarize) sumTable <- pander_return(summarize(v, characterSummaries = characterSummaries,
                                                                 factorSummaries = factorSummaries,
                                                                 labelledSummaries = labelledSummaries,
                                                                 numericSummaries = numericSummaries,
                                                                 integerSummaries = integerSummaries,
                                                                 logicalSummaries = logicalSummaries,
                                                                 ...), justify="lr")
              #if (doSummarize) sumTable <- pandoc.table.return(summarize(v, ...))
                                        #exactly the same result as with pander_return()

                ## Label information
                ## Right now we are not doing anything besides wirint the label above



            ## make Visualization
            if (doVisualize) visual <- visualize(v, vnam, doEval=FALSE, allVisuals = allVisuals, ...)

                ## add visualization + summary results to output file
                if (twoCol) {
                    twoCols.wrapper(sumTable, visual)
                } else {
                    if (doSummarize) writer(sumTable)
                    if (doVisualize) fig.wrapper(visual)
                    writer("\n")
                }

                ## add check results to file
                if (doCheck) {
                    if (any(problems)) {
                        messages <- sapply(checkRes, function(x) x[[2]])[problems]
                        for (i in 1:length(messages)) {
                            writer(paste0("- ", pander_return(messages[i])))
                        }
                    }
                }
            }

            writer("\n")
            if (output=="html") writer("---\n")
            if (output=="pdf") writer("\\fullline\n")

            ## Add garbage collection. Should help with memory problems.
            if (garbageCollection) secretChunk.wrapper("gc(verbose=FALSE)")
        }

    }

    ## This could be wrapped in a tryCatch for those rather weird situations where the package is not installed.
    ## But it is indeer rather obscure
    if (brag) {
    writer("This report was created by cleanR v", paste(packageVersion("cleanR"), sep="."), ".")
    }
    ## Now we should not write anything more to the file

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

    if (output=="screen") {
        unlink(file) #delete rmd
    }

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

    if (openResult) system(paste("open", outFile)) 
}


#################################################################################################
##################################Not exported below#############################################
#################################################################################################


#Check if a numeric/integer variable has less than maxLevel unique
#values. If so, the variable is changed into a smartNum object.
#Note that smartNum inherits from the factor class, so if
#the user does not supply specific smartNum methods, they will
#match factor methods.
doSmartNum <- function(v, maxLevels = 5, ...) {
  #check if v is numeric/integer here? now we check it before the function is called
  v <- na.omit(v)
  if (length(unique(v)) <= maxLevels) v <- smartNum(v)
  v
}


#Replaces characters that are not allowed in file names with "_".
normalizeFileName <- function(fileName, replaceChar = "_") {
  forbidChar <- "[^-_.[:alnum:]]" #note: "^" is "not"
                #Note: I'm not allowing blankspaces atm
  gsub(forbidChar, replaceChar, fileName)
}




################DELETE AT SOME POINT##################################
#Extract a function summary/description from checkFunction objects and
#return the function name for other function types.
#NOTE: fName is a string containing the function name and therefore,
#this function cannot be implemented as a generic function with
#methods.
#funSum <- function(fName) {
#  foo <- get(fName)
#  if ("checkFunction" %in% class(foo)) {
#    out <- attr(foo, "description")
#  } else out <- fName
#  out
#}
#####################################################################






