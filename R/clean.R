#' Perform a check of potential errors in a data frame
#'
#' Runs a set of validation checks to check the variables in a data frame for potential errors.
#' Performs checking steps according to user input and/or data type of the inputted variable.
#'
#' @param standAlone If TRUE, the document begins with a markdown preamble such that it
#' can be rendered as is.
#' @param brag If TRUE, a note about cleanR is appended at the end of the output document.
#' @param ordering Choose the ordering of the variables in the data presentation. The options
#' are "asIs" (ordering as in the dataset) and "alpha" (alphabetical order).
#' @param cleanUp - not done yet -.
#' @param output Output format, options are pdf and html
#' @param finish "render" (makes pdf/html), "markdown" (makes markdown file), "print" (prints to screen).
#' @param twoCol Should the results be presented in two columns (if finish is "render" or "markdown")?
#' @param o the data frame or tibble(???) object to be checked
#' @param characterChecks a list of error-checking functions to apply to character vectors
#' @param integerChecks a list of error-checking functions to apply to integer vectors
#' @param silent Should clean() run completely silently? Note that this option overrules the settings for
#' "quiet": A silent session is always quiet.
#' @param openResult If TRUE, the file produced by clean() is automatically opened by the end of
#' the function run.
#' @param mode Vector of tasks to perform among the three categories "summarize", "visualize" and "check".
#' Note that... SOMETHING ABOUT HOW THE FUNCTIONS CALLED IN EACH PART ARE CONTROLLED.
#' @param useVar Variables to clean and present results for. Either a list of variable names (as used in
#' the data.frame o) or one of two special options; "all" (every variable is cleaned and presented) or
#' "problematic" (only variables yielding problems from the checking function are presented)
#' @param nagUser Remove at some point
#' @param smartNum If TRUE, numeric and integer variables with less than maxLevels (defaults to 5) unique
#' values are treated as factor variables in the checking, visualization and summary functions. A
#' message is printed in the data summary as well.
#' @param preChecks Variable checks that are performed before the summary/visualization/checking step. If
#' any of these checks find problems, the variable will not be summarized nor visualized nor checked.
#' @param stopOverwrite If TRUE, an error is returned if cleanR is about to overwrite a file that was
#' not produced by cleanR
#' @param listChecks If TRUE, the document contains an overview of what checks were performed for 
#' each variable data type.
#' @param checkDetails MAYBE ALSO IMPLEMENT THIS?: If TRUE, details about each check function are added to the document (if available)
#' @param \dots other arguments that are passed on the to checking, summary and visualization functions
#' @return ???
#' \itemize{
#'   \item{"name"}{The name of the check}
#'   \item{"description"}{Slightly more information about the check}
#'   \item{"problem"}{An integer giving an error code. 0 means no potential errors were identified}
#'   \item{"message"}{A string giving summary information in R markdown format about the results}
#' }
#' @author Anne H. Petersen \email{ahpe@@sund.ku.dk} and Claus Thorn Ekstrom \email{ekstrom@@sund.ku.dk}
#' @seealso \code{\link{clean}}
#' @keywords misc
#' @examples
#'
#' clean(data.frame(x=1:15))
#'
#' \dontrun{
#' data(testData)
#' clean(testData)
#' }
#' 
#' \dontrun{
#' characterFoo <- function(v) {
#'  if (substr(substitute(a), 1, 1) == "_") {
#'    out <- list(problem=TRUE, message="Note that the variable name begins with \\_")
#'  } else out <- list(problem=FALSE, message="")
#'  out
#' }
#' class(characterFoo) <- "checkFunction"
#' attr(characterFoo, "description") <- "I really hate underscores"
#' clean(testData, characterChecks="characterFoo")
#' }
#'
#' @export
clean <- function(o, file=NULL, removeExisting=TRUE, maxnum=NULL,
                  standAlone=TRUE, brag=FALSE, ordering=c("asIs", "alpha"),
                  cleanUp="deletethisoption?",
                  quiet=TRUE, output="pdf", finish = "markdown",
                  twoCol=TRUE, silent=FALSE, openResult=TRUE,
                  mode=c("summarize", "visualize", "check"),
                  useVar="all", nagUser=TRUE,
                  smartNum=TRUE, preChecks=c("isSpecial", "isCPR"),
                  stopOverwrite=TRUE, listChecks=TRUE,
                  checkDetails=FALSE, ...) {

    ## Start by doing a few sanity checks
    if (! (is(o, "data.frame") )) {
        ## tibble is automatically a data frame
        stop("clean requires a data.frame or tibble as input")
    }

    ##
    ordering <- match.arg(ordering)

  ## dataframe name
    dfname <- deparse(substitute(o))

  ## What variables should be used?
    if (!identical(useVar, "all") & !identical(useVar, "problematic")) {
      o <- o[, useVar, drop=FALSE]  #warning here if this doesn't work + overwrite stuff?
      useVar <- "subset"
    }

  ## Background variables
    nvariables <- ncol(o)
    if (ordering == "alpha") {
      index <- order(names(o))
    } else index <- 1:nvariables
    n <- nrow(o)
    vnames <- names(o)
    dots <- list(...) 


  ## check function input and initial settings
    if (!is.data.frame(o)) {
      if (is.matrix(o)) {
        o <- as.data.frame(o)
        warning("Data was converted into a data.frame object")
      } else stop("Data is of the wrong type, use data.frame or matrix data")
    }

    if (is.null(file) || substr(file, nchar(file)-3, nchar(file)) != ".Rmd") {
        #maybe try fixing the user's faulty file name instead of just overwriting it?
      file <- paste("cleanR_", dfname, ".Rmd", sep="")
    }
    fileName <- file #for printing purposes

  ## check if we are about to overwrite a file that was not generated by cleanR
    if (stopOverwrite && file.exists(file)) {
      l12 <- readLines(file, 2, warn=FALSE)
      if (!identical(l12, c("---", "cleanR: yes"))) {
        stop(paste("The file name to be used by cleanR,", paste(file, ",", sep=""),
                   "is already in use and the file does not look like it was produced by cleanR.",
                   "We recommend trying one of the following solutions: \n",
                   "- rename your cleanR output file using the \"file\" option \n",
                   "- check that you do not want to keep the original file and if so, use cleanR with stopOverwrite = FALSE"))
      }
    }

    if (silent) {
      quiet <- TRUE
      nagUser <- FALSE
    }

    doCheck <- "check" %in% mode
    doVisualize <- "visualize" %in% mode
    doSummarize <- "summarize" %in% mode

    if (!doCheck & !doVisualize & !doSummarize) {
      warning("Note that no proper arguments were supplied to \"mode\" - no cleaning is performed")
    } #rewrite warning message

    if (!doVisualize || !doSummarize) twoCol <- FALSE



  ## make tables left-aligned and allow for 6 columns
    #change back to original settings in the end of the function?
    panderOptions("table.alignment.default", "left")
    panderOptions("table.split.table", Inf)
    panderOptions("table.alignment.rownames", "left") #why doesn't this work?!

  ## write to file
    writer <- function(x, ..., outfile=file, sep="\n") {
      cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
    }


    chunk.wrapper <- function(x, ..., outfile=file, options=c("echo=FALSE", "warning=FALSE")) {
      writer(paste("```{r", paste(options, collapse=", "), "}"))
      writer(x, ..., outfile=outfile)
      writer("```")
    }

    fig.wrapper <- function(x, ..., outfile=file, options=c("echo=F", "fig.width=4",
                                                            "fig.height=3", "message=F",
                                                            "warning=F")) {
      chunk.wrapper(x, outfile=outfile, options=options)
    }

    secretChunk.wrapper <- function(x, ..., outfile=file, options=c("echo=F", "include=F",
                                                                    "warning=F", "message=F",
                                                                    "error=F")) {
      chunk.wrapper(x, outfile=outfile, options=options)
    }

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

    if (removeExisting) unlink(file)

  ## write YAML preamble
    writer("---")
    writer("cleanR: yes")
    if (standAlone & !(finish=="print")) {
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

  ## include packages
    secretChunk.wrapper("library(ggplot2)\n library(stringi)\n library(pander)")

    ## Title
    writer("# Data cleaning summary")

    ## Summary
    sumMat <- matrix(c("Number of rows", "Number of variables",
                       n, nvariables), 2,
                     dimnames= list(NULL, c("Feature", "Result")))
    writer(pander_return(sumMat))

    ## if useVar options are chosen, they are printed accordingly
    if (useVar=="subset") {
      writer("\n")
      writer(paste("* Only the following variables in", dfname, "were cleaned:",
                   paste(vnames, collapse=", ")))
    } else if (useVar=="problematic") {
      writer("\n")
      writer("* Only variables that were deemed potentially problematic are included in this summary")
    }

   writer("\n")

   #browser() 
   
   ## List the checking that were used for each possible variable type
   if (listChecks) {
     if ("characterChecks" %in% names(dots)) cChecks <- dots$characterChecks
     else cChecks <- eval(formals(check.character)$characterChecks)
     
     if ("factorChecks" %in% names(dots)) fChecks <- dots$factorChecks
     else fChecks <- eval(formals(check.factor)$factorChecks)
     
     if ("labelledChecks" %in% names(dots)) lChecks <- dots$labelledChecks
     else lChecks <- eval(formals(check.labelled)$labelledChecks)
     
     if ("numericChecks" %in% names(dots)) nChecks <- dots$numericChecks
     else nChecks <- eval(formals(check.numeric)$numericChecks)
     
     if ("integerChecks" %in% names(dots)) iChecks <- dots$integerChecks
     else iChecks <- eval(formals(check.integer)$integerChecks)
     
     if ("logicalChecks" %in% names(dots)) bChecks <- dots$logicalChecks
     else bChecks <- eval(formals(check.logical)$logicalChecks)
     
     allChecks <- union(cChecks, c(fChecks, lChecks, nChecks, iChecks, bChecks))
     checkMat <- matrix("", length(allChecks), 6, #6: number of different variable types
                        dimnames=list(allChecks, c("character", "factor", "labelled", 
                                                   "numeric", "integer", "logical")))
     y <- "$\\times$"
     checkMat[cChecks, "character"] <- y
     checkMat[fChecks, "factor"] <- y
     checkMat[lChecks, "labelled"] <- y
     checkMat[nChecks, "numeric"] <- y
     checkMat[iChecks, "integer"] <- y
     
     rownames(checkMat) <- sapply(rownames(checkMat), funSum)
     
    ##works
     #writer(pander_return(checkMat))
     #writer("\n")
    ##
     
     
    ##Doesn't change anything
     #checkMat <- as.data.frame(checkMat)
    ## 
     
     writer("[Write some meta text here?]. The following variable checks were performed, 
            depending on the data type of each variable:")
     writer(pandoc.table.return(checkMat, justify="centre",
                                emphasize.rownames=FALSE,
                                table.alignment.rownames = "left")) #allows for centering in this table only
     writer("\n")
    
    ##doesn't work
     #library(xtable) 
     #writer(xtable(checkMat))
     #writer("\n")
    ##
     
    ##works 
      #chunk.wrapper(paste("pander(", paste(deparse(checkMat), collapse=" "), ")", sep=""))
      #writer("\n")
    ##
     
    ##doesn't work - printed in verbatim 
      #chunk.wrapper("panderOptions(\"table.alignment.rownames\", \"left\");", paste("pandoc.table(", paste(deparse(checkMat), collapse=" "), ", justify=\"centre\"",
      #                  ", emphasize.rownames=FALSE)", sep=""), options="results=\"markup\"") 
    ##
     
   }
   
    ## List of variables
    writer("# Variable list")

    for (idx in index) {
        extraMessages <- list(do=FALSE, messages=NULL)
        skip <- FALSE
        problems <- FALSE

        v <- o[[idx]]
        vnam <- vnames[idx]

        ## Check if variable is key/empty
        preCheckRes <- lapply(preChecks, function(x) eval(call(x, v)))
        preCheckProblems <- sapply(preCheckRes, function(x) x$problem)
        preCheckMessages <- sapply(preCheckRes, function(x) x$message)

        ## use smartNum
        if (smartNum & class(v) %in% c("numeric", "integer")) {
          v <- doSmartNum(v, ...)
          if ("smartNum" %in% class(v)) {
            extraMessages$do <- TRUE
            extraMessages$messages <- c(extraMessages$messages,
                                        "Note that this variable is treated as a factor variable below, as it only takes a few unique values.")
              #more concrete message here?
          }
        }

        ## Make checks
        if (doCheck && !any(preCheckProblems)) {
          checkRes <- check(v, ...)
          problems <- sapply(checkRes, function(x) x[[1]])
        }

      ## skip non problem-causing variables
        if (useVar=="problematic" && (!any(preCheckProblems) && !any(problems))) skip <- TRUE


        ###remove
        #if (specialStatus$problem) browser()
        ###

        if (!skip) {

          ## Variable name
          writer("## **", #ifelse(substring(vnam, 1, 1) == "_",
                          #      paste("\\", vnam, sep=""),
                          #      vnam),
                 gsub("_", "\\\\_", vnam),
                "**\n")

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
            if (doSummarize) sumTable <- pander_return(summarize(v, ...))
              #if (doSummarize) sumTable <- pandoc.table.return(summarize(v, ...))
                #exactly the same result as with pander_return()
            
            
            ## Label information
              #???

            ## make Visualization
            if (doVisualize) visual <- visualize(v, vnam, doEval=F, ...)

            ## add visualization + summary results to file
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
                  writer(paste("- ", pander_return(messages[i]), sep=""))
                }
              }
            }
          }

          writer("\n")
          if (output=="html") writer("---\n")
          if (output=="pdf") writer("\\fullline \n")

          #collect garbage, should maybe help with memory?
          if (cleanUp=="always") {
             secretChunk.wrapper("gc(verbose=F)")
          }
        }
    }

#    close(ff)
    if (brag) {
        ## This could be wrapped in a tryCatch for those rather weird situations where the package is not installed.
        writer("This report was created by cleanR v", paste(packageVersion("cleanR"), sep="."), ".")
    }

    if(finish=="render") {
          ####is it possible to close the file clean_data.pdf/html if it is open such
          ####that no access permission issues can occur?
          ####or maybe just check if it is open and then not try and render.
      fileName <- paste(substring(fileName, 1, nchar(fileName)-4), ".",
                        output, sep="")
      if (!silent) {
        message("Data cleaning is finished. Please wait while your output file is rendered.")
      }
      if (nagUser && output=="pdf" && identical(as.character(Sys.info()["sysname"]),"Windows")) {
        message(paste("\n Is", fileName,
                      "open on your computer? Please close it as fast as possible to avoid problems! \n"))
      }
      render(file, quiet=quiet)
    }

    if (finish=="print") {
      unlink(file) #delete rmd
    }

    if (!silent) { #whoops - version 1 only makes sense for windows, doesn't it?
                    #does version 2 work on mac/linux?
                   #also: problems if people supply their own file paths using the "file"-argument?
      #print(paste("Data cleaning was succesful. Find your results in", ###version 1
       #     paste(getwd(), "/", fileName, sep="")))
      message(paste("Data cleaning was succesful. Find your results in", ###version 2
            path.expand(paste("~/", fileName, sep=""))))
          #to do: make into link so that the user can just click it and open the file.
          #must be possible, debug() does interactive stuff..
          #CHECK: Does this work on mac? linux?

      #awkward if openResult==T? What should we write instead in that case?
      #also feels awkward if no message is printed in that case (in case the user e.g.
      #accidentally shuts down the pdf/html/rmd-file.)
    }

    if (openResult) system(paste("open", fileName))
}


#MOVE THESE FUNCTIONS ELSEWHERE

#Check if a variable is a key (all observations have unique values) or
#"empty" (only one unique value)
#Maybe deal with missing values? They count as distinct values right now...
isSpecial <- function(v) {
  out <- list(problem=F, status="", message="")
  nVals <- length(unique(v))
  if (nVals == 1) {
    out <- list(problem=T, status="empty",
                message=paste("The variable only takes one value: \"", v[1],
                                         "\".", sep=""))
  } else if (nVals == length(v) & !(class(v) %in% c("numeric", "integer"))) {
    out <- list(problem=T, status="key",
                message="The variable is a key (distinct values for each observation).")
  }
  out
}


#Check if a numeric/integer variable has less than maxLevel unique
#values. If so, the variable is changed into a smartNum object.
#Note that smartNum inherits from the factor class, so if
#the user does not supply specific smartNum methods, they will
#match factor methods.
doSmartNum <- function(v, maxLevels = 5, ...) {
  #check if v is numeric/integer here? now we check it before the function is called

  if (length(unique(v)) <= maxLevels) {
    oClass <- class(v)
    v <- factor(v)
    attr(v, "originalClass") <- oClass
    class(v) <- c("smartNum", "factor")
  }
  v
}


oClass <- function(v) UseMethod("oClass")
oClass.default <- function(v) class(v)
oClass.smartNum <- function(v) attr(v, "originalClass")


#make pdf/html from a .Rmd file
render <- function(file, quiet) rmarkdown::render(file, quiet=quiet)



#Check if v contains only (except for NAs) values that look like Danish
#civil registration numbers
#works until 2036...
isCPR <- function(v) {
  out <- list(problem=FALSE, message="")
  m <- "Warning: The variable seems to consist of Danish civil regristration (CPR) numbers."
  v <- as.character(na.omit(v))
  posCPR <- FALSE
  chars <- nchar(v)

  if (!all(chars %in% c(10,11))) return(out)

  if (all(chars == 10)) {
    posCPR <- grepl("[0-9]{10}", v)
  }
  if (all(chars== 11)) {
    posCPR <- grepl("[0-9]{6}-[0-9]{4}", v)
  }

  if (!all(posCPR)) return(out)

  if (!all(isDanishDate(substring(v, 1, 6)))) return(out)
  
  v <- gsub("-", "", v)
  
  year <- as.numeric(substring(v, 5, 6))
  digit7 <- substring(v, 7, 7)

  noCheckPl <- year<36 & year>=7 & digit7 >= 4 #is this right?
  
  if (!all(noCheckPl)) {
    check <- function(x) {
      x <- as.numeric(strsplit(x, "")[[1]])
      a <- c(4, 3, 2, 7, 6, 5, 4, 3, 2, 1)
      (x %*% a) %% 11 == 0 #note: x %*% a = a %*% x for 1 x n vectors in R
    }
    res <- sapply(v[!noCheckPl], check)
    if (!all(res)) return(out)
  } else if (!all(digit7[noCheckPl]>3)) return(out)
  
  out$problem <- TRUE
  out$message <- m
  out
}

#Checks whether strs contains only entries on the form DDMMYY
isDanishDate <- function(strs) {
  if (!(all(nchar(strs) == 6) & all(grepl("[0-9]{6}", strs)))) return(FALSE)

  ds <- as.numeric(substring(strs, 1, 2))
  ms <- as.numeric(substring(strs, 3, 4))

  if (any(ms > 13)) return(FALSE)

  mds <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  maxDs <- mds[ms]

  if (any(ds > maxDs)) return(FALSE)

  TRUE
}


#Extract a function summary/description from checkFunction objects and 
#return the function name for other function types. 
#NOTE: fName is a string containing the function name and therefore, 
#this function cannot be implemented as a generic function with 
#methods.
funSum <- function(fName) {
  foo <- get(fName)
  if ("checkFunction" %in% class(foo)) {
    out <- attr(foo, "description")
  } else out <- fName
  out
}

