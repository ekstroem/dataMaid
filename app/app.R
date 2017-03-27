library(shiny)
library(shinyjs)
library(dataMaid)
library(haven)
source("busyIndicator.R")

c1_w <- 4
cRes_w <- 1

ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
  sidebarLayout(
    sidebarPanel(
      HTML("<title> dataMaid online tools </title>"),
      h2("dataMaid online tools"),
      br(),
      fluidRow(HTML("<b> Step 1: </b> Upload data.")),
      fluidRow(fileInput("data", ""), paste("Use the options for data loading to the right",
                     "and the data preview below to",
                     "ensure that the data is loaded correctly.")),
      fluidRow(br(), HTML(paste("<b> Step 2: </b>", 
                          "Choose what your report should contain using the panel to the right."))),
      fluidRow(br(), HTML("<b> Step 3: </b> Do data cleaning.")),
      fluidRow(withBusyIndicatorUI(actionButton("doClean", textOutput("dataStatus")))),
      fluidRow(br(), HTML("<b> Step 4: </b> Download (if pdf) or view (if html) your report below."))
      ),
    mainPanel(tabsetPanel(
      tabPanel("Options for data inputting", 
                fluidRow(
                  column(6, 
                     radioButtons("fileType", "File format", list("Text (txt)" = "txt",
                                                                  "Comma-separated (csv)" = "csv",
                                                                  "Stata (dta)" = "stata",
                                                                  "SAS (sas7bdat)" = "sas"))
                   ),
                  column(6, 
                         conditionalPanel(condition = "input.fileType == \"txt\" | input.fileType == \"csv\"",
                            radioButtons("decimalChar", "Decimal mark", list(". (point)" = ".", ", (comma)" = ","),
                                         inline = TRUE),
                            checkboxInput("headerTRUE", "First data line contains the variable names", TRUE)
                         ),
                         conditionalPanel(condition = "input.fileType == \"csv\"",
                                          radioButtons("csvSep", "Column separator", 
                                                        list(", (comma)" = ",", "; (semicolon)" = ";"),
                                                       inline = TRUE))
                  )
                ),
               hr(),
               fluidRow(
                  checkboxGroupInput("missStrStd", "Strings used to indicate missing values",
                                     list("NA", "NaN", ". (dot)"=".", " (empty string)" = ""), 
                                     selected = "NA")
               )#,
              #to do: Change such that an update is trickered by a submit button
              #if the dataset looks large...
              # radioButtons("lineSep")
               ),
      tabPanel("Options for report", 
               fluidRow(
                  column(4, radioButtons("clean.output", "Output format", list("html (web)" = "html", "pdf" = "pdf"))),
                  column(4, radioButtons("clean.ordering", "Variable order",
                                        list("As is" = "asIs", "Alphabetical" = "alphabetical"))),
                  column(4, radioButtons("clean.maxProbVals", "Number of displayed problematic values",
                                     list("1"= 1, "5" = 5, "10" = 10, "All" = Inf),
                                     selected = 10))
               ),
               hr(),
               fluidRow(HTML("<b> Choose what checks to perform </b>"),
                 fluidRow(
                   column(c1_w, ""),
                   column(cRes_w, "char.", align = "center"),
                   column(cRes_w, "factor", align = "center"),
                   column(cRes_w, "labelled", align = "center"),
                   column(cRes_w, "numeric", align = "center"),
                   column(cRes_w, "integer", align = "center"),
                   column(cRes_w, "logical", align = "center"),
                   column(cRes_w, "Date", align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify case issues"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.c", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.f", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.l", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.n", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.i", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.d", "", FALSE), align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify levels with < 6 obs."),
                   column(cRes_w, checkboxInput("identifyLoners.c", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.f", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.l", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.n", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.i", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyLoners.d", "", FALSE), align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify miscoded missing values"),
                   column(cRes_w, checkboxInput("identifyMissing.c", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.f", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.l", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.n", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.i", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyMissing.d", "", FALSE), align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify misclassified numeric or integer variables"),
                   column(cRes_w, checkboxInput("identifyNums.c", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.f", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.l", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.n", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.i", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyNums.d", "", FALSE), align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify outliers"),
                   column(cRes_w, checkboxInput("identifyOutliers.c", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.f", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.l", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.n", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.i", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyOutliers.d", "", TRUE), align = "center")
                 ),
                 fluidRow(
                   column(c1_w, "Identify prefixed and suffixed whitespace"),
                   column(cRes_w, checkboxInput("identifyWhitespace.c", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.f", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.l", "", TRUE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.n", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.i", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.b", "", FALSE), align = "center"),
                   column(cRes_w, checkboxInput("identifyWhitespace.d", "", FALSE), align = "center")
                )
               )
      ),
      tabPanel("About", includeHTML("about.html"))
    ))
  )
  ),
  fluidRow(
    tabsetPanel(id = "dataReportPanel",
      tabPanel("Data", dataTableOutput("dataHead"), value = "dataTab"),
      tabPanel(textOutput("reportTab"), value = "reportTab",
               fluidRow(
                 column(10),
                 column(2, downloadButton("report", textOutput("cleanStatus")))),
               conditionalPanel(condition = "input.clean.output == \"html\"",
                                uiOutput("reportPageHTML")#,
               )
#               tags$iframe(style="height:600px; width:100%", # src="http://www.dna.caltech.edu/Papers/DNAorigami-nature.pdf")
 #                          src = "dataMaid_d.pdf")
#dataMaid_report.pdf")
              # htmlOutput("reportPagePdf")
      )
    )
   )
))

allChecks <- c("identifyCaseIssues", "identifyLoners", "identifyMissing", 
               "identifyNums", "identifyOutliers", "identifyWhitespace")


#data(toyData)
server <- function(input, output, session) {
 # outputType <- "html"
  dataDone <- FALSE
  data <- NULL
  readyForClean <- FALSE
  cleanDone <- FALSE
  if (TRUE) { #statement only here so I can collapse the contents 
              #into one in my code-editor
    checks.c <- list(identifyCaseIssues = TRUE, 
                   identifyLoners = TRUE,
                   identifyMissing = TRUE,
                   identifyNums = TRUE,
                   identifyOutliers = FALSE,
                   identifyWhitespace = TRUE)
  checks.f <- list(identifyCaseIssues = TRUE, 
                   identifyLoners = TRUE,
                   identifyMissing = TRUE,
                   identifyNums = TRUE,
                   identifyOutliers = FALSE,
                   identifyWhitespace = TRUE)
  checks.l <- list(identifyCaseIssues = TRUE, 
                   identifyLoners = TRUE,
                   identifyMissing = TRUE,
                   identifyNums = TRUE,
                   identifyOutliers = FALSE,
                   identifyWhitespace = TRUE)
  checks.n <- list(identifyCaseIssues = FALSE, 
                   identifyLoners = FALSE,
                   identifyMissing = TRUE,
                   identifyNums = FALSE,
                   identifyOutliers = TRUE,
                   identifyWhitespace = FALSE)
  checks.i <- list(identifyCaseIssues = FALSE, 
                   identifyLoners = FALSE,
                   identifyMissing = TRUE,
                   identifyNums = FALSE,
                   identifyOutliers = TRUE,
                   identifyWhitespace = FALSE)
  checks.b <- list(identifyCaseIssues = FALSE, 
                   identifyLoners = FALSE,
                   identifyMissing = FALSE,
                   identifyNums = FALSE,
                   identifyOutliers = FALSE,
                   identifyWhitespace = FALSE)
  checks.d <- list(identifyCaseIssues = FALSE, 
                   identifyLoners = FALSE,
                   identifyMissing = FALSE,
                   identifyNums = FALSE,
                   identifyOutliers = TRUE,
                   identifyWhitespace = FALSE)
  
  makeReactiveBinding("dataDone")
  makeReactiveBinding("data")
  makeReactiveBinding("readyForClean")
  makeReactiveBinding("cleanDone")
  makeReactiveBinding("outputType")
  
  observeEvent(input$identifyCaseIssues.c, {checks.c["identifyCaseIssues"] <<- input$identifyCaseIssues.c})
  observeEvent(input$identifyLoners.c, {checks.c["identifyLoners"] <<- input$identifyLoners.c})
  observeEvent(input$identifyMissing.c, {checks.c["identifyMissing"] <<- input$identifyMissing.c})
  observeEvent(input$identifyNums.c, {checks.c["identifyNums"] <<- input$identifyNums.c})
  disable("identifyOutliers.c")
  observeEvent(input$identifyWhitespace.c, {checks.c["identifyWhitespace"] <<- input$identifyWhitespace.c})
  
  
  observeEvent(input$identifyCaseIssues.f, {checks.f["identifyCaseIssues"] <<- input$identifyCaseIssues.f})
  observeEvent(input$identifyLoners.f, {checks.f["identifyLoners"] <<- input$identifyLoners.f})
  observeEvent(input$identifyMissing.f, {checks.f["identifyMissing"] <<- input$identifyMissing.f})
  observeEvent(input$identifyNums.f, {checks.f["identifyNums"] <<- input$identifyNums.f})
  observeEvent(input$identifyOutliers.f, {checks.f["identifyOutliers"] <<- input$identifyOutliers.f})
  disable("identifyOutliers.f")
  observeEvent(input$identifyWhitespace.f, {checks.f["identifyWhitespace"] <<- input$identifyWhitespace.f})
  
  observeEvent(input$identifyCaseIssues.l, {checks.l["identifyCaseIssues"] <<- input$identifyCaseIssues.l})
  observeEvent(input$identifyLoners.l, {checks.l["identifyLoners"] <<- input$identifyLoners.l})
  observeEvent(input$identifyMissing.l, {checks.l["identifyMissing"] <<- input$identifyMissing.l})
  observeEvent(input$identifyNums.l, {checks.l["identifyNums"] <<- input$identifyNums.l})
  observeEvent(input$identifyOutliers.l, {checks.l["identifyOutliers"] <<- input$identifyOutliers.l})
  disable("identifyOutliers.l")
  observeEvent(input$identifyWhitespace.l, {checks.l["identifyWhitespace"] <<- input$identifyWhitespace.l})
  
  disable("identifyCaseIssues.n")
  disable("identifyLoners.n")
  observeEvent(input$identifyMissing.n, {checks.n["identifyMissing"] <<- input$identifyMissing.n})
  disable("identifyNums.n")
  observeEvent(input$identifyOutliers.n, {checks.n["identifyOutliers"] <<- input$identifyOutliers.n})
  disable("identifyWhitespace.n")
  
  disable("identifyCaseIssues.i")
  disable("identifyLoners.i")
  observeEvent(input$identifyMissing.i, {checks.i["identifyMissing"] <<- input$identifyMissing.i})
  disable("identifyNums.i")
  observeEvent(input$identifyOutliers.i, {checks.i["identifyOutliers"] <<- input$identifyOutliers.i})
  disable("identifyWhitespace.i")
  
  disable("identifyCaseIssues.b")
  disable("identifyLoners.b")
  disable("identifyMissing.b")
  disable("identifyNums.b")
  disable("identifyOutliers.b")
  disable("identifyWhitespace.b")  
  
  disable("identifyCaseIssues.d")
  disable("identifyLoners.d")
  disable("identifyMissing.d")
  disable("identifyNums.d")
  observeEvent(input$identifyOutliers.d, {checks.d["identifyOutliers"] <<- input$identifyOutliers.d})
  disable("identifyWhitespace.d")
  }
  
  observeEvent(input$data, {
    dataDone <<- TRUE
    dataFile <<- input$data
  #  print(str(input$data))
  })
  
  output$dataHead <- renderDataTable({
    if (dataDone) {
      naStrs <- unlist(input$missStrStd)
      if (input$fileType %in% c("txt", "csv")) {
        if (input$fileType == "csv") {
          if (input$csvSep == ",") loadFunction <- "read.csv"
          if (input$csvSep == ";") loadFunction <- "read.csv2"
        }
        else loadFunction <- "read.table"
        data <<- do.call(loadFunction, list(file = dataFile$datapath, header = input$headerTRUE, 
                                            dec = input$decimalChar, na.strings = naStrs))
      }
      if (input$fileType == "stata") {
        data <<- read_stata(dataFile$datapath)
        for (i in 1:ncol(data)) {
           data[data[, i] %in% naStrs, i] <- NA
        }
      }
      if (input$fileType == "sas") {
        data <<- read_sas(dataFile$datapath)
        for (i in 1:ncol(data)) {
          data[data[, i] %in% naStrs, i] <- NA
        }
      }
      return(data)
    } else NULL
  })

  output$reportTab <- renderText({
    if (!cleanDone) return(NULL)
   "Report"
  })
  
  observeEvent(input$doClean, {
   withBusyIndicatorServer("doClean", {
     readyForClean <<- TRUE
     clean(data, replace = TRUE, file = "dataMaid_report.rmd", openResult = FALSE,
           output = input$clean.output,
           ordering = input$clean.ordering,
           maxProbVals = as.numeric(input$clean.maxProbVals),
           characterChecks = allChecks[unlist(checks.c)],
           factorChecks = allChecks[unlist(checks.f)],
           labelledChecks = allChecks[unlist(checks.l)],
           numericChecks = allChecks[unlist(checks.n)],
           integerChecks = allChecks[unlist(checks.i)],
           logicalChecks = allChecks[unlist(checks.b)],
           dateChecks = allChecks[unlist(checks.d)],
           reportTitle = dataFile$name) 
     message("clean done!")
     fileName <<- paste("dataMaid_report.", ifelse(input$clean.output == "html", "html", "pdf"), sep = "") #OBS
     output$reportPageHTML <- renderUI({
       if (cleanDone & input$clean.output == "html") { #OBS
           includeHTML(fileName)
       } else NULL
     })
     output$report <- downloadHandler(
       filename = fileName, 
       content = function(file) file.copy(fileName, file)
     )
 #  output$reportPagePdf <- renderText({
#     if (cleanDone & input$clean.output == "pdf") {
#        paste('<iframe style="height:600px; width:100%" src="dataMaid_report.pdf"></iframe>')
#     } else NULL
#   })
     updateTabsetPanel(session, "dataReportPanel", selected = "reportTab")
   })
  })
  
 # observeEvent(input$clean.output, {
   # browser()
  #             print(input$clean.output)
   # print(outputType)
  #})
  
  output$dataStatus <- renderText({
    if (!dataDone) return("Please wait")
    outputType <<- input$clean.output
    "Make report"
  })
  
  output$cleanStatus <- reactive({
    if (!readyForClean) "Please wait"
   cleanDone <<- TRUE
   "Download report"
  })
    
#  fileName <- reactive({
#    paste("dataMaid_report.", ifelse(input$clean.output == "html", "html", "pdf"), sep = "")
#  })
    
  
  
  on.exit({
    unlink("dataMaid_report.html")
    unlink("dataMaid_report.rdm")
  })
  
}

#runApp(list(ui = ui, server = server))
shinyApp(ui=ui, server=server)
