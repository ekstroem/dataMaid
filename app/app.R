library(shiny)
library(shinyjs)
library(dataMaid)


c1_w <- 4
cRes_w <- 1

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("<center> dataMaid online tools </center>")),
  fluidRow(
  sidebarLayout(
    sidebarPanel(
      fluidRow(HTML("<b> Step 1: </b> Upload data.")),
      fluidRow(fileInput("data", ""), paste("Use the options for data loading to the right",
                     "and the data preview below to",
                     "ensure that the data is loaded correctly.")),
      fluidRow(br(), HTML(paste("<b> Step 2: </b>", 
                          "Choose what your report should contain using the panel to the right."))),
      fluidRow(br(), HTML("<b> Step 3: </b> Do data cleaning.")),
      fluidRow(actionButton("doClean", textOutput("dataStatus"))),
      fluidRow(br(), HTML("<b> Step 4: </b> Download your results or view your report below.")),
      fluidRow(downloadButton("report", textOutput("cleanStatus")))
      ),
    mainPanel(tabsetPanel(
      tabPanel("Options for data inputting", 
               radioButtons("fileType", "File format", list("Text (txt)" = "txt",
                                                            "Comma-separated (csv) (NOT IMPLEMENTED)" = "csv",
                                                            "Stata (dta) (NOT IMPLEMENTED)" = "stata",
                                                            "SAS (sas7bdat) (NOT IMPLEMENTED)" = "sas")),
               checkboxInput("headerTRUE", "First data line contains the variable names", TRUE),
               radioButtons("decimalChar", "Decimal mark", list(". (point)" = ".", ", (comma)" = ",")),
               checkboxGroupInput("missStrStd", "Strings used to indicate missing values",
                                  list("NA", "NaN", ". (dot)"=".", " (empty string)" = ""), 
                                  selected = "NA")#,
              #to do: Change such that an update is trickered by a submit button
              #if the dataset looks large...
              # radioButtons("lineSep")
               ),
      tabPanel("Options for report", 
               fluidRow(
                  column(4, radioButtons("clean.output", "Output format", list("html", "pdf"))),
                  column(4, radioButtons("clean.ordering", "Variable order",
                                        list("As is" = "asis", "Alphabetical" = "alphabetical"))),
                  column(4, radioButtons("clean.maxProbVals", "Number of displayed problematic values",
                                     list("0"= 0, "5" = 5, "10" = 10, "All" = Inf),
                                     selected = "10"))
               ),
               fluidRow(HTML("<b> Choose what checks to perform: </b>"),
                 fluidRow(
                   column(c1_w, ""),
                   column(cRes_w, "char."),
                   column(cRes_w, "factor"),
                   column(cRes_w, "labelled"),
                   column(cRes_w, "numeric"),
                   column(cRes_w, "integer"),
                   column(cRes_w, "logical"),
                   column(cRes_w, "Date")
                 ),
                 fluidRow(
                   column(c1_w, "Identify case issues"),
                   column(cRes_w, checkboxInput("identifyCaseIssues.c", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyCaseIssues.f", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyCaseIssues.l", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, "")
                 ),
                 fluidRow(
                   column(c1_w, "Identify levels with < 6 obs."),
                   column(cRes_w, checkboxInput("identifyLoners.c", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyLoners.f", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyLoners.l", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, "")
                 ),
                 fluidRow(
                   column(c1_w, "Identify miscoded missing values"),
                   column(cRes_w, checkboxInput("identifyMissing.c", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyMissing.f", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyMissing.l", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyMissing.n", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyMissing.i", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, "")
                 ),
                 fluidRow(
                   column(c1_w, "Identify misclassified numeric or integer variables"),
                   column(cRes_w, checkboxInput("identifyNums.c", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyNums.f", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyNums.l", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, "")
                 ),
                 fluidRow(
                   column(c1_w, "Identify outliers"),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, checkboxInput("identifyOutliers.n", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyOutliers.i", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, checkboxInput("identifyOutliers.d", "", TRUE))
                 ),
                 fluidRow(
                   column(c1_w, "Identify prefixed and suffixed whitespace"),
                   column(cRes_w, checkboxInput("identifyWhitespace.c", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyWhitespace.f", "", TRUE)),
                   column(cRes_w, checkboxInput("identifyWhitespace.l", "", TRUE)),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, ""),
                   column(cRes_w, "")
                )
               )
      ),
      tabPanel("Help", "Maybe a tutorial or whatever here?")
    ))
  )
  ),
  fluidRow(
    tabsetPanel(id = "dataReportPanel",
      tabPanel("Data", dataTableOutput("dataHead"), value = "dataTab"),
      tabPanel(textOutput("reportTab"), uiOutput("reportPage"), value = "reportTab")
   )
  )
))

allChecks <- c("identifyCaseIssues", "identifyLoners", "identifyMissing", 
               "identifyNums", "identifyOutliers", "identifyWhitespace")

server <- function(input, output, session) {
  dataDone <- FALSE
  data <- NULL
  readyForClean <- FALSE
  cleanDone <- FALSE
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
  
  observeEvent(input$identifyCaseIssues.c, {checks.c["identifyCaseIssues"] <<- input$identifyCaseIssues.c})
  observeEvent(input$identifyLoners.c, {checks.c["identifyLoners"] <<- input$identifyLoners.c})
  observeEvent(input$identifyMissing.c, {checks.c["identifyMissing"] <<- input$identifyMissing.c})
  observeEvent(input$identifyNums.c, {checks.c["identifyNums"] <<- input$identifyNums.c})
  observeEvent(input$identifyWhitespace.c, {checks.c["identifyWhitespace"] <<- input$identifyWhitespace.c})
  
  observeEvent(input$identifyCaseIssues.f, {checks.f["identifyCaseIssues"] <<- input$identifyCaseIssues.f})
  observeEvent(input$identifyLoners.f, {checks.f["identifyLoners"] <<- input$identifyLoners.f})
  observeEvent(input$identifyMissing.f, {checks.f["identifyMissing"] <<- input$identifyMissing.f})
  observeEvent(input$identifyNums.f, {checks.f["identifyNums"] <<- input$identifyNums.f})
  observeEvent(input$identifyOutliers.f, {checks.f["identifyOutliers"] <<- input$identifyOutliers.f})
  observeEvent(input$identifyWhitespace.f, {checks.f["identifyWhitespace"] <<- input$identifyWhitespace.f})
  
  observeEvent(input$identifyCaseIssues.l, {checks.l["identifyCaseIssues"] <<- input$identifyCaseIssues.l})
  observeEvent(input$identifyLoners.l, {checks.l["identifyLoners"] <<- input$identifyLoners.l})
  observeEvent(input$identifyMissing.l, {checks.l["identifyMissing"] <<- input$identifyMissing.l})
  observeEvent(input$identifyNums.l, {checks.l["identifyNums"] <<- input$identifyNums.l})
  observeEvent(input$identifyOutliers.l, {checks.l["identifyOutliers"] <<- input$identifyOutliers.l})
  observeEvent(input$identifyWhitespace.l, {checks.l["identifyWhitespace"] <<- input$identifyWhitespace.l})
  
  observeEvent(input$identifyMissing.n, {checks.n["identifyMissing"] <<- input$identifyMissing.n})
  observeEvent(input$identifyOutliers.n, {checks.n["identifyOutliers"] <<- input$identifyOutliers.n})
  
  observeEvent(input$identifyMissing.i, {checks.i["identifyMissing"] <<- input$identifyMissing.i})
  observeEvent(input$identifyOutliers.i, {checks.i["identifyOutliers"] <<- input$identifyOutliers.i})
  
  observeEvent(input$identifyOutliers.d, {checks.d["identifyOutliers"] <<- input$identifyOutliers.d})
  
  observeEvent(input$data, {
    dataDone <<- TRUE
    dataFile <<- input$data
  })
  
  output$dataHead <- renderDataTable({
    if (dataDone) {
      data <<- read.table(dataFile$datapath, header = input$headerTRUE, 
                          dec = input$decimalChar, na.strings = unlist(input$missStrStd))
      return(data)
    } else NULL
  })

  output$reportTab <- renderText({
    if (!cleanDone) return(NULL)
   "Report"
  })
  
  observeEvent(input$doClean, {
   readyForClean <<- TRUE
   clean(data, replace = TRUE, file = "dataMaid_report.rmd", openResult = FALSE,
         output = input$clean.output,
         ordering = input$clean.ordering,
         maxProbVals = input$clean.maxProbVals,
         characterChecks = allChecks[unlist(checks.c)],
         factorChecks = allChecks[unlist(checks.f)],
         labelledChecks = allChecks[unlist(checks.l)],
         numericChecks = allChecks[unlist(checks.n)],
         integerChecks = allChecks[unlist(checks.i)],
         logicalChecks = allChecks[unlist(checks.b)],
         dateChecks = allChecks[unlist(checks.d)]) 
   output$reportPage <- renderUI({
     if (cleanDone) {
       includeHTML("dataMaid_report.html")
     } else NULL
   })
   updateTabsetPanel(session, "dataReportPanel", selected = "reportTab")
  })
  
  output$dataStatus <- renderText({
    if (!dataDone) return("Please wait")
    "Make report"
  })
  
  output$cleanStatus <- reactive({
    if (!readyForClean) return("Please wait")
    cleanDone <<- TRUE
   "Download report"
  })
    
  output$report <- downloadHandler(
    filename = "dataMaid_report.html",
    content = function(file) file.copy("dataMaid_report.html", file)
  )
  
  on.exit({
    unlink("dataMaid_report.html")
    unlink("dataMaid_report.rdm")
  })
  
}

#runApp(list(ui = ui, server = server))
shinyApp(ui=ui, server=server)
