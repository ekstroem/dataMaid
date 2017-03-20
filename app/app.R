library(shiny)
library(shinyjs)
library(dataMaid)


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("<center> dataMaid online tools </center>")),
  fluidRow(
  sidebarLayout(
    sidebarPanel(
      fluidRow(HTML("<b> Step 1: </b> Upload data")),
      fluidRow(fileInput("data", "")),
      fluidRow(HTML(paste("<b> Step 2: </b>", 
                          "Choose what your report should contain using the panel to the right <br/>"))),
      fluidRow(HTML("<b> Step 3: </b> Do data cleaning")),
      fluidRow(actionButton("doClean", textOutput("dataStatus"))),
      fluidRow("Step 4: Download your results"),
      fluidRow(downloadButton("report", textOutput("cleanStatus")))
      ),
    mainPanel(tabsetPanel(
      tabPanel("Options for data inputting", "Set options here..."),
      tabPanel("Options for report", "More options here"),
      tabPanel("Help", "Maybe a tutorial or whatever here?")
    ))
  )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Data", dataTableOutput("dataHead")),
      tabPanel(textOutput("reportTab"), fluidRow(uiOutput("reportPage")))
               
               #dataTableOutput("dataHead3"))) 
      #uiOutput("report"))
                #htmlOutput("report"))
               #includeHTML("jazz.html"))
   )
  )
))


server <- function(input, output) {
  dataDone <- FALSE
  data <- NULL
  readyForClean <- FALSE
  cleanDone <- FALSE
  
  makeReactiveBinding("dataDone")
  makeReactiveBinding("data")
  makeReactiveBinding("readyForClean")
  makeReactiveBinding("cleanDone")
  
  observeEvent(input$data, {
    dataDone <<- TRUE
    dataFile <- input$data
    data <<- read.table(dataFile$datapath, header = TRUE)
  })
  
  output$dataHead <- renderDataTable({data})
  output$dataHead3 <- renderDataTable({data})
  
  output$reportPage <- renderUI({
    if (cleanDone) {
     includeHTML("dataMaid_report.html")
    } else NULL
  })
  
  output$reportTab <- renderText({
    if (!cleanDone) return(NULL)
    
    "Report"
  })
  
  observeEvent(input$doClean, {
   readyForClean <<- TRUE
  })
  
  output$dataStatus <- renderText({
    #dataFile <- input$data
    
    if (!dataDone) return("Please wait")
    
    "Make report"
  })
  
   output$cleanStatus <- reactive({
    if (!readyForClean) return("Please wait")
     
     clean(data, replace = TRUE, file = "dataMaid_report.rmd", openResult = FALSE,
           output = "html")
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