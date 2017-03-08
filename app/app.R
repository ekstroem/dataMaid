library(shiny)
library(shinyjs)
library(dataMaid)


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("dataMaid online tools"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(HTML("<b> Step 1: </b> Upload data")),
      fluidRow(fileInput("data", "")),
      fluidRow(HTML(paste("<b> Step 2: </b>", 
                          "Choose what your report should contain using the panel to the right <br/>"))),
      fluidRow("Step 3: Do data cleaning"),
      fluidRow(actionButton("doClean", textOutput("dataStatus"))),
      fluidRow("Step 4: Download your results"),
      fluidRow(downloadButton("report", textOutput("cleanStatus")))
      ),
    mainPanel("Set options here...")
    )
  )
)


server <- function(input, output) {
  dataDone <- FALSE
  data <- NULL
  readyForClean <- FALSE
  makeReactiveBinding("dataDone")
  makeReactiveBinding("data")
  makeReactiveBinding("readyForClean")
  
  observeEvent(input$data, {
    dataDone <<- TRUE
    dataFile <- input$data
    data <<- read.table(dataFile$datapath)
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
     
     clean(data, replace = TRUE, file = "dataMaid_report.rmd", openResult = FALSE)
   "Download report"
  })
    
  output$report <- downloadHandler(
    filename = "dataMaid_data.pdf",
    content = function(file) file.copy("dataMaid_report.pdf", file)
  )
  
  on.exit({
    unlink("dataMaid_report.pdf")
    unlink("dataMaid_report.rdm")
  })
  
}

runApp(list(ui = ui, server = server))
