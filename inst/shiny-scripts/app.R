library(shiny)
library(DT)  # Ensure DT is loaded for dataTableOutput
library(MissensePathoR)

ui <- fluidPage(
  titlePanel("Pathogenicity Analysis of Missense Variants"),
  sidebarLayout(
    sidebarPanel(
      tags$p("This is a simple Shiny App that is part of the MissensePathoR in R."),
      br(),
      fileInput("vcfFile", "Upload your grouped VCF file (csv format).", accept = c(".csv")),
      # Side note for reading VCF files
      actionButton("vcfSampleBtn", "Example VCF file format"),  # Changed to actionButton for simplicity
      tags$p("Note: You can use readVCF() to read VCF files in a directory, then fwrite()"),
      br(), br(),
      fileInput("alphaMissenseFile", "Upload AlphaMissense Predictions (tsv/tsv.gz format).", accept = c(".tsv.gz",".tsv")),
      # Side note for downloading AlphaMissense Predictions
      actionButton("alphaMissenseSampleBtn", "Example AlphaMissense file format"),  # Changed to actionButton for simplicity
      tags$p("Note: You can download AlphaMissense Predictions from ",
             tags$a(href = "https://storage.googleapis.com/dm_alphamissense/AlphaMissense_hg38.tsv.gz",
                    target = "_blank",
                    "this link.")),
      br(),
      actionButton("runAnalysis", "Run Analysis")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Class Summary",
                           DT::dataTableOutput("classSummaryTable")),
                  tabPanel("Pathway Visualization",
                           selectInput("pathwayType", "Select Pathway Type:", choices = c("sig", "up", "down")),
                           plotOutput("pathwayPlot")))
      )
  )
)

server <- function(input, output, session) {
  observeEvent(input$vcfSampleBtn, {
    showModal(modalDialog(
      title = "Example VCF Sample Format",
      DT::dataTableOutput("vcfSampleTable")
    ))
  })

  output$vcfSampleTable <- DT::renderDataTable({
    DT::datatable(head(MissensePathoR::vcfSample))
  })

  observeEvent(input$alphaMissenseSampleBtn, {
    showModal(modalDialog(
      title = "Example AlphaMissense Sample Format",
      DT::dataTableOutput("alphaMissenseSampleTable")
    ))
  })

  output$alphaMissenseSampleTable <- DT::renderDataTable({
    DT::datatable(head(MissensePathoR::AlphaMissenseSample))
  })


  # For classSummary
  runAnalysis <- eventReactive(input$runAnalysis, {
    if (is.null(input$vcfFile) || is.null(input$alphaMissenseFile)) {
      return(NULL)
    }
    # Read the files
    vcfData <- fread(input$vcfFile$datapath)
    alphaMissenseData <- fread(input$alphaMissenseFile$datapath)

    # Run predictPathoScore (assuming this function is defined in your package)
    predScoreSample <- predictPathoScore(vcfData, alphaMissenseData)

    # Run classSummary
    classSummary(predScoreSample)
  })

  # Output for classSummary
  output$classSummaryTable <- DT::renderDataTable({
    runAnalysis()
  })

  pathwayData <- eventReactive(input$runAnalysis, {
    req(input$vcfFile)
    req(input$alphaMissenseFile)

    # Read the files
    vcfData <- fread(input$vcfFile$datapath)
    alphaMissenseData <- fread(input$alphaMissenseFile$datapath)
    # Run predictPathoScore
    predScoreSample <- predictPathoScore(vcfData, alphaMissenseData)

    # Continuing the analysis pipeline
    result <- mapGene(predScoreSample)
    result$sample_name <- paste0(result$sample_name, "_", result$group)
    set.seed(1)
    diffOut <- diffSNPs(result, "0h")
    enrichOut <- enrichSNP(diffOut)
    enrichOut
  })

  # Plot output for pathwayViz
  output$pathwayPlot <- renderPlot({
    req(pathwayData())
    enrichOut <- pathwayData()
    pathwayViz(enrichOut, input$pathwayType)
  })

  # Example data display (optional, if you want to show example data by default)
  output$classSummaryTable <- DT::renderDataTable({
    if (is.null(runAnalysis())) {
      classSummary(predictPathoScore(MissensePathoR::vcfSample, MissensePathoR::AlphaMissenseSample))
    } else {
      runAnalysis()
    }
  })
}

shinyApp(ui = ui, server = server)
