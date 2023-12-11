library(shiny)
library(DT)  # Ensure DT is loaded for dataTableOutput
library(MissensePathoR)

ui <- fluidPage(
  titlePanel("Pathogenicity Analysis of Missense Variants"),
  sidebarLayout(
    sidebarPanel(
      tags$p("This is a Shiny App that is part of the MissensePathoR in R."),
      br(),

      tags$b("Description: MissensePathoR is an R package designed for pathogenicity analysis of missense variants in the human proteome using AlphaMissense's deep learning predictions. This Shiny App is a component of the MissensePathoR package, includes functions for summarizing variant classes (classSummary) and visualizing enriched pathways (pathwayViz). Users can explore the distribution and impact of pathogenic variants, and understand the biological pathways involved in their genomic data. For more detail, see ?classSummary and ?pathwayViz."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$p("Instructions: Below, upload required to perform the analysis. Then press 'Run Analysis'. Navigate through
             the different tabs to the right to explore the results."),

      # br() element to introduce extra vertical spacing ----
      br(),

      fileInput("vcfFile", "Upload your grouped VCF file (csv format).", accept = c(".csv")),
      # Side note for reading VCF files
      uiOutput("downloadLinkVcfSample"),
      actionButton("vcfSampleBtn", "Example VCF file format"),  # Changed to actionButton for simplicity
      tags$p("Note: You can use readVCF() to read VCF files in a directory, then fwrite()"),
      br(), br(),
      fileInput("alphaMissenseFile", "Upload AlphaMissense Predictions (tsv/tsv.gz format).", accept = c(".tsv.gz",".tsv")),
      # Side note for downloading AlphaMissense Predictions
      uiOutput("downloadLinkAlphaMissenseSample"),
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
                           selectInput("pathwayType", "Select Pathway Type:", choices = c("all significant genes"= "sig", "up-regulated genes" = "up", "down-regulated genes" = "down")),
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

  # example data download
  # URL for downloading VCF sample data
  output$downloadLinkVcfSample <- renderUI({
    a("Download VCF Sample Dataset", href = "https://github.com/Lola-W/MissensePathoR/raw/main/inst/extdata/vcfSample.csv", target = "_blank")
  })

  # Modal for VCF sample data description
  observeEvent(input$downloadLinkVcfSample, {
    shinyalert(title = "VCF Sample Dataset",
               text = "This dataset contains combined VCF data for Hela cell replicates across four time points (0, 1, 4, and 8 hours) after introducing H2O2, processed with the `readVCF` function from the MissensePathoR package. Citation: Rendleman J, Cheng Z, Maity S, et al. New insights into the cellular temporal response to proteostatic stress. Elife. 2018;7:e39054. doi: 10.7554/eLife.39054.",
               type = "info")
  })

  # URL for downloading AlphaMissense sample data
  output$downloadLinkAlphaMissenseSample <- renderUI({
    a("Download AlphaMissense Sample Dataset", href = "https://github.com/Lola-W/MissensePathoR/raw/main/inst/extdata/AlphaMissenseSample.tsv", target = "_blank")
  })

  # Modal for AlphaMissense sample data description
  observeEvent(input$downloadLinkAlphaMissenseSample, {
    shinyalert(title = "AlphaMissense Sample Dataset",
               text = "A sample dataset containing missense variant predictions from the AlphaMissense community dataset resource. Citation: Jun Cheng et al., 'Accurate proteome-wide missense variant effect prediction with AlphaMissense.' Science 381, eadg7492 (2023). DOI: 10.1126/science.adg7492.",
               type = "info")
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
