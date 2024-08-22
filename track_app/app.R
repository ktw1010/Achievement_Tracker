#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyFiles)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(readxl)
library(dendextend)

ui <- navbarPage(
  "Data Explorer and Statistical Modeling",
  
  # First tab for Data Explorer
  tabPanel("Data Explorer",
           sidebarLayout(
             sidebarPanel(
               fileInput("existing_file", "Upload Existing Data File (if any)",
                         multiple = FALSE,
                         accept = c(".csv", ".xls", ".xlsx")),
               
               # Section for files with headers
               fileInput("files_with_headers", "Upload Data Files with Headers", 
                         multiple = TRUE, 
                         accept = c(".csv", ".txt", ".xls", ".xlsx")),
               
               # Section for files without headers and feature selection
               uiOutput("file_inputs_ui"),
               
               actionButton("add_file", "Add New File"),
               actionButton("combine_data", "Combine Data"),
               downloadButton("download_combined", "Download Combined Data"),
               actionButton("start_over", "Start Over")
             ),
             mainPanel(
               DT::dataTableOutput("combined_data_table")
             )
           )
  ),
  
  # Second tab for Statistical Modeling
  tabPanel("Statistical Modeling",
           sidebarLayout(
             sidebarPanel(
               fileInput("clean_file", "Upload Clean Data File"),
               
               selectInput("clustering_method", "Choose Clustering Method", 
                           choices = c("K-means", "Hierarchical")),
               
               numericInput("clusters", "Number of Clusters", value = 3, min = 2),
               
               actionButton("run_clustering", "Run Clustering"),
               actionButton("start_over_modeling", "Start Over")
             ),
             mainPanel(
               plotOutput("cluster_plot")
             )
           )
  )
)



server <- function(input, output, session) {
  combined_data <- reactiveVal()
  file_inputs <- reactiveValues(count = 1)
  
  # Dynamically generate file input and feature selection UI elements
  observe({
    output$file_inputs_ui <- renderUI({
      lapply(1:file_inputs$count, function(i) {
        tagList(
          fileInput(paste0("file", i), paste("Upload Data File ", i),
                    multiple = FALSE,
                    accept = c(".csv", ".txt", ".xls", ".xlsx")),
          selectInput(paste0("feature", i), paste("Select Feature for File ", i), 
                      choices = c(
                        "NSN", "AGS ID", "Form class", "Ethnicity 1", "Ethnicity 2", "Ethnicity 3", 
                        "Stats Ethnicity", "Level 1", "Level 2", "Level 2 Credit Count", "L1 Lit", 
                        "L1 Num", "UE Num", "UE Lit", "UE Read", "UE Write", "UE Read / Write", 
                        "L2 Externals % ABS", "L2 Externals % SNA", "L2 Externals % NA", 
                        "AS 91098 (L2)", "AS 91099 (L2)", "AS 91100 (L2)", "Total External", 
                        "AS 91101 (L2)", "AS 91102 (L2)", "AS 91106 (L2)", "Total Internal", 
                        "TOTAL CREDITS", "F6 School exam T1 Agg", "F6 School exam T2 Agg", 
                        "F6 School exam T3 Agg", "F6 School exam Avg agg", "F6 School exam Drop mid-year", 
                        "F6 Term 1 Attn", "F6 Term 2 Attn", "F6 Term 3 Attn", "F6 Term 4 Attn", 
                        "F6 Year Avg", "F6 Term 2-Term 3 Attn drop", "F6 RED FLAGS", "Does L3 CAS", 
                        "Does L3 CON", "F7 Term 1 Aggregate", "F7 Term 1 A&E D grades", 
                        "F7 Term 1 ZEROs", "2 May Term 1 Credit count", "16 June Term 2 Credit Count", 
                        "1 August Term 2 credit count", "F7 Term 2 Aggregate", "F7 Term 2 A&E D grades", 
                        "F7 Term 2 ZEROs", "TOTAL RED FLAGS", "F7 RED FLAGS"),
                      selected = "NSN")
        )
      })
    })
  })
  
  observeEvent(input$add_file, {
    file_inputs$count <- file_inputs$count + 1
  })
  
  observeEvent(input$combine_data, {
    # Initialize an empty data frame with the specified columns
    combined_df <- data.frame(
      NSN = character(),
      `AGS ID` = character(),
      `Form class` = character(),
      `Ethnicity 1` = character(),
      `Ethnicity 2` = character(),
      `Ethnicity 3` = character(),
      `Stats Ethnicity` = character(),
      `Level 1` = character(),
      `Level 2` = character(),
      `Level 2 Credit Count` = character(),
      `L1 Lit` = character(),
      `L1 Num` = character(),
      `UE Num` = character(),
      `UE Lit` = character(),
      `UE Read` = character(),
      `UE Write` = character(),
      `UE Read / Write` = character(),
      `L2 Externals % ABS` = character(),
      `L2 Externals % SNA` = character(),
      `L2 Externals % NA` = character(),
      `AS 91098 (L2)` = character(),
      `AS 91099 (L2)` = character(),
      `AS 91100 (L2)` = character(),
      `Total External` = character(),
      `AS 91101 (L2)` = character(),
      `AS 91102 (L2)` = character(),
      `AS 91106 (L2)` = character(),
      `Total Internal` = character(),
      `TOTAL CREDITS` = character(),
      `F6 School exam T1 Agg` = character(),
      `F6 School exam T2 Agg` = character(),
      `F6 School exam T3 Agg` = character(),
      `F6 School exam Avg agg` = character(),
      `F6 School exam Drop mid-year` = character(),
      `F6 Term 1 Attn` = character(),
      `F6 Term 2 Attn` = character(),
      `F6 Term 3 Attn` = character(),
      `F6 Term 4 Attn` = character(),
      `F6 Year Avg` = character(),
      `F6 Term 2-Term 3 Attn drop` = character(),
      `F6 RED FLAGS` = character(),
      `Does L3 CAS` = character(),
      `Does L3 CON` = character(),
      `F7 Term 1 Aggregate` = character(),
      `F7 Term 1 A&E D grades` = character(),
      `F7 Term 1 ZEROs` = character(),
      `2 May Term 1 Credit count` = character(),
      `16 June Term 2 Credit Count` = character(),
      `1 August Term 2 credit count` = character(),
      `F7 Term 2 Aggregate` = character(),
      `F7 Term 2 A&E D grades` = character(),
      `F7 Term 2 ZEROs` = character(),
      `TOTAL RED FLAGS` = character(),
      `F7 RED FLAGS` = character(),
      stringsAsFactors = FALSE
    )
    
    # Read and combine existing file if provided
    if (!is.null(input$existing_file)) {
      ext <- tools::file_ext(input$existing_file$name)
      existing_data <- if (ext == "csv" || ext == "txt") {
        read.csv(input$existing_file$datapath)
      } else if (ext == "xls" || ext == "xlsx") {
        read_excel(input$existing_file$datapath)
      } else {
        NULL
      }
      
      if (!is.null(existing_data)) {
        # Ensure all columns are character type
        existing_data <- mutate_all(existing_data, as.character)
        combined_df <- bind_rows(combined_df, existing_data)
      }
    }
    
    # Read and combine files with headers
    if (!is.null(input$files_with_headers)) {
      for (file in input$files_with_headers$datapath) {
        ext <- tools::file_ext(file)
        data <- if (ext == "csv" || ext == "txt") {
          read.csv(file, header = TRUE)
        } else if (ext == "xls" || ext == "xlsx") {
          read_excel(file, col_names = TRUE)
        } else {
          NULL
        }
        
        if (!is.null(data)) {
          # Ensure all columns are character type
          data <- mutate_all(data, as.character)
          combined_df <- bind_rows(combined_df, data)
        }
      }
    }
    
    # Read and combine files without headers
    for (i in 1:file_inputs$count) {
      file_input <- input[[paste0("file", i)]]
      feature <- input[[paste0("feature", i)]]
      
      if (!is.null(file_input)) {
        ext <- tools::file_ext(file_input$name)
        data <- if (ext == "csv" || ext == "txt") {
          read.csv(file_input$datapath, header = FALSE)
        } else if (ext == "xls" || ext == "xlsx") {
          read_excel(file_input$datapath, col_names = FALSE)
        } else {
          NULL
        }
        
        if (!is.null(data)) {
          # Set column names based on selected feature
          colnames(data) <- feature
          # Ensure all columns are character type
          data <- mutate_all(data, as.character)
          combined_df <- bind_rows(combined_df, data)
        }
      }
    }
    
    combined_data(combined_df)
  })
  
  # Display combined data table
  output$combined_data_table <- DT::renderDataTable({
    req(combined_data())
    DT::datatable(combined_data())
  })
  
  # Download combined data
  output$download_combined <- downloadHandler(
    filename = function() {
      paste("combined_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(combined_data(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$start_over, {
    combined_data(NULL)
    file_inputs$count <- 1
    output$file_inputs_ui <- renderUI(NULL)
    updateFileInput(session, "existing_file", value = NULL)
    updateFileInput(session, "files_with_headers", value = NULL)
  })
  
  observeEvent(input$start_over_modeling, {
    combined_data(NULL)
    updateFileInput(session, "clean_file", value = NULL)
    updateSelectInput(session, "clustering_method", selected = "K-means")
    updateNumericInput(session, "clusters", value = 3)
  })
}


shinyApp(ui = ui, server = server)
