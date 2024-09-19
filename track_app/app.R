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
library(openxlsx)
library(dendextend)

ui <- navbarPage(
  "Data Explorer and Statistical Modeling",
  
  # First tab for Merging Data
  tabPanel("Merge Data",
           sidebarLayout(
             sidebarPanel(
               fileInput("existing_file", 
                         label = HTML("Upload Existing Data File or<br>Upload ID Alignment to Start New"),
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
  
  # Second tab for Data exploring
  tabPanel("Data Explorer",
           sidebarLayout(
             sidebarPanel(
               
               fileInput("comb_file", "Upload Combined Data File", 
                         multiple = FALSE,
                         accept = c(".csv", ".txt", ".xls", ".xlsx")),
               
               # Input to select the column for the x-axis
               selectInput("x_column", "Select Column for X-Axis", choices = c(
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
                 selected = "NSN"),
               
               # Input to select the column for the y-axis (for some plots)
               selectInput("y_column", "Select Column for Y-Axis", choices = c(
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
                 selected = NULL),
               
               # Input to select the type of plot
               selectInput("plot_type", "Select Plot Type", 
                           choices = c("Box Plot" = "boxplot",
                                       "Histogram" = "histogram",
                                       "Scatter Plot" = "scatter")),
               
               # Input to select additional options (e.g., number of bins for histogram)
               conditionalPanel(
                 condition = "input.plot_type == 'histogram'",
                 numericInput("bins", "Number of Bins", value = 30, min = 1)
               ),
               
               actionButton("plot_btn", "Generate Plot")
             ),
             mainPanel(
               # Display the plot
               plotOutput("data_plot"),
               
             )
           )
  ),
  
  # Third tab for Statistical Modeling
  tabPanel("Statistical Modeling",
           sidebarLayout(
             sidebarPanel(
               fileInput("clean_file", "Upload Clean Data File", 
                         multiple = FALSE,
                         accept = c(".csv", ".txt", ".xls", ".xlsx")),
               
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
      stringsAsFactors = FALSE,
      check.names=FALSE
    )
    features <- names(combined_df)
    
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
          ids <- c("Local Id", "AGS ID", "AGS Student ID", "Student")
          data <- data %>%
            mutate(`AGS ID` = unlist(data[intersect(ids, names(data))]),
                   `Form class` = unlist(data[intersect("Form", names(data))]),
                   `Ethnicity 1` = unlist(data[intersect("Ethnicity 1", names(data))]),
                   `Ethnicity 2` = unlist(data[intersect("Ethnicity 2", names(data))]),
                   `Ethnicity 3` = unlist(data[intersect("Ethnicity 3", names(data))]),
                   `Stats Ethnicity` = unlist(data[intersect("Stats Ethnicity", names(data))]),
                   #`Level 1`
                   #`Level 2`
                   `Level 2 Credit Count` = unlist(select(data, starts_with("L2 Credits Achieved"))),
                   `L1 Lit` = unlist(data[intersect("L1 Literacy", names(data))]),
                   `L1 Num` = unlist(data[intersect("L1 Numeracy", names(data))]),
                   `UE Lit` = unlist(data[intersect("UE Literacy", names(data))]),
                   `UE Num` = unlist(data[intersect("UE Numeracy", names(data))]),
                   `UE Read` = unlist(data[intersect("UE Read", names(data))]),
                   `UE Write` = unlist(data[intersect("UE Write", names(data))]),
                   `UE Read / Write` = unlist(data[intersect("UE Read / Write", names(data))]),
                   #`L2 Externals % ABS`
                   #`L2 Externals % SNA`
                   #`L2 Externals % NA`
                   `AS 91098 (L2)` = unlist(data[intersect("AS 91098 (L2)", names(data))]),
                   `AS 91099 (L2)` = unlist(data[intersect("AS 91099 (L2)", names(data))]),
                   `AS 91100 (L2)` = unlist(data[intersect("AS 91100 (L2)", names(data))]),
                   `Total External` = unlist(data[intersect("Total External", names(data))]),
                   `AS 91101 (L2)` = unlist(data[intersect("AS 91101 (L2)", names(data))]),
                   `AS 91102 (L2)` = unlist(data[intersect("AS 91102 (L2)", names(data))]),
                   `AS 91106 (L2)` = unlist(data[intersect("AS 91106 (L2)", names(data))]),
                   `Total Internal` = unlist(data[intersect("Total Internal", names(data))]),
                   `TOTAL CREDITS` = unlist(data[intersect("TOTAL CREDITS", names(data))]),
                   `F6 School exam T1 Agg` = unlist(data[intersect("T1 Agg", names(data))]),
                   `F6 School exam T2 Agg` = unlist(data[intersect("T2 Agg", names(data))]),
                   `F6 School exam T3 Agg` = unlist(data[intersect("T3 Agg", names(data))]),
                   `F6 School exam Avg agg` = unlist(data[intersect("Avg Agg", names(data))]),
                   `F6 Term 1 Attn` = unlist(data[intersect("Atten T1 Agg", names(data))]),
                   `F6 Term 2 Attn` = unlist(data[intersect("Atten T2 Agg", names(data))]),
                   `F6 Term 3 Attn` = unlist(data[intersect("Atten T3 Agg", names(data))]),
                   `F6 Term 4 Attn` = unlist(data[intersect("Atten T4 Agg", names(data))]),
                   `F6 Year Avg` = unlist(data[intersect("Atten Year Agg", names(data))])
                   #`Does L3 CAS`
                   #`Does L3 CON`
                   #`F7 Term 1 Aggregate`
                   #`F7 Term 1 A&E D grades`	
                   #`2 May Term 1 Credit count`	
                   #`16 June Term 2 Credit Count`	
                   #`1 August Term 2 credit count`	
                   #`F7 Term 2 Aggregate`	
                   #`F7 Term 2 A&E D grades`	
            )
          # Ensure all columns are character type
          data <- mutate_all(data, as.character)
          #combined_df <- full_join(combined_df, data, by = c("NSN", "AGS.ID"))
          #combined_df <- merge(combined_df, data, by = c("NSN", "AGS.ID"), all = TRUE)
          #combined_df <- bind_rows(combined_df, data)
          #combined_df <- combined_df[, features]
          data <- data %>%
            select(any_of(names(combined_df)))
          id <- intersect(c("NSN", "AGS ID"), names(data))
          combined_df <- rows_update(combined_df, data, by = id, unmatched = "ignore")
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
          # Ensure all columns are character type
          data <- mutate_all(data, as.character)
          # Set column names based on selected feature
          if (nchar(data[1, 1]) == 9){
            colnames(data) <- c("NSN", feature)
          } else if (nchar(data[1, 1]) == 5){
            colnames(data) <- c("AGS ID", feature)
          }
          #combined_df <- bind_rows(combined_df, data)
          
          id <- intersect(c("NSN", "AGS ID"), names(data))
          combined_df <- rows_update(combined_df, data, by = id, unmatched = "ignore")
        }
      }
    }
    
    char_cols <- c(3, 7:9, 11:14, 21:23, 25:27, 42, 43)
    exclude_cols <- colnames(combined_df)[char_cols]
    combined_df <- combined_df %>%
      mutate(across(-all_of(exclude_cols), ~ as.numeric(.)),
             `F6 Term 2-Term 3 Attn drop` = `F6 Term 3 Attn` - `F6 Term 2 Attn`,
             `F6 School exam Drop mid-year` = `F6 School exam T3 Agg` - `F6 School exam T2 Agg`)
    combined_data(combined_df)
  })
  
  
  
  # Display combined data table
  output$combined_data_table <- DT::renderDataTable({
    req(combined_data())
    DT::datatable(combined_data())
  })
  
  color_header <- function(wb, font_color, header_color, col_start, col_end){
    header_style <- createStyle(
      fontSize = 11,                # Set font size to 11
      fontColour = font_color,         # Set font color to black
      fgFill = header_color,           # Header background color
      textRotation = 90,            # Rotate text to vertical
      halign = "center",            # Horizontal alignment
      valign = "center",            # Vertical alignment
      textDecoration = "bold"       # Make text bold
    )
    
    # Apply the style to the header row (row 1)
    addStyle(wb, "Master Data", style = header_style, rows = 1, cols = col_start:col_end, gridExpand = TRUE)
  }
  
  # Apply Conditional formatting
  create_excel_with_formatting <- function(data) {
    wb <- createWorkbook()
    addWorksheet(wb, "Master Data")
    writeData(wb, "Master Data", data)
    
    custom_header_colors <-rbind(
      c(1, 3, "darkseagreen1", "black"),  
      c(4, 7 , "gray49", "black"),  
      c(8, 17, "lightskyblue", "black"), 
      c(18, 20, "lightsalmon", "black"),
      c(21, 29, "lightgreen", "black"),
      c(30, 34, "purple4", "white"),
      c(35, 40, "goldenrod1", "black"),
      c(41, 41, "red", "white"),
      c(42, 43, "seagreen", "white"),
      c(44, 44, "purple4", "white"),
      c(45, 45, "violet", "black"),
      c(46, 46, "plum1", "black"),
      c(47, 49, "palegreen3", "black"),
      c(50, 50, "purple4", "white"),
      c(51, 51, "violet", "black"),
      c(52, 52, "plum1", "black"),
      c(53, 54, "red", "white"))
    
    for (i in (1:(nrow(custom_header_colors)))){
      color_header(wb, custom_header_colors[i,4], custom_header_colors[i,3], 
                   custom_header_colors[i,1], custom_header_colors[i,2])
    }
    
    
    
    # Conditional formatting for "Score" column
    conditionalFormatting(
      wb, "Master Data", cols = 2, rows = 2:(nrow(data) + 1), 
      rule = "<60", 
      style = createStyle(fontColour = "white", bgFill = "red")
    )
    
    # Conditional formatting for "Attendance" column
    conditionalFormatting(
      wb, "Master Data", cols = 3, rows = 2:(nrow(data) + 1), 
      rule = ">=80", 
      style = createStyle(fontColour = "white", bgFill = "green")
    )
    
    # Save the workbook to a temporary file
    tmp_file <- tempfile(fileext = ".xlsx")
    saveWorkbook(wb, file = tmp_file, overwrite = TRUE)
    tmp_file
  }
  
  # Download combined data
  output$download_combined <- downloadHandler(
    filename = function() {
      "combined_data.xlsx"
    },
    content = function(file) {
      # Create the Excel file with conditional formatting
      tmp_file <- create_excel_with_formatting(combined_data())
      file.copy(tmp_file, file)
    }
  )
  
  # Data Explorer Part
  observeEvent(input$plot_btn, {
    if (!is.null(input$comb_file)) {
      ext <- tools::file_ext(input$comb_file$name)
      comb_data <- if (ext == "csv" || ext == "txt") {
        read.csv(input$comb_file$datapath)
      } else if (ext == "xls" || ext == "xlsx") {
        read_excel(input$comb_file$datapath)
      } else {
        NULL
      }}
    
    xcol <- input$x_column
    ycol <- input$y_column
    plot_type <- input$plot_type
    
    # Create plot based on selected plot type
    plot_data <- comb_data
    
    output$data_plot <- renderPlot({
      ggplot(plot_data, aes(x = get(xcol), y = get(ycol))) +
        {
          if (plot_type == "boxplot") {
            geom_boxplot()
          } else if (plot_type == "histogram") {
            geom_histogram()
          } else if (plot_type == "scatter") {
            geom_point()
          }
        } +
        theme_minimal()
    })
  })
  
  # Clustering part
  clustering_results <- reactiveVal()
  observeEvent(input$run_clustering, {
    req(input$clean_file)
    
    # Read the clean data file
    if (!is.null(input$clean_file)) {
      ext <- tools::file_ext(input$clean_file$name)
      clean_data <- if (ext == "csv" || ext == "txt") {
        read.csv(input$clean_file$datapath)
      } else if (ext == "xls" || ext == "xlsx") {
        read_excel(input$clean_file$datapath)
      } else {
        NULL
      }}
    clean_data <- clean_data %>%
      mutate_all(~ replace(., is.na(.), 0))
    clean_data <- clean_data %>%
      mutate(across(where(is.character), ~ as.numeric(as.factor(.))))
    clean_data <- clean_data %>% select(NSN, `AGS ID`, `L1 Lit`, `L1 Num`, `UE Num`, 
                                        `UE Lit`, `UE Read`, `UE Write`, `UE Read / Write`,
                                        `TOTAL CREDITS`,
                                        `F6 School exam Drop mid-year`, `F6 Year Avg`,
                                        `F6 Term 2-Term 3 Attn drop`)
    # Perform clustering based on the selected method
    if (input$clustering_method == "K-means") {
      clusters <- kmeans(clean_data, centers = input$clusters)
      clustering_results(clusters)
      
      clean_data <- clean_data %>%
        mutate(Cluster = as.factor(clusters$cluster))
      # Plot the K-means clustering results
      output$cluster_plot <- renderPlot({
        ggplot(clean_data, aes(x = NSN, y = `AGS ID`, color = Cluster)) +
          geom_point() +
          labs(color = "Cluster") +
          theme_minimal()
      })
    } else if (input$clustering_method == "Hierarchical") {
      clusters <- hclust(dist(clean_data))
      clustering_results(clusters)
      
      # Plot the hierarchical clustering results
      output$cluster_plot <- renderPlot({
        dend <- as.dendrogram(clusters)
        plot(color_branches(dend, k = input$clusters))
      })
    }
  })
  
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
