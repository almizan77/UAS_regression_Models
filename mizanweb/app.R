library(shiny)
library(readr)
library(readxl)
library(DT)
library(ggplot2)
library(reshape2)
library(dplyr)
library(broom)

ui <- fluidPage(
  titlePanel("Analisis Data, Model Regresi, dan Prediksi oleh Almizan Kocak"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Unggah Dataset Utama (.csv / .xlsx)",
                accept = c(".csv", ".xlsx")),
      tags$hr(),
      helpText("Gunakan dataset ini untuk eksplorasi dan pembuatan model."),
      
      conditionalPanel(
        condition = "output.fileUploaded == true",
        
        h4("Scatter Plot"),
        selectInput("x_var", "X Axis:", choices = NULL),
        selectInput("y_var", "Y Axis:", choices = NULL),
        selectInput("color_var", "Warna Berdasarkan:", choices = NULL),
        
        tags$hr(),
        h4("Model Regresi"),
        selectInput("reg_y", "Y (Target):", choices = NULL),
        selectInput("reg_x", "X (Prediktor):", choices = NULL),
        
        tags$hr(),
        h4("Prediksi Data Baru"),
        fileInput("newdata", "Unggah Dataset Baru untuk Prediksi",
                  accept = c(".csv", ".xlsx")),
        downloadButton("download_prediction", "Unduh Hasil Prediksi")
      )
    ),
    
    mainPanel(
      h4("Pratinjau Data"),
      DTOutput("preview_table"),
      tags$hr(),
      
      h4("Struktur Kolom"),
      tableOutput("column_info"),
      tags$hr(),
      
      h4("Heatmap Korelasi"),
      plotOutput("correlation_heatmap"),
      tags$hr(),
      
      h4("Scatter Plot (Gradasi Warna Berdasarkan Target)"),
      plotOutput("scatter_plot"),
      tags$hr(),
      
      h4("Hasil Regresi Linier"),
      verbatimTextOutput("model_summary"),
      tableOutput("model_metrics"),
      plotOutput("regression_plot"),
      tags$hr(),
      
      h4("Hasil Prediksi dari Dataset Baru"),
      DTOutput("prediction_table")
    )
  )
)

server <- function(input, output, session) {
  
  # DATASET UTAMA
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
                 csv = read_csv(input$file$datapath),
                 xlsx = read_excel(input$file$datapath),
                 validate("Format file tidak didukung"))
    return(df)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  observe({
    req(data())
    num_cols <- names(data())[sapply(data(), is.numeric)]
    
    updateSelectInput(session, "x_var", choices = num_cols)
    updateSelectInput(session, "y_var", choices = num_cols)
    updateSelectInput(session, "color_var", choices = num_cols)
    
    updateSelectInput(session, "reg_x", choices = num_cols)
    updateSelectInput(session, "reg_y", choices = num_cols)
  })
  
  output$preview_table <- renderDT({
    req(data())
    datatable(head(data(), 10), options = list(scrollX = TRUE))
  })
  
  output$column_info <- renderTable({
    req(data())
    data.frame(
      Nama_Kolom = names(data()),
      Tipe_Data = sapply(data(), class)
    )
  })
  
  output$correlation_heatmap <- renderPlot({
    req(data())
    df <- data() %>% select(where(is.numeric)) %>% na.omit()
    if (ncol(df) < 2) {
      plot.new()
      title("Tidak cukup kolom numerik")
      return()
    }
    corr_matrix <- cor(df)
    melted_corr <- melt(corr_matrix)
    ggplot(data = melted_corr, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Korelasi") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_fixed()
  })
  
  output$scatter_plot <- renderPlot({
    req(data(), input$x_var, input$y_var, input$color_var)
    ggplot(data(), aes_string(x = input$x_var, y = input$y_var, color = input$color_var)) +
      geom_point(size = 2, alpha = 0.8) +
      scale_color_gradient(low = "yellow", high = "red") +
      theme_minimal()
  })
  
  model <- reactive({
    req(data(), input$reg_x, input$reg_y)
    df <- data() %>% select(all_of(c(input$reg_y, input$reg_x))) %>% na.omit()
    lm(as.formula(paste(input$reg_y, "~", input$reg_x)), data = df)
  })
  
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$model_metrics <- renderTable({
    req(model())
    glance(model())
  })
  
  output$regression_plot <- renderPlot({
    req(model())
    df <- model()$model
    df$pred <- predict(model())
    
    ggplot(df, aes_string(x = input$reg_y, y = "pred")) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(x = "Actual", y = "Predicted") +
      theme_minimal()
  })
  
  # PREDIKSI DATA BARU
  new_data <- reactive({
    req(input$newdata)
    ext <- tools::file_ext(input$newdata$name)
    df <- switch(ext,
                 csv = read_csv(input$newdata$datapath),
                 xlsx = read_excel(input$newdata$datapath),
                 validate("Format file tidak didukung"))
    return(df)
  })
  
  prediction_result <- reactive({
    req(model(), new_data(), input$reg_x)
    df <- new_data()
    if (!(input$reg_x %in% names(df))) {
      validate("Kolom prediktor tidak ditemukan dalam data baru")
    }
    df$Predicted_Y <- predict(model(), newdata = df)
    return(df)
  })
  
  output$prediction_table <- renderDT({
    req(prediction_result())
    datatable(prediction_result(), options = list(scrollX = TRUE))
  })
  
  output$download_prediction <- downloadHandler(
    filename = function() {
      paste0("prediksi_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(prediction_result(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
