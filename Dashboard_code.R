library(shiny)
library(ggplot2)
library(DT)
library(corrplot)
library(nortest)
library(psych)
library(FactoMineR)
library(factoextra)
library(rcompanion)
library(ppcor)


# Define UI for application
ui <- fluidPage(
  # Add custom CSS for background image
  tags$style(
    "
    h1 {
      font-family: 'Helvetica', sans-serif; /* Font for the main title */
      font-size: 40px; /* Font size for the title */
      color: black; /* Font color */
      text-align: center; /* Center the title */
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5); /* Add shadow for better readability */
    }
    "
  ),
  
  # Main title
  titlePanel(
    h1("COVID-19 Analysis Dashboard")),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      helpText("Please upload a standardized CSV file for analysis."),
      selectInput("variable", "Select Variable for the Plots", choices = NULL, multiple = FALSE),
      checkboxInput("all_in_one", "Display All Plots Together", FALSE),
      actionButton("update", "Update"),
      hr(),
      selectInput("dependent", "Select Dependent Variable", choices = NULL),
      selectizeInput("independent_vars", "Select Independent Variables", choices = NULL, multiple = TRUE),
      selectizeInput("control_vars", "Select Control Variables", choices = NULL, multiple = TRUE),
      selectizeInput("kmo_vars", "Select Variables for KMO Test", choices = NULL, multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("View Dataset", DTOutput("dataset_table")),  # Added tab
        tabPanel("Summary Statistics", verbatimTextOutput("summary_box")),
        tabPanel("Normality Results", verbatimTextOutput("normality_results")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("QQ Plot", plotOutput("qqplot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Correlation Matrix", plotOutput("correlation_matrix", height = "800px")),
        tabPanel("Partial Correlation", verbatimTextOutput("partial_correlation")),
        tabPanel("KMO Test", verbatimTextOutput("kmo_test")),
        tabPanel("Factor Analysis", verbatimTextOutput("factor_analysis")),
        tabPanel("PCA Results", plotOutput("scree_plot"), verbatimTextOutput("pca_results")),
        tabPanel("Linear Regression", verbatimTextOutput("linear_regression"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    data(df)
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "variable", choices = numeric_vars)
    updateSelectInput(session, "dependent", choices = numeric_vars)
    updateSelectizeInput(session, "independent_vars", choices = numeric_vars)
    updateSelectizeInput(session, "control_vars", choices = numeric_vars)
    updateSelectizeInput(session, "kmo_vars", choices = numeric_vars)
  })
  
  output$dataset_table <- renderDT({
    req(data())
    df <- data()
    datatable(df, options = list(pageLength = 500, scrollX = TRUE))
  })
  
  output$summary_box <- renderPrint({
    req(data())
    df <- data()
    summary(df)
  })
  
  output$normality_results <- renderPrint({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    results <- lapply(numeric_data, function(column) {
      pval <- ad.test(column)$p.value
      if (pval < 0.05) {
        return("Not Normal")
      } else {
        return("Normal")
      }
    })
    names(results) <- names(numeric_data)
    results
  })
  
  output$boxplot <- renderPlot({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    if (input$all_in_one) {
      num_vars <- ncol(numeric_data)
      plot_rows <- ceiling(sqrt(num_vars))  # Calculate rows dynamically
      plot_cols <- ceiling(num_vars / plot_rows)  # Calculate columns dynamically
      par(mfrow = c(plot_rows, plot_cols), mar = c(4, 4, 2, 1))  # Adjust margins
      for (var in names(numeric_data)) {
        boxplot(numeric_data[[var]], main = paste("Boxplot of", var), col = "lightblue")
      }
      par(mfrow = c(1, 1))  # Reset layout
    } else {
      ggplot(df, aes_string(y = input$variable)) +
        geom_boxplot(fill = "lightblue") +
        theme_minimal() +
        labs(y = input$variable, title = paste("Boxplot of", input$variable))
    }
  })
  
  output$qqplot <- renderPlot({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    if (input$all_in_one) {
      num_vars <- ncol(numeric_data)
      plot_rows <- ceiling(sqrt(num_vars))  # Calculate rows dynamically
      plot_cols <- ceiling(num_vars / plot_rows)  # Calculate columns dynamically
      par(mfrow = c(plot_rows, plot_cols), mar = c(4, 4, 2, 1))  # Adjust margins
      for (var in names(numeric_data)) {
        qqnorm(numeric_data[[var]], main = paste("QQ Plot of", var))
        qqline(numeric_data[[var]])
      }
      par(mfrow = c(1, 1))  # Reset layout
    } else {
      qqnorm(df[[input$variable]], main = paste("QQ Plot of", input$variable))
      qqline(df[[input$variable]])
    }
  })
  
  output$histogram <- renderPlot({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    
    if (input$all_in_one) {
      num_vars <- ncol(numeric_data)
      plot_rows <- ceiling(sqrt(num_vars))  # Calculate rows dynamically
      plot_cols <- ceiling(num_vars / plot_rows)  # Calculate columns dynamically
      par(mfrow = c(plot_rows, plot_cols), mar = c(4, 4, 2, 1))  # Adjust margins
      for (var in names(numeric_data)) {
        plotNormalHistogram(numeric_data[[var]], 
                            main = paste("Histogram of", var), 
                            col = "lightgreen", 
                            linecol = "red")
      }
      par(mfrow = c(1, 1))  # Reset layout
    } else {
      # Single variable case using plotNormalHistogram
      selected_var <- input$variable
      plotNormalHistogram(df[[selected_var]], 
                          main = paste("Histogram of", selected_var), 
                          col = "lightgreen", 
                          linecol = "red")
    }
  })
  
  output$correlation_matrix <- renderPlot({
    req(data(), input$dependent)
    df <- data()
    dependent_var <- input$dependent
    independent_vars <- setdiff(names(df)[sapply(df, is.numeric)], dependent_var)
    
    correlation_data <- df[, c(dependent_var, independent_vars)]
    correlation_matrix <- cor(correlation_data, use = "complete.obs")
    
    corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8)
  }, height = 800)
  
  output$kmo_test <- renderPrint({
    req(data(), input$kmo_vars)
    df <- data()
    selected_vars <- input$kmo_vars
    if (length(selected_vars) == 0) {
      return("Please select variables for the KMO test.")
    }
    numeric_data <- df[, selected_vars, drop = FALSE]
    kmo <- KMO(numeric_data)
    kmo
  })
  
  output$factor_analysis <- renderPrint({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    fa_result <- fa(numeric_data, nfactors = 2, rotate = "varimax")
    print(fa_result)
  })
  
  output$scree_plot <- renderPlot({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    pca_result <- PCA(numeric_data, graph = FALSE)
    fviz_eig(pca_result)
  })
  
  output$pca_results <- renderPrint({
    req(data())
    df <- data()
    numeric_data <- df[, sapply(df, is.numeric)]
    pca_result <- PCA(numeric_data, graph = FALSE)
    print(summary(pca_result))
  })
  
  output$linear_regression <- renderPrint({
    req(data(), input$dependent)
    df <- data()
    dependent_var <- input$dependent
    independent_vars <- input$independent_vars
    if (length(independent_vars) == 0) {
      independent_vars <- setdiff(names(df)[sapply(df, is.numeric)], dependent_var)
    }
    formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = "+")))
    lm_result <- lm(formula, data = df)
    summary(lm_result)
  })
  
  output$partial_correlation <- renderPrint({
    req(data(), input$dependent, input$independent_vars, input$control_vars)
    df <- data()
    dependent_var <- input$dependent
    independent_vars <- input$independent_vars
    control_vars <- input$control_vars
    
    pcorr_result <- pcor.test(df[, dependent_var], df[, independent_vars], df[, control_vars])
    pcorr_result
  })
}

# Run the application
shinyApp(ui = ui, server = server)
