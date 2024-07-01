################# SERVER #############################
server <- function(input, output) {
  observeEvent(input$run, {
    # Define the command to run the Python script
    py_script <- "../python_files/correlation_matrix.py"
    cmd <- paste("python", py_script)
    
    # Run the command and capture the output
    result <- system(cmd, intern = TRUE)
    
    # Read the CSV file
    csv_file <- "../output/correlation_matrix.csv"
    
    validate(
      need(file.exists(csv_file), "CSV file not found.")
    )
    
    corr <- read.csv(csv_file, row.names = 1)
    
    # Display the CSV content in the Shiny app
    output$corrplot <- renderPlot({
      plt_corr(corr)
    })
    
    output$corrtable <- renderTable({
      find_high_corr(corr)
    })
    
    
  })
}


