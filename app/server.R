################# SERVER #############################
server <- function(input, output, session) {

  required_files <- c("../data/pareto_genomes.txt", "../data/pareto_fitness.txt", "../data/hru.con", "../data/measure_location.csv")   # Example file names
  
  # Check if required files exist
  checkFiles <- reactive({
    sapply(required_files, function(file) file.exists(file))
  })
  
  # Output message about file status
  
  output$fileStatusMessage <- renderText({
    if (all(checkFiles())) {
      ""
    } else {
      paste("The following file(s) are missing:", paste(required_files[!checkFiles()], collapse = ", "))
    }
  })
  
  # write a new config.ini with selected variables and find the highest correlation
  observeEvent(input$run,{
    write_corr(vars = input$selements)
  })
  
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
    
  
    corr = read.csv(csv_file, row.names = 1)
    
    # Display the CSV content in the Shiny app
    output$corrplot <- renderPlot({
      plt_corr(corr)
    })
    
    # Highest correlation under selected threshold
    output$corrtable <- renderTable({
     
      find_high_corr(corr,threshold=input$thresh, tab=T) #tab = T means this returns the full table, =F is for pulling variables
    })
    
    output$selements <- renderTable({
      input$selements
      },rownames = T,colnames = F)
    })
  
   output$excla <- renderText({
    input$excl
  })
 
   
   observe({
     updateSelectInput(session, "excl",
                       choices = find_high_corr(corr,threshold=input$thresh, tab=F)
                       )
   })
}


