################# SERVER #############################
server <- function(input, output, session) {

   pca_remove <- reactiveVal(NULL)
  
  required_files <- c("../data/pareto_genomes.txt", "../data/pareto_fitness.txt", "../data/hru.con", "../data/measure_location.csv")   # Example file names
  
  # Check if required files exist #####
  checkFiles <- reactive({sapply(required_files, function(file) file.exists(file))})
  
  # Output message about file status
   output$fileStatusMessage <- renderText({if (all(checkFiles())) {""} else {paste("The following file(s) are missing:", paste(required_files[!checkFiles()], collapse = ", "))}})
  
   
  observeEvent(input$run,{
    # write a new config.ini with selected variables and find the highest correlation
    write_corr(vars = input$selements)
    
    # Define the command to run the Python script
    py_script <- "../python_files/correlation_matrix.py"
    cmd <- paste("python", py_script)
    
    # Run the command and capture the output
    result <- system(cmd, intern = TRUE)
    
    
    # Correlation Plot (does not change for different thresholds)
    csv_file <- "../output/correlation_matrix.csv"
    validate(need(file.exists(csv_file), "CSV file not found."))
    corr = read.csv(csv_file, row.names = 1)
    output$corrplot <- renderPlot({plt_corr(corr)})
 
  # events tied to a change in threshold, however also tied to change in selected variables, therefore also observe run
  observeEvent(input$thresh,{
    # Highest correlation under selected threshold
    output$corrtable <- renderTable({find_high_corr(corr,threshold=input$thresh, tab=T)}) #tab = T means this returns the full table, =F is for pulling variables
   
    # Top Table with selected elements
    output$selements <- renderTable({input$selements},rownames = T,colnames = F)
    })
  
    # Drop Down Menu with subset of those with selected threshold
    observe({updateSelectInput(session, "excl",choices = find_high_corr(corr,threshold=input$thresh, tab=F))})
  })
  
  observeEvent(input$confirm_selection,{pca_remove(input$excl)

    # Display confirmed selection in the Correlation Analysis tab
  output$confirmed_selection <- renderText({
    paste("Removed variables: ", paste(pca_remove(),collapse = ", "))})
  
  output$pca_incl <- renderTable({
    all_var[-which(all_var%in%pca_remove())]
  },colnames = F)
  
  
  
  })
  
  observeEvent(input$runPCA,{
    # write a new config.ini with selected variables and find the highest correlation
    write_corr(pca_content = all_var[-which(all_var%in%pca_remove())])
    
    # Define the command to run the Python script
    pca_script <- "../python_files/kmeans.py"
    pcacmd <- paste("python", pca_script)
    
    # Run the command and capture the output
    result <- system(pcacmd, intern = TRUE)})
  
  
}


