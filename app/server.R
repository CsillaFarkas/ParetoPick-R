################# SERVER #############################
server <- function(input, output, session) {

  pca_remove <- reactiveVal(NULL)
  script_output <- reactiveVal("") # data prep R output
  dp_done = reactiveVal(FALSE) # checking if data prep R output is done
  all_choices = reactiveVal()
  isElementVisible = reactiveVal(FALSE)
  ### Data Prep ###
  # Check if required files exist #####
  observeEvent(input$files_avail,{
     
     checkFiles <- reactive({sapply(required_files, function(file) file.exists(file))})
     
     output$fileStatusMessage <- renderText({if (all(checkFiles())) {HTML(paste("All Files found.", 
    "Please provide the names of the objectives represented in the Pareto front. The names and the order in which they are given have 
    to align with what is provided in the first four columns of pareto_fitness.txt", sep="<br/><br/>"))} else {paste("The following file(s) are missing:", paste(required_files[!checkFiles()], collapse = ", "))}})
  })
  
  
   # initialise PCA table when app starts
   pca_in <- reactiveValues(data = read_pca()) #this only reads config$columns, NULL if opening for the first time
   
   objs <- reactiveValues(data=NULL)
   
   # only show objective naming when files have been checked
   observeEvent(input$files_avail, {shinyjs::show(id="sel_obj")})
   
   # Observe event for confirm button
   observeEvent(input$obj_conf, {
     objs$data<- data.frame(Objective = c(input$col1, input$col2, input$col3, input$col4), stringsAsFactors = FALSE )
     
     objs$objectives <- c(input$col1, input$col2, input$col3, input$col4)
     
     saveRDS(assigned_objnames,file="../input/object_names.RDS") #for later use in background script
     
     output$obj_conf <- renderTable({
       rng = get_obj_range(colnames = objs$objectives)
       bng = rng
       
       for(i in 1:4){
         for(j in 2:3){
          bng[i,j] = formatC(rng[i,j],digits= unlist(lapply(rng[,2],num.decimals))[i])#same decimal for min and max
         }
       }
       bng
       
       },rownames = T)})
   

   # Data Prep with external script
    
     observeEvent(input$runprep,{
       
       dp_done(FALSE)
       script_output(character()) #clear old output
       
       optain <- process$new("Rscript",c("../convert_optain.R"),stdout = "|", stderr = NULL) #stdout | ---> pipe output, stderr ---> ignore
       autoInvalidate <- reactiveTimer(100)
       
       observe({autoInvalidate()
         if (optain$is_alive()) {
           new_output <- optain$read_output_lines()
           if (length(new_output) > 0) {
             current_output <- script_output()
             
             # Append new output lines and limit to last 10 lines
             updated_output <- c(current_output, new_output)
             if (length(updated_output) > 10) {
               updated_output <- tail(updated_output, 10)
             }
             # update the reactive value
             script_output(updated_output)
           }
         }else{
           final_output <- optain$read_output_lines()
           if (length(final_output) > 0) {
             current_output <- script_output()
             updated_output <- c(current_output, final_output)
             if (length(updated_output) > 10) {
               updated_output <- tail(updated_output, 10)
             }
             script_output(updated_output)
           }
           dp_done(TRUE)
         }
       })
     })
     
      output$script_output  <- renderUI({
        if(dp_done()){
          tags$p("The data preparation was successful. You can now continue with the Correlation and Principal Component Analysis. You will not need this tab again.")
        }else{verbatimTextOutput("rscriptcmd")}
        
        })
      output$rscriptcmd <- renderText({
        paste(script_output(), collapse = "\n")
      })
      
    ### Correlation Analysis ###
   
  observeEvent(input$run,{
 
    write_corr(vars = input$selements,cor_analysis = T, pca = F)
    
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
  
  # on clicking confirm selection the config ini is updated
  observeEvent(input$confirm_selection,{
    pca_remove(input$excl)

    pca_in$data = all_var[-which(all_var%in%pca_remove())]
    
    write_corr(pca_content = pca_in$data,pca = T,cor_analysis = F)#startup this is also called into the pca tab
    
    nonoval = paste(pca_remove(),collapse = ", ")
  # Display confirmed selection in the Correlation Analysis tab
   output$confirmed_selection <- renderText({HTML(paste0("Removed variables: ","<b>", nonoval,"</b>"))})
  
  })
  
  # table with variables INCLUDED in PCA (renewed every time confirm selection is clicked in correlation tab)
  output$pca_incl <- renderTable({paste(pca_in$data)},rownames=T,colnames = F)
  # Reactive values to store selected choices
  selections <- reactiveValues(
    element1 = NULL,
    element2 = NULL,
    element3 = NULL,
    element4 = NULL
  )
 
  observe({ 
    if(input$tabs == "pca"){
      
    choices = readRDS("../input/object_names.RDS")
    choices = c("off",choices)
    all_choices(choices)
    preselected = read_config_plt(config,obj=T,axis=F)
    axiselected = read_config_plt(config,obj=F,axis=T)

    
    updateTextInput(session, "axisx",  value  = axiselected[1])
    updateTextInput(session, "axisy", value = axiselected[2])
    updateTextInput(session, "colour", value = axiselected[3])
    updateTextInput(session, "size", value = axiselected[4])
    
    
    updateSelectInput(session, "element1", choices = choices, selected = preselected[1])
    updateSelectInput(session, "element2", choices = choices, selected = preselected[2])
    updateSelectInput(session, "element3", choices = choices, selected = preselected[3])
    updateSelectInput(session, "element4", choices = choices, selected = preselected[4])
    
    
      
    }
      })
  
 
  
  observe({
    req(all_choices())
    
    # Get the current selections
    selected1 <- input$element1
    selected2 <- input$element2
    selected3 <- input$element3
    selected4 <- input$element4
    
    # Determine the available choices for each dropdown
    choices1 <- setdiff(all_choices(), c(selected2, selected3, selected4))
    choices2 <- setdiff(all_choices(), c(selected1, selected3, selected4))
    choices3 <- setdiff(all_choices(), c(selected1, selected2, selected4))
    choices4 <- setdiff(all_choices(), c(selected1, selected2, selected3))
    
    # Update the choices for each dropdown, maintaining the current selection if it's valid
    updateSelectInput(session, "element1", choices = choices1, selected = selected1)
    updateSelectInput(session, "element2", choices = choices2, selected = selected2)
    updateSelectInput(session, "element3", choices = choices3, selected = selected3)
    updateSelectInput(session, "element4", choices = choices4, selected = selected4)
    
    selected1_2 <- input$axisx
    selected2_2 <- input$axisy
    selected3_2 <- input$colour
    selected4_2 <- input$size
    
    # Determine the available choices for each dropdown in the second set
    choices1_2 <- setdiff(all_choices(), c(selected2_2, selected3_2, selected4_2))
    choices2_2 <- setdiff(all_choices(), c(selected1_2, selected3_2, selected4_2))
    choices3_2 <- setdiff(all_choices(), c(selected1_2, selected2_2, selected4_2))
    choices4_2 <- setdiff(all_choices(), c(selected1_2, selected2_2, selected3_2))
    
    # Update the choices for each dropdown in the second set, maintaining the current selection if it's valid
    updateTextInput(session, "axisx", value = selected1_2)
    updateTextInput(session, "axisy", value = selected2_2)
    updateTextInput(session, "colour",  value = selected3_2)
    updateTextInput(session, "size",  value = selected4_2)
  })
  
  observeEvent(input$set_choices,{
    empty_count <- sum(input$element1 == "off", input$element2 == "off", input$element3 == "off", input$element4 == "off")
    if (empty_count < 2){
      write_pca_ini(config,input$element1,input$element2,input$element3,input$element4)
    }
  
  output$selected_elements <- renderText({
    
    if (empty_count >= 2) {
      "Please make selections for at least three elements."
    } else {
      HTML(paste("X Axis: ", ifelse(input$element1 == "off", "No selection", input$element1),
            "<br/>Y Axis: ", ifelse(input$element2 == "off", "No selection", input$element2),
            "<br/>Colour: ", ifelse(input$element3 == "off", "No selection", input$element3),
            "<br/>Size: ", ifelse(input$element4 == "off", "No selection", input$element4)))
    }
  })
  
  })
  
  observeEvent(input$confirm_axis,{
    empty_count2 <- sum(input$axisx == "", input$axisy == "", input$colour == "", input$size == "")
    if (empty_count2 < 2){
      write_axis_ini(config,input$axisx,input$axisy,input$colour,input$size)
    }
    
    output$axis_text <- renderText({
      
      if (empty_count2 >= 2) {
        "Please make selections for at least three elements."
      } else {
        HTML(paste("X Axis: ", ifelse(input$element1 == "", "No selection", input$axisx),
                   "<br/>Y Axis: ", ifelse(input$element2 == "", "No selection", input$axisy),
                   "<br/>Colour: ", ifelse(input$element3 == "", "No selection", input$colour),
                   "<br/>Size: ", ifelse(input$element4 == "", "No selection", input$size)))
      }
    })
  })

 
  
  
  observeEvent(input$runPCA,{
    
    output$pca_mess <- renderUI({
      tags$p("If all data was provided in the right format, the PCA outputs will open in separate windows - you can discard or save them as necessary.")
      
    })
    isElementVisible(TRUE)
    
    ## Prepare config.ini
    write_corr(pca_content = all_var[-which(all_var%in%pca_remove())],pca=T, cor_analysis = F)# columns
    
    # Define the command to run the Python script
    if(input$pcamethod=="k-means"){pca_script <- "../python_files/kmeans.py"}else{pca_script <- "../python_files/kmedoid.py"}
    
    pcacmd <- paste("python", pca_script)
    
    # Run the command and capture the output
    result <- system(pcacmd, intern = TRUE)
    
    # Display the output
    output$pca_status <- renderText({
      paste(result, collapse = "\n")})
    
    })
  
  # Pass the reactive value to output for use in conditionalPanel
  output$isElementVisible <- reactive({
    isElementVisible()
  })
  
  # Required for conditionalPanel to work with reactive output
  outputOptions(output, "isElementVisible", suspendWhenHidden = FALSE)
}


