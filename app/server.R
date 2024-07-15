################# SERVER #############################
server <- function(input, output, session) {

  pca_remove <- reactiveVal(NULL)
  script_output <- reactiveVal("") # data prep R output
  dp_done = reactiveVal(FALSE) # checking if data prep R output is done
  all_choices = reactiveVal()
  isElementVisible = reactiveVal(FALSE)
  
  ## pca tab - pca table
  pca_ini <- read_pca()
  
  pca_table <- reactiveVal(pca_ini)
  
  output$pca_incl <- renderTable({
    pca_table()
  }, rownames = T, colnames = F)
  
  pca_status <- reactiveVal("")
  
  axiselected = reactiveVal(read_config_plt(obj=F,axis=T))
  
  ### Data Prep ####
  required_files <- c("../data/pareto_genomes.txt", "../data/pareto_fitness.txt", "../data/hru.con", "../data/measure_location.csv")
  
  file_data1 <- reactiveVal(NULL)
  file_data2 <- reactiveVal(NULL)
  file_data3 <- reactiveVal(NULL)
  file_data4 <- reactiveVal(NULL)
  
  # Check if required files exist 
  observeEvent(input$file1, { file <- input$file1
    if (is.null(file)) {return(NULL)}
     file_data1(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file2, { file <- input$file2
    if (is.null(file)) {return(NULL)}
    file_data2(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file3, { file <- input$file3
    if (is.null(file)) {return(NULL)}
    file_data3(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file4, {file <- input$file4
    if (is.null(file)) {return(NULL)}
    file_data4(list(path = file$datapath, name = file$name))})
  
  
  observeEvent(input$files_avail,{
    # optional_inputs <- c(input$file1, input$file2, input$file3, input$file4)
    # any_empty <- any(sapply(optional_inputs, function(x) x == ""))
    # 
    # if (!any_empty)
      
     req(file_data1(),file_data2(),file_data3(),file_data4())
    
     save_dir <- "../data/"
     save_filename1 <- file_data1()$name
     save_filename2 <- file_data2()$name
     save_filename3 <- file_data3()$name
     save_filename4 <- file_data4()$name
     save_path1 <- file.path(save_dir, save_filename1)
     save_path2 <- file.path(save_dir, save_filename2)
     save_path3 <- file.path(save_dir, save_filename3)
     save_path4 <- file.path(save_dir, save_filename4)
     
     file.copy(file_data1()$path, save_path1, overwrite = TRUE)
     file.copy(file_data2()$path, save_path2, overwrite = TRUE)
     file.copy(file_data3()$path, save_path3, overwrite = TRUE)
     file.copy(file_data4()$path, save_path4, overwrite = TRUE)
     
     checkFiles <- reactive({sapply(required_files, function(file) file.exists(file))})
     
     output$fileStatusMessage <- renderText({if (all(checkFiles())) {HTML(paste("All Files found.", 
    "Please provide the names of the objectives represented in the Pareto front. The names and the order in which they are given have 
    to align with what is provided in the first four columns of pareto_fitness.txt", sep="<br/><br/>"))} else {paste("The following file(s) are missing:", paste(required_files[!checkFiles()], collapse = ", "))}})
     
     
     mes = read.csv("../data/measure_location.csv")
     mes = unique(mes$nswrm)
     nm = length(mes)
  })
  
  
   # initialise PCA table when app starts
   pca_in <- reactiveValues(data = read_pca()) #this only reads config$columns, NULL if opening for the first time
   
   objs <- reactiveValues(data=NULL)
   
   # only show objective naming when files have been checked
   observeEvent(input$files_avail, {shinyjs::show(id="sel_obj")})
   
   # Observe event for confirm button
   observeEvent(input$obj_conf, {
     shinyjs::show(id="range_title")
     objs$data<- data.frame(Objective = c(input$col1, input$col2, input$col3, input$col4), stringsAsFactors = FALSE )
     
     objs$objectives <- c(input$col1, input$col2, input$col3, input$col4)
     
     assigned_objnames = objs$objectives
     saveRDS(assigned_objnames,file="../input/object_names.RDS") #for later use in background script
     
     output$obj_conf <- renderTable({
       rng = get_obj_range(colnames = objs$objectives)
       bng = rng
       
       for(i in 1:4){
         for(j in 2:3){
          bng[i,j] = formatC(rng[i,j],digits= unlist(lapply(rng[,2],num.decimals))[i],drop0trailing = T,format="f")#same decimal for min and max
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
          tags$strong("The data preparation was successful. You can now continue with the Correlation and Principal Component Analysis. You will not need this tab again.")
        }else{verbatimTextOutput("rscriptcmd")}
        
        })
      output$rscriptcmd <- renderText({
        paste(script_output(), collapse = "\n")
      })
      
  ### Correlation Analysis ####
   
  observeEvent(input$run,{
    all_var <<- readRDS("../input/all_var.RDS")
    
    write_corr(vars = input$selements,cor_analysis = T, pca = F)
    
    # Define the command to run the Python script
    py_script <- "../python_files/correlation_matrix.py"
    cmd <- paste("python", py_script)
    
    # Run the command and capture the output
    result <- system(cmd, intern = TRUE)
    
    
  # Correlation Plot (does not change for different thresholds)
    csv_file <- "../output/correlation_matrix.csv"
    validate(need(file.exists(csv_file), "CSV file not found."))
    corr <<- read.csv(csv_file, row.names = 1) #global because of re-rendering of plot
    output$corrplot <- renderPlot({plt_corr(corr)})
   
  # events tied to a change in threshold, however also tied to change in selected variables, therefore also observe run
  observeEvent(input$thresh,{
    
    # reprint highest correlation table marking removed 
    output$corrtable <- renderDT({find_high_corr(corr,threshold=input$thresh, tab=T, strike=NULL)}) #tab = T means this returns the full table, =F is for pulling variables
   
    # Top Table with selected elements
    output$selements <- renderTable({input$selements},rownames = T,colnames = F)
    })
  
    # Drop Down Menu with subset of those with selected threshold
    observe({updateSelectInput(session, "excl",choices = find_high_corr(corr,threshold=input$thresh, tab=F))})
  })
  
  # on clicking confirm selection the config ini is updated
  observeEvent(input$confirm_selection,{
    pca_remove(input$excl)

    output$corrtable <- renderDT({
      datatable(find_high_corr(corr,threshold = input$thresh,tab = T,strike = input$excl),escape = FALSE)}) #tab = T means this returns the full table, =F is for pulling variables
    
    pca_content = all_var[-which(all_var %in% pca_remove())]
    saveRDS(pca_content,file = "../input/pca_content.RDS") #required for PCA
    
    pca_in$data = pca_content
    
    write_corr(pca_content = pca_in$data,
               pca = T,
               cor_analysis = F)#this is also called into the pca tab on startup
    
    nonoval = paste(pca_remove(), collapse = ", ")
  # Display confirmed selection in the Correlation Analysis tab
   output$confirmed_selection <- renderText({HTML(paste0("Removed variables: ","<b>", nonoval,"</b>"))})
  

  
  ### PC Analysis ####
  # table with variables INCLUDED in PCA (renewed every time confirm selection is clicked in correlation tab)
    pca_table(pca_in$data)

  })
  # Reactive values to store selected choices
  selections <- reactiveValues(
    element1 = NULL,
    element2 = NULL,
    element3 = NULL,
    element4 = NULL
  )
 
  observeEvent(input$tabs == "pca",{ 
    if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
    
    choices = c("off", choices)
    all_choices(choices)
    preselected = read_config_plt(obj = T, axis = F)
    axiselected(read_config_plt(obj = F, axis = T))
    
    
    updateTextInput(session, "axisx",  value  = axiselected()[1])
    updateTextInput(session, "axisy", value = axiselected()[2])
    updateTextInput(session, "colour", value = axiselected()[3])
    updateTextInput(session, "size", value = axiselected()[4])
    
    
    updateSelectInput(session, "element1", choices = choices, selected = preselected[1])
    updateSelectInput(session, "element2", choices = choices, selected = preselected[2])
    updateSelectInput(session, "element3", choices = choices, selected = preselected[3])
    updateSelectInput(session, "element4", choices = choices, selected = preselected[4])
    
    
      
    
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
      write_pca_ini(var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4,
                    var1_lab=input$axisx,var2_lab=input$axisy,var3_lab=input$colour,var4_lab=input$size)
      write_quali_ini(var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4)
    }
  
  output$selected_elements <- renderText({
    
    if (empty_count >= 1) {
      "Please make selections for all four elements - the analysis currently does not support less than four objectives."
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
      write_pca_ini(var1=input$element1,var2=input$element2,var3=input$element3,var4=input$element4,
                    var1_lab=input$axisx,var2_lab=input$axisy,var3_lab=input$colour,var4_lab=input$size)
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
##
 
  
  
  observeEvent(input$runPCA,{
    pca_content <<- readRDS("../input/pca_content.RDS")
    
    output$pca_mess <- renderUI({
      tags$p("If all data was provided in the right format, the PCA outputs will open in separate windows - you can discard or save them as necessary.")
      })
    
    isElementVisible(TRUE)
    
    ## Prepare config.ini
    write_corr(pca_content = pca_content,pca=T, cor_analysis = F)# columns
    
    # Define the command to run the Python script
    if(input$pcamethod=="k-means"){pca_script <- "../python_files/kmeans.py"}else{pca_script <- "../python_files/kmedoid.py"}
    
    run_python_script(path_script=pca_script,pca_status)
    
    }, once = TRUE)
  
   # python status
   output$pca_status <- renderText({pca_status()})
  
  
  # cluster specs
  observeEvent(input$write_clust, {
    fixbool = ifelse(input$clusyn == "No", "true", "false")
    if (input$clusyn == "No") {
      write_cluster(fixed_clusters = input$clus_fix,fixed_cluster_boolean = fixbool)
      } else{
      write_cluster(min_cluster = input$clus_min,max_cluster = input$clus_max,fixed_cluster_boolean = fixbool)
     }
  })
  # outlier specs
  observeEvent(input$write_outl, {
    outlbool = ifelse(input$outlyn == "No","false","true")
    if(input$outlyn == "Yes"){
      write_outl(handle_outliers_boolean=outlbool,deviations_min=input$sd_min,deviations_max=input$sd_max, 
                 count_min=input$count_min,count_max=input$count_max,outlier_to_cluster_ratio=input$outlier_ratio )
    }else{
      write_outl(handle_outliers_boolean=outlbool)#bool is turning on all others, if false all others are ignored/default value works
    }
    
  })
  
  # Pass the reactive value to output for use in conditionalPanel
  output$isElementVisible <- reactive({
    isElementVisible()
  })
  
  # Required for conditionalPanel to work with reactive output
  outputOptions(output, "isElementVisible", suspendWhenHidden = FALSE)
  
  
  # pca min/max specs
  observeEvent(input$pcaminmax,{
    write_pcanum(pcamin=input$pca_min,pcamax=input$pca_max)
  })
}


