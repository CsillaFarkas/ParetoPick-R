######################## SERVER ####################################
# comments: 
# Project: Clustering Pareto Solutions/Multi-objective visualisation
####################################################################
server <- function(input, output, session) {
  
  ## reactive values
  rv <-reactiveValues(sizes= NULL,colls = NULL) #color for parallel axes
  
  objectives <- reactiveVal(character())
  par_fiti <- reactiveVal(NULL)
  fit <- reactiveVal(NULL)
  pca_remove <- reactiveVal(NULL)
  script_output <- reactiveVal("") # data prep R output
  dp_done = reactiveVal(FALSE) # checking if data prep R output is done
  all_choices = reactiveVal()
  isElementVisible = reactiveVal(FALSE)
  
  settings_text= reactiveVal("") #printing pca settings
  update_settings <- function() {
    settings <- pca_settings(input)
    settings_text(settings)
  }
  pca_ini <- read_pca()
  pca_table <- reactiveVal(pca_ini)
  
  #empty pca table
  output$pca_incl <- renderTable({pca_table()}, rownames = T, colnames = F)
  
  pca_status <- reactiveVal("")
  
  axiselected = reactiveVal(read_config_plt(obj=F,axis=T))
  max_pca <- reactive({get_num_pca()})# required for max pc field
  
  #results table
  sols <- reactiveVal()
  
  #catchment shape
  cm <- reactiveVal()
  needs_buffer <- reactiveVal()
  
  ### Introduction ####
 
  ### Play Around Tab ####
  
  ##check if names of objectives have to be supplied or already present
  observeEvent(input$tabs == "play_around",{ 
    if(!file.exists("../input/object_names.RDS")) {
      
      shinyjs::show(id = "obj_first")
      
      #get new objective names
      observe({
        req(input$short1, input$short2, input$short3, input$short4)
        updated_objectives <<- c(input$short1, input$short2, input$short3, input$short4)
        objectives(updated_objectives)
      })
      
      observeEvent(input$save_par_fiti, {
        saveRDS(updated_objectives, file = "../input/object_names.RDS")
      }) 
      
    } else{
      shinyjs::hide(id="obj_first")
      short = readRDS("../input/object_names.RDS")
      objectives(short)
    }
    
    if(file.exists("../data/pareto_fitness.txt")){shinyjs::hide(id="parfit")}
      
    })
    
    
  ## update slider labels based on objectives
  observe({
    req(objectives())
    obj <- objectives()
    updateSliderInput(session, "obj1", label = obj[1])
    updateSliderInput(session, "obj2", label = obj[2])
    updateSliderInput(session, "obj3", label = obj[3])
    updateSliderInput(session, "obj4", label = obj[4])
    updateCheckboxGroupInput(session, "sel_neg", choices = objectives())
  })
  
  
  
  if (file.exists(pareto_path)) {
    observe({
      req(objectives())
      
      data <- read.table( pareto_path, header = FALSE, stringsAsFactors = FALSE,sep = ',')
      new_col_data <- objectives()
      colnames(data) = new_col_data
      fit(data)
      shinyjs::hide(id = "parfit")
      output$uploaded_pareto <- renderText({"All Files found."})
    })
  } else {
    shinyjs::show(id = "parfit")
  }
   
  ## user supplies pareto_fitness.txt
  observeEvent(input$par_fit, { 
    req(input$par_fit)
    file <- input$par_fit
    if (is.null(file)) {return(NULL)}
    par_fiti(list(path = file$datapath, name = file$name))
    })
  
  
  observeEvent(input$save_paretofit,{
    req(par_fiti(),objectives())
    save_par_fiti <- par_fiti()$name
    save_path_par_fiti <- file.path(save_dir, save_par_fiti)
    file.copy(par_fiti()$path, save_path_par_fiti, overwrite = TRUE)
    
    data = read.table(pareto_path, header=F,stringsAsFactors=FALSE,sep = ',')
    names(data) = objectives()
    fit(data)
    
  })

  
    ## base dataframe for all further operations in play_around tab
  f_scaled <- reactive({
    req(fit())  
    fit() %>% mutate(across(everything(), ~ scales::rescale(.)))%>%mutate(id = row_number())
   
  })
  
  ## ggplot melt and change plotting order
  pp <- reactive({
    req(f_scaled(),objectives())
    f_scaled()%>% pivot_longer(.,cols=-id)%>%mutate(name=factor(name,levels=objectives()),id=as.factor(id))
     })

  observe({
    req(pp())
    rv$sizes= rep(0.5, length(unique(pp()$id)))
    rv$colls = rep("grey50", length(unique(pp()$id)))
  })
 
  ## pull values from parallel axis line when clicked
  observeEvent(input$clickline,{
    req(fit(), objectives())
    x = round(input$clickline$x)
    val = input$clickline$y

    sc= match_scaled(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                     maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]), scal_tab=f_scaled(),allobs=objectives())

    # pull the closest value
    yo= sc%>% mutate(id = row_number())%>%slice(which.min(abs(.[[x]] - val)))

    rom = as.numeric(yo[["id"]])
  
    # change size vector
    rv$sizes[rom] = 1.3
    rv$colls[rom] = "#FF5666"# "#797596"
  
    # reset other sizes and colors
    rv$sizes[-rom] = 0.5
    rv$colls[-rom] = "grey50"
  
    te  <- find_row(dat = f_scaled(),  colname = objectives()[[x]], val = val,absdat = fit())
    ete <- te
    for(i in 1:4){
      ete[,i] = formatC(te[,i],digits =num.decimals(te[,i]),drop0trailing = T,format = "f")
    }
   
    er<- reactive({ete})
    
    colnms = objectives()
    #get unit input 
    uns = c(input$unit1,input$unit2,input$unit3,input$unit4)
    
    new_colnms <- mapply(function(col, unit) {
      if (unit != "") {
        paste(col, " (", unit, ")", sep = "")
      } else {
        col
      }
    }, col = colnms, unit = uns, SIMPLIFY = TRUE)
   
    
    ## table of chosen line 
    output$click_info <- renderTable({
      lclick = as.data.frame(er())
      colnames(lclick) = new_colnms
      
      lclick
        }, include.rownames = F)
    
    }, ignoreNULL = TRUE)
  
  
  ## line plot
  output$linePlot <- renderPlot({
    req(f_scaled,objectives())
    sk= match_scaled(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                     maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),scal_tab = f_scaled(),allobs = objectives())

    ko= sk%>% mutate(id = factor(row_number()))%>%pivot_longer(.,cols=-id)%>%
      mutate(name=factor(name))%>%mutate(name=forcats::fct_relevel(name,objectives()))

    plot_parline(datt=ko,colols=rv$colls,sizz=rv$sizes)

  })
  
  ## scaled table 
    output$sliders <- renderTable({
      req(f_scaled(),fit(),objectives())
      
      slid = data.frame(
      col1 = c(input$obj1[2],input$obj1[1]),
      col2 = c(input$obj2[2],input$obj2[1]),
      col3 = c(input$obj3[2],input$obj3[1]),
      col4 = c(input$obj4[2],input$obj4[1]),
      row.names = c("max","min")
    )
     colnames(slid) = objectives()
     
     slid},
     include.rownames=TRUE)

  
  ## absolute table
  output$sliders_abs <- renderTable({
    req(f_scaled(),fit(),objectives())
    colnms = objectives()
    
    dt <- scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                           maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                           scal_tab = f_scaled(),
                           abs_tab = fit(),
                           allobs = objectives())
    
    # varying number of decimals
    # min and max the same number of dec
    dn = dt
    for(j in 1:2){
      for(i in 1:4){
        dn[j,i] = formatC(dt[j,i],digits =num.decimals(as.numeric(dt[1,i])),drop0trailing = T,format = "f")
      }}
    
    #get unit input 
    uns = c(input$unit1,input$unit2,input$unit3,input$unit4)
    
    new_colnms <- mapply(function(col, unit) {
      if (unit != "") {
        paste(col, " (", unit, ")", sep = "")
      } else {
        col
      }
    }, col = colnms, unit = uns, SIMPLIFY = TRUE)
    
    colnames(dn) = new_colnms  
    
    dn},
    include.rownames = TRUE)
  
  ## barplot
  output$sliders_plot <- renderPlot({
   req(fit(),f_scaled(),objectives())
   
    matchi =  reactive({scaled_abs_match(
      minval_s = c(input$obj1[1], input$obj2[1], input$obj3[1], input$obj4[1]),
      maxval_s = c(input$obj1[2], input$obj2[2], input$obj3[2], input$obj4[2]),
      abs_tab = fit(),allobs = objectives(),scal_tab = f_scaled(),
      smll = F)})
    
    pldat<- prep_diff_bar(abs_tab=fit(),red_tab=matchi(),allobs= objectives(), neg_var=input$sel_neg)
  
    plot_diff_bar(pldat,obj_choices=objectives())
  
  })
  
  ## scatter plot
  output$scatter_plot <- renderPlot({
    req(fit(), objectives())
    
    scat_abs = scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                                maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                                abs_tab = fit(),scal_tab = f_scaled(),
                                allobs = objectives(),smll=F)
    
    ggpairs(scat_abs,
          lower = list(continuous = "points"),  # Lower triangle: scatter plots
          diag = list(continuous = "blank"),    # Diagonal: blank
          upper = list(continuous = "blank"),   # Upper triangle: blank
          axisLabels = "show"                   # Show axis labels
  )+ 
    theme_bw(base_size = 10) +  # Apply a theme with smaller base size
    theme(
      strip.background = element_blank(),  # Remove background from facet labels
      strip.text = element_text(size = 10, face = "bold"),  # Smaller facet labels
      axis.text = element_text(size = 8),  # Smaller axis text
      axis.title = element_text(size = 10, face = "bold"),  # Smaller axis titles
      panel.spacing = unit(0.1, "lines"),  # Reduce space between panels
      plot.margin = margin(5, 5, 5, 5)  # Reduce plot margins
    )})
  
  
  ### Data Prep ####
  
  if(!file.exists("../data/pareto_fitness.txt")){shinyjs::show(id="fitness_avail")}
  
  file_data1 <- reactiveVal(NULL)
  file_data2 <- reactiveVal(NULL)
  file_data3 <- reactiveVal(NULL)
  file_data4 <- reactiveVal(NULL)
  shapefile <- reactiveVal(NULL)

  ## check if required files exist
  observeEvent(input$file1, { file <- input$file1
    if (is.null(file)) {return(NULL)}
     file_data1(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file2, { file <- input$file2
    if (is.null(file)) {return(NULL)}
    file_data2(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file3, { file <- input$file3
    if (is.null(file)) {return(NULL)}
    file_data3(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file4, { file <- input$file4
  if (is.null(file)) {return(NULL)}
  file_data4(list(path = file$datapath, name = file$name))})
  
  
  observeEvent(input$shapefile, { file <- input$shapefile
  if (is.null(file)) {return(NULL)}
  shapefile(list(path = file$datapath, name = file$name))})
 
  
  observeEvent(input$files_avail,{
     
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

     shp_req = c(".shp",".shx", ".dbf", ".prj")
     shapefile <- input$shapefile
     shapefile_names <- shapefile$name
     shapefile_paths <- shapefile$datapath
     missing_shapefile_components <- shp_req[!sapply(shp_req, function(ext) any(grepl(paste0(ext, "$"), shapefile_names)))]
     
     # copy shapefile components if none are missing
     if (length(missing_shapefile_components) == 0) {
       lapply(seq_along(shapefile_paths), function(i) {
         save_path <- file.path(save_dir, shapefile_names[i])
         if (!file.exists(save_path)) {
           file.copy(shapefile_paths[i], save_path, overwrite = TRUE)
         }
       })
     }
     
     
     required_files <- c("../data/pareto_genomes.txt","../data/hru.con",   "../data/measure_location.csv",
                         "../data/hru.shp","../data/hru.shx", "../data/hru.dbf", "../data/hru.prj","../data/pareto_fitness.txt")
     
     checkFiles <- sapply(required_files, function(file) file.exists(file))
     
     output$fileStatusMessage <- renderText({
       if (all(checkFiles) & file.exists("../input/object_names.RDS")) {
         HTML(
           "All Files found.")
         
       } else if(all(checkFiles) & !file.exists("../input/object_names.RDS")){
         HTML("All files found. <br>Please provide the names of the objectives represented in the Pareto front.
             The names and the order in which they are given have
             to align with what is provided in the first four columns of pareto_fitness.txt")
         }else {
         missing_files = required_files[!checkFiles]
         HTML(paste("The following file(s) are missing:<br/>", paste(sub('../data/', '', missing_files), collapse = "<br/> ")))
       }
     })
     
     
  })
  
  
  
  if(!file.exists("../input/var_corr_par.csv") | !file.exists("../input/hru_in_optima.RDS") | !file.exists("../input/all_var.RDS")){
    shinyjs::show(id="runprep_show")
  }
  
   ## initialise PCA table when app starts
   pca_in <- reactiveValues(data = read_pca()) #this only reads config$columns, NULL if opening for the first time
   
   objs <- reactiveValues(data=NULL)
   
   ## observe event for confirm button
   if(!file.exists("../input/object_names.RDS")){
     shinyjs::show(id = "sel_obj")
   observeEvent(input$obj_conf, {
     shinyjs::show(id="range_title")
     objs$data<- data.frame(Objective = c(input$col1, input$col2, input$col3, input$col4), stringsAsFactors = FALSE )
     
     objs$objectives <- c(input$col1, input$col2, input$col3, input$col4)
     
     assigned_objnames <<- objs$objectives
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
       
     },rownames = T)})}else{assigned_objnames <<- readRDS("../input/object_names.RDS")
     output$obj_conf <- renderTable({
       rng = get_obj_range(colnames = assigned_objnames)
       bng = rng
       
       for(i in 1:4){
         for(j in 2:3){
           bng[i,j] = formatC(rng[i,j],digits= unlist(lapply(rng[,2],num.decimals))[i],drop0trailing = T,format="f")#same decimal for min and max
         }
       }
       bng
       
     },rownames = T)}
   

   ## run external script that calculates all variables considered in the clustering
    
     observeEvent(input$runprep,{
       
       dp_done(FALSE)
       script_output(character()) #clear old output
       
       optain <- process$new("Rscript",c("convert_optain.R"),stdout = "|", stderr = NULL) #stdout | ---> pipe output, stderr ---> ignore
       autoInvalidate <- reactiveTimer(100)
       
       observe({autoInvalidate()
         if (optain$is_alive()) {
           new_output <- optain$read_output_lines()
           if (length(new_output) > 0) {
             current_output <- script_output()
             
             # append new output lines and limit to last 10 lines
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
      
      ## hide tab if user has not supplied all data files
      observeEvent(input$tabs == "correlation_analysis", {
        required_files <- c(
          "../data/pareto_genomes.txt",
          "../data/hru.con",
          "../data/measure_location.csv",
          "../data/hru.shp",
          "../data/hru.shx",
          "../data/hru.dbf",
          "../data/hru.prj",
          "../data/pareto_fitness.txt",
          "../input/object_names.RDS",
          "../input/all_var.RDS"
        )
        
        checkFiles <- sapply(required_files, function(file) file.exists(file))
        
        ## only show correlation tab when all files are available
        if (all(checkFiles) == F) {
          shinyjs::hide(id = "corr_content")
          shinyjs::show(id = "corr_notthere")
          shinyjs::hide(id = "corr_sidebar")
          neednames = ""
          whatsmissing = ""
          output$corr_notthere <- renderText({
            missing_files = required_files[!checkFiles]
            if ("../input/object_names.RDS" %in% missing_files & length(missing_files) != 1) {
              whatsmissing = "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>"
              
              missing_files <- missing_files[missing_files != "../input/object_names.RDS"]
              neednames = "To be able to proceed please also define the objective names in the previous tab."
              
            } else if ("../input/object_names.RDS" %in% missing_files & length(missing_files) == 1){
              whatsmissing = "All files have been provided, please specify the objective names in the previous tab."
              
            } else if ("../input/all_var.RDS" %in% missing_files & length(missing_files) != 1){
              missing_files <- missing_files[missing_files != "../input/all_var.RDS"]
              
              whatsmissing = "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>"
              neednames = "Please also define the objective names in the previous tab."
              
            }else if ("../input/all_var.RDS" %in% missing_files & length(missing_files) == 1){
              neednames = "Please rerun the Data Preparation in the previous tab."
              
            }else{
              whatsmissing = "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>"
              
            }
            
            HTML(paste(
              whatsmissing,
              paste(sub('../data/', '', missing_files), collapse = "<br/> "), "<br/> ",neednames
            ))
          })
        }else{ shinyjs::show(id = "corr_content")
          shinyjs::show(id = "corr_sidebar")
          shinyjs::hide(id = "corr_notthere")}
      })
      
      
      ## actual tab
      observeEvent(input$tabs == "correlation_analysis",{ 
        if(file.exists("../data/measure_location.csv")) {
          mes = read.csv("../data/measure_location.csv")
          mes <<- unique(mes$nswrm)

          nm <<- length(mes)
        }} )

      
  observeEvent(input$run,{
    all_var <<- readRDS("../input/all_var.RDS")
    
    write_corr(vars = input$selements,cor_analysis = T, pca = F)
    
    # define the command to run the Python script
    py_script <- "../python_files/correlation_matrix.py"
    cmd <- paste("python", py_script)
    
    # run the command and capture the output
    result <- system(cmd, intern = TRUE)
    
    
  # correlation Plot (does not change for different thresholds)
    csv_file <- "../output/correlation_matrix.csv"
    validate(need(file.exists(csv_file), "CSV file not found."))
    corr <<- read.csv(csv_file, row.names = 1) #global because of re-rendering of plot
    output$corrplot <- renderPlot({plt_corr(corr)})
   
  ## events tied to a change in threshold, however also tied to change in selected variables, therefore also observe run
  observeEvent(input$thresh,{
    
    # reprint highest correlation table marking removed 
    output$corrtable <- renderDT({find_high_corr(corr,threshold=input$thresh, tab=T, strike=NULL)}) #tab = T means this returns the full table, =F is for pulling variables
   
    # Top Table with selected elements
    output$selements <- renderTable({input$selements},rownames = T,colnames = F)
    })
  
    # Drop Down Menu with subset of those with selected threshold
    observe({updateSelectInput(session, "excl",choices = find_high_corr(corr,threshold=input$thresh, tab=F))})
  })
  
  ## on clicking confirm selection the config ini is updated
  observeEvent(input$confirm_selection,{
    pca_remove(input$excl)

    output$corrtable <- renderDT({
      datatable(find_high_corr(corr,threshold = input$thresh,tab = T,strike = input$excl),escape = FALSE)}) #tab = T means this returns the full table, =F is for pulling variables
    
    pca_content = all_var[-which(all_var %in% pca_remove())]
    print(pca_content)
    saveRDS(pca_content,file = "../input/pca_content.RDS") #required for PCA
    
    pca_in$data = pca_content
    
    write_corr(pca_content = pca_in$data,
               pca = T,
               cor_analysis = F)#this is also called into the pca tab on startup
    
    nonoval = paste(pca_remove(), collapse = ", ")
    
  # display confirmed selection in the Correlation Analysis tab
   output$confirmed_selection <- renderText({HTML(paste0("Removed variables: ","<b>", nonoval,"</b>"))})
  
  
  ### PC Analysis ####
  # table with variables INCLUDED in PCA (renewed every time confirm selection is clicked in correlation tab)
    pca_table(pca_in$data)

  })
  # reactive values to store selected choices
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
    
    if(file.exists("../input/pca_content.RDS")){updateNumericInput(session, "pca_max", value = max_pca(), max=max_pca())}
      
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
    
    # current selections
    selected1 <- input$element1
    selected2 <- input$element2
    selected3 <- input$element3
    selected4 <- input$element4
    
    # available choices for each dropdown
    choices1 <- setdiff(all_choices(), c(selected2, selected3, selected4))
    choices2 <- setdiff(all_choices(), c(selected1, selected3, selected4))
    choices3 <- setdiff(all_choices(), c(selected1, selected2, selected4))
    choices4 <- setdiff(all_choices(), c(selected1, selected2, selected3))
    
    # update the choices for each dropdown
    updateSelectInput(session, "element1", choices = choices1, selected = selected1)
    updateSelectInput(session, "element2", choices = choices2, selected = selected2)
    updateSelectInput(session, "element3", choices = choices3, selected = selected3)
    updateSelectInput(session, "element4", choices = choices4, selected = selected4)
    
    selected1_2 <- input$axisx
    selected2_2 <- input$axisy
    selected3_2 <- input$colour
    selected4_2 <- input$size
    
    choices1_2 <- setdiff(all_choices(), c(selected2_2, selected3_2, selected4_2))
    choices2_2 <- setdiff(all_choices(), c(selected1_2, selected3_2, selected4_2))
    choices3_2 <- setdiff(all_choices(), c(selected1_2, selected2_2, selected4_2))
    choices4_2 <- setdiff(all_choices(), c(selected1_2, selected2_2, selected3_2))
    
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
  update_settings()
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
    update_settings()
  })

  
  observeEvent(input$runPCA,{
    # python status
    output$pca_status <- renderText({pca_status()})
    pca_content <<- readRDS("../input/pca_content.RDS")
    
    output$pca_mess <- renderUI({
      tags$p("If all data was provided in the right format, the PCA outputs will open in separate windows - you can discard or save them as necessary.")
      })
    
    isElementVisible(TRUE)
    
    ## prepare config.ini
    write_corr(pca_content = pca_content,pca=T, cor_analysis = F)# columns
    
    # command to run the Python script
    if(input$pcamethod=="k-means"){pca_script <- "../python_files/kmeans.py"}else{pca_script <- "../python_files/kmedoid.py"}
    
    run_python_script(path_script=pca_script,pca_status)
    
    }, once = TRUE)
  
  ## cluster specs
  observeEvent(input$write_clust, {
    fixbool = ifelse(input$clusyn == "No", "true", "false")
    if (input$clusyn == "No") {
      write_cluster(fixed_clusters = input$clus_fix,fixed_cluster_boolean = fixbool)
      } else{
      write_cluster(min_cluster = input$clus_min,max_cluster = input$clus_max,fixed_cluster_boolean = fixbool)
      }
    update_settings()
  })
  ## outlier specs
  observeEvent(input$write_outl, {
    outlbool = ifelse(input$outlyn == "No","false","true")
    if(input$outlyn == "Yes"){
      write_outl(handle_outliers_boolean=outlbool,deviations_min=input$sd_min,deviations_max=input$sd_max, 
                 count_min=input$count_min,count_max=input$count_max,outlier_to_cluster_ratio=input$outlier_ratio )
    }else{
      write_outl(handle_outliers_boolean=outlbool)#bool is turning on all others, if false all others are ignored/default value works
    }
    update_settings()
  })
  
  ## reactive value to output for use in conditionalPanel
  output$isElementVisible <- reactive({
    isElementVisible()
  })
  
  ## conditionalPanel to work with reactive output
  outputOptions(output, "isElementVisible", suspendWhenHidden = FALSE)
  
  
  ## pca min/max specs
  observeEvent(input$pcaminmax,{
    write_pcanum(pcamin=input$pca_min,pcamax=input$pca_max)
    update_settings()
  })
  output$pca_settings_summary <- renderUI({HTML(settings_text())})
  
  
  ### Analysis ####
  observeEvent(input$tabs == "analysis", {
    if (file.exists(dir("../output/", pattern = 'clusters_representativesolutions', full.names = TRUE))) {
      
      sols_data = read.csv(dir("../output/", pattern = 'clusters_representativesolutions', full.names = TRUE))
      
      sols(sols_data %>% rownames_to_column("optimum") %>%
          filter(!is.na(Representative_Solution)) %>% select(1:5) %>%
          mutate(across(where(is.numeric), round, digits = 5))
      )
    } else{
      sols(data.frame(Message = "something went wrong - has the PCA run properly"))
    }
    output$antab <- renderDT({
      datatable(sols(),
                selection = "single",
                options = list(pageLength = 20, autoWidth = TRUE))
    })
    if(file.exists("../input/hru_in_optima.RDS")){
    cm(pull_shp(layername="hru", optims = sols(),hru_in_opt_path = "../input/hru_in_optima.RDS")) }
    needs_buffer(pull_buffer())
    if(file.exists("../data/hru.con")){lalo <<- plt_latlon(conpath = "../data/hru.con")}

  })
  
  observeEvent(input$plt_opti,{
    selected_row <- input$antab_rows_selected
    if (is.null(selected_row)) {
      
      shinyjs::show(id = "no_row")
      output$no_row = renderText({paste("No row selected")})
    } else {
      shinyjs::hide(id = "no_row")
      
      selected_data <- sols()[selected_row,]

      buffs = needs_buffer()

      hru_sel =  plt_sel(shp=cm(),opti_sel = selected_data$optimum)

      mes = read.csv("../data/measure_location.csv")
    
      output$map <- renderLeaflet({plt_lf(dat=hru_sel,cols = "optim", mes = unique(mes$nswrm),la = lalo[1],lo =lalo[2], buff_els=buffs)})
   
    }
  })
  
}

# setwd("C:/Users/wittekin/Documents/cle2024/projects/src/pareto_optain/app")
# profvis(runApp())
