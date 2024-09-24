######################## SERVER ####################################
# comments: 
# Project: Clustering Pareto Solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
server <- function(input, output, session) {

  ## reactive values
  objectives <- reactiveVal(character()) #objective names
  
  par_fiti <- reactiveVal(NULL)#handling pareto_fitness
  sq_file <- reactiveVal(NULL)#handling sq_fitness
  fit <- reactiveVal(NULL) #absolute value dataframe
  f_scaled <- reactiveVal(NULL) #scaled value dataframe
  pca_remove <- reactiveVal(NULL) #variables removed from pca
  stq <- reactiveVal(NULL) #status quo
  #data prep
  run_prep_possible = reactiveValues(files_avail = FALSE) #allow prep script to run when all required files available
  script_output <- reactiveVal("") # data prep R output
  dp_done = reactiveVal(FALSE) # checking if data prep R output is done
  
  all_choices = reactiveVal()
  ahp_choices = reactiveVal()
  isElementVisible = reactiveVal(FALSE)
  
  #play around
  rv <-reactiveValues(sizes= NULL,colls = NULL) #color for parallel axes
  er <- reactiveVal(NULL) #position
  
  best_option = reactiveVal(NULL)
  
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
  max_pca <- reactiveVal()# required for max pc field
  pca_available <- reactiveValues(button1_clicked = FALSE, button2_clicked = FALSE) #controls config.ini writing previous to clustering
  #results table
  check_files<- reactiveVal(NULL)
  sols <- reactiveVal()
  
  #catchment shapes
  cm <- reactiveVal()
  needs_buffer <- reactiveVal()
  cm_clean <- reactiveVal()
  
  #ahp
  previous_vals = reactiveValues(
    x_var = NULL,
    y_var = NULL,
    color_var = NULL,
    size_var = NULL
  )
  coma = reactiveVal()
  range_controlled = reactiveVal(NULL)
  initial_update_done = reactiveVal(FALSE)
  
  #figure in analysis rendering
  is_rendering <- reactiveVal(FALSE)
  
  ### Introduction ####
  
  ### Play Around Tab ####
  
  # limit input size of objective names
  shinyjs::runjs("$('#short1').attr('maxlength', 17)")
  shinyjs::runjs("$('#short2').attr('maxlength', 17)")
  shinyjs::runjs("$('#short3').attr('maxlength', 17)")
  shinyjs::runjs("$('#short4').attr('maxlength', 17)")
  
  
  ##check if names of objectives have to be supplied or already present
  observeEvent(input$tabs == "play_around",{ 
    ## make or pull objectives()
    if(!file.exists("../input/object_names.RDS")) {
      
      shinyjs::show(id = "obj_first")
      shinyjs::hide(id = "tab_play1")
      shinyjs::hide(id = "tab_play2")
      
      ##get new objective names
      observe({
        req(input$short1, input$short2, input$short3, input$short4)
        short <<- c(input$short1, input$short2, input$short3, input$short4)
        objectives(short)
        
        updateSelectInput(session, "x_var3",   choices = short, selected = short[1])
        updateSelectInput(session, "y_var3",   choices = short, selected = short[2])
        updateSelectInput(session, "col_var3", choices = short, selected = short[3])
        updateSelectInput(session, "size_var3",choices = short, selected = short[4])
        
      })
      
      observeEvent(input$save_par_fiti, {
        req(objectives(),input$short1, input$short2, input$short3, input$short4)
        saveRDS(short, file = "../input/object_names.RDS")
        
        updateTextInput(session,"col1", value = objectives()[1] )
        updateTextInput(session,"col2", value = objectives()[2] )
        updateTextInput(session,"col3", value = objectives()[3] )
        updateTextInput(session,"col4", value = objectives()[4] )
        
        write_pca_ini(var1=input$short1,var2=input$short2,var3=input$short3,var4=input$short4,
                      var1_lab="",var2_lab="",var3_lab="",var4_lab="")#save label for future use (pulled w/ read_config_plt in Data prep, pca and cluster)
      }) 
      
    } else {
      shinyjs::hide(id="obj_first")
      short = readRDS("../input/object_names.RDS")
      objectives(short)
      
      updateSelectInput(session, "x_var3",   choices = short, selected = short[1])
      updateSelectInput(session, "y_var3",   choices = short, selected = short[2])
      updateSelectInput(session, "col_var3", choices = short, selected = short[3])
      updateSelectInput(session, "size_var3",choices = short, selected = short[4])
    }
    
    ## update slider labels based on objectives
    observe({
      req(objectives())
      obj <- objectives()
      for(i in 1:4){
        updateSliderInput(session, paste0("obj",i), label = obj[i])
        updateSliderInput(session, paste0("obj",i,"_ahp"), label = obj[i])
        
        }
      updateCheckboxGroupInput(session, "sel_neg", choices = objectives(), selected = NULL)
    })
    
    
    ## make or pull fit()
    observe({
      if (file.exists(pareto_path)) {
        
        req(objectives())
        shinyjs::hide(id = "parfit")

        data <- read.table( pareto_path, header = FALSE, stringsAsFactors = FALSE,sep = ',')
        new_col_data <- objectives()
        colnames(data) = new_col_data
        fit(data)
        
        yo = fit() %>% mutate(across(everything(), ~ scales::rescale(.)))%>%mutate(id = row_number())
        f_scaled(yo)
        
        output$uploaded_pareto <- renderText({"All Files found. You can now examine the Pareto front. 
        How does it change when the objective ranges are modified?"})
        
        
       ## adapt sliders in ahp tab
          min_max <-data.frame(t(sapply(data, function(x) range(x, na.rm = TRUE))))
          names(min_max) =c("min","max")
          range_value = NULL
          for (i in 1:4) {
            var_name <- paste0("steps", i)
            
            if (abs(min_max$max[i]) <= 0.05) {
              min_max$max[i] = min_max$max[i] * 1000
              min_max$min[i] = min_max$min[i] * 1000
              
              range_value = append(range_value,(rownames(min_max[i, ])))
            }
          range_controlled(range_value)
          
          assign(var_name, (min_max$max[i]-min_max$min[i])/20)
          
          updateSliderInput(session, paste0("obj",i,"_ahp"), value = c(min_max$min[i],min_max$max[i]),min =min_max$min[i],max = min_max$max[i],step=var_name)
          initial_update_done(TRUE)
          
          }
        
      } else {
        shinyjs::show(id = "parfit")
      }
    })
    
    ## make fit() based on user input
    observeEvent(input$par_fit, {     
      req(input$par_fit)
      file <- input$par_fit
      if (is.null(file)) {return(NULL)}
      par_fiti(list(path = file$datapath, name = file$name))
    })
    
    #get pareto_fitness.txt and make fit()
    observeEvent(input$save_paretofit,{
      req(par_fiti(),objectives())
      save_par_fiti <- par_fiti()$name
      save_path_par_fiti <- file.path(save_dir, save_par_fiti)
      file.copy(par_fiti()$path, save_path_par_fiti, overwrite = TRUE) #copy pareto_fitness.txt
      
      data = read.table(pareto_path, header=F,stringsAsFactors=FALSE,sep = ',')
      names(data) = objectives()
      fit(data)
      
      yo = fit() %>% mutate(across(everything(), ~ scales::rescale(.)))%>%mutate(id = row_number())
      f_scaled(yo)
    })
    
   #pull status quo
    observe({
      if (file.exists("../data/sq_fitness.txt")) {
        req(objectives())
        
        shinyjs::enable("plt_sq")
        
        shinyjs::hide("sq")
        st_q = read.table("../data/sq_fitness.txt", header = FALSE, stringsAsFactors = FALSE, sep = ' ')
        names(st_q) = objectives()
        stq(st_q)
        
        }else{shinyjs::show("sq")
          shinyjs::disable("plt_sq")} })
    
    #make status quo based on user input
    observeEvent(input$sq_in, {
      req(input$sq_in)
      file <- input$sq_in
      if(is.null(file)){return(NULL)}
      sq_file(list(path=file$datapath, name = file$name))
      
    })
    
    observeEvent(input$save_sq_in, {
      req(sq_file(), req(objectives))
      save_sq <- sq_file()$name
      save_path_sq <- file.path(save_dir, save_sq)
      file.copy(sq_file()$path,save_path_sq,overwrite = TRUE) #copy sq_fitness.txt
      
      st_q = read.table("../data/sq_fitness.txt", header = FALSE, stringsAsFactors = FALSE, sep = ' ')
      names(st_q) = objectives()
      stq(st_q)
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
    
    ## get unit input 
   if(file.exists("../input/units.RDS")){shinyjs::hide(id="units")
        uns <<-  readRDS("../input/units.RDS")
      }else{
        uns <<- c(input$unit1,input$unit2,input$unit3,input$unit4)
        
        observeEvent(input$save_unit,{saveRDS(uns, file="../input/units.RDS")
          isolate({axiselected(c(input$unit1,input$unit2,input$unit3, input$unit4))}) #for later use in clustering
          
          updateTextInput(session, "axisx",  value  = axiselected()[1])
          updateTextInput(session, "axisy", value = axiselected()[2])
          updateTextInput(session, "colour", value = axiselected()[3])
          updateTextInput(session, "size", value = axiselected()[4])
          
          write_uns(var1_lab= input$unit1, var2_lab = input$unit2, var3_lab = input$unit3, var4_lab = input$unit4,inipath="../input/config.ini")
          })
      }
    })
  
    ## show rest of tab if all required data available
    observe({
      
      test_fit = fit()
      test_objectives = objectives()

    if (!is.null(test_fit) && !is.null(test_objectives)) {
      shinyjs::show("tab_play1")
      shinyjs::show("tab_play2")
      shinyjs::show("scatter")
     }
    })
    ## pareto plot on top
    
    first_pareto_fun = function(){
      req(f_scaled(),objectives(),fit())
      #match scaled input with unscaled fit() to create dat
      dat = scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                       maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                       abs_tab = fit(),scal_tab = f_scaled(),
                       allobs = objectives(),smll=F)
      #run plt_sc_optima with sq but no other options
      return(plt_sc_optima(dat=dat,    x_var = input$x_var3,
                    y_var = input$y_var3,
                    col_var = input$col_var3,
                    size_var = input$size_var3, status_q = input$add_sq_f,an_tab=T))
      
      
    }
    
    output$first_pareto <- renderPlot({ first_pareto_fun() })
    
    
    ## line plot
    parplot_fun = function(){
      req(f_scaled(),objectives(),fit())
      sk= match_scaled(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                       maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),scal_tab = f_scaled(),allobs = objectives())
      
      ko= sk%>% mutate(id = factor(row_number()))%>%pivot_longer(.,cols=-id)%>%
        mutate(name=factor(name))%>%mutate(name=forcats::fct_relevel(name,objectives()))
      
      if(input$plt_sq) {
        req(stq())
        
        #rescale single (extra) point
        min_fit <- apply(fit(), 2, min)
        max_fit <- apply(fit(), 2, max)
        
        stq_sk <- as.data.frame(mapply(function(col_name, column) {
          rescale_column(column, min_fit[col_name], max_fit[col_name])
        }, objectives(), stq(), SIMPLIFY = FALSE))
        
        colnames(stq_sk) = objectives()#otherwise spaces do not work because mapply adds dots
        
        stq_ko <- pivot_longer(stq_sk,cols = everything(),names_to = "name",values_to = "value")
        stq_ko <- stq_ko %>% mutate(name=forcats::fct_relevel(name,objectives()))
        
        return(plot_parline(datt = ko,colols = rv$colls,   sizz = rv$sizes, sq = stq_ko))
        
      }else{
        
        return(plot_parline(datt = ko,colols = rv$colls, sizz = rv$sizes, sq= NULL))
      }
      
    }
    
    output$linePlot <- renderPlot({ parplot_fun() })
    
 
  ## pull values from parallel axis line when clicked
  observeEvent(input$clickline,{
    req(fit(), objectives())
    x = round(input$clickline$x)
   
    val = input$clickline$y

    sc= match_scaled(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                     maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]), scal_tab=f_scaled(),allobs=objectives())

    # pull the closest value
    yo= sc%>% mutate(id = row_number())%>%slice(which.min(abs(.[[x]] - val)))
    rom = as.numeric(yo[["id"]]) #length(rv) has to align with sc and NOT with fit()
    
    # change size vector
    rv$sizes[rom] = 1.3
    rv$colls[rom] = "#FF5666"# "#797596"
  
    # reset other sizes and colors
    rv$sizes[-rom] = 0.5
    rv$colls[-rom] = "grey50"
  
    # selected optimum from reduced table
    fml = scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                           maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                           abs_tab = fit(),scal_tab = f_scaled(),
                           allobs = objectives(),smll=F)

    te = fml[yo$id,]   # te <- fit()[yo$id,] would not work!!
   
    ete <- te
    for(i in 1:4){
      ete[,i] = formatC(te[,i],digits =num.decimals(te[,i]),drop0trailing = T,format = "f")
    }
   
    er(ete)
    
    colnms = objectives()
    
 
    ## table of chosen line 
    output$click_info <- renderTable({
      new_colnms <- mapply(function(col, unit) {
        if (unit != "") {
          paste(col, " (", unit, ")", sep = "")
        } else {
          col
        }
      }, col = colnms, unit = uns, SIMPLIFY = TRUE)
      
      lclick = as.data.frame(er())
      colnames(lclick) = new_colnms
      lclick
        }, include.rownames = F)
    
    }, ignoreNULL = TRUE)
  
  
  
  output$download_line_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      paste(input$line_plot_savename,curt, ".png", sep = "")
    },
    content = function(file) {
      # png(file, width=1000, height = 800)
      # parplot_fun()
      # dev.off()
      ggsave(file, plot = parplot_fun(), device = "png", width = 2000, height = 1000,units = "px")
    }
  )
  
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
     
     slid}, include.rownames=TRUE)

  
  ## absolute table
  output$sliders_abs <- renderTable({
    req(f_scaled(),fit(),objectives())
    colnms = objectives()
    
    dt <- scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                           maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                           scal_tab = f_scaled(),
                           abs_tab = fit(),
                           allobs = objectives())
    
    # varying number of decimals, but min and max the same number of dec
    dn = dt
    for(j in 1:2){
      for(i in 1:4){
        dn[j,i] = formatC(dt[j,i],digits =num.decimals(as.numeric(dt[1,i])),drop0trailing = T,format = "f")
      }}
    
    #get unit input 
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
  
  mima_fun = function(){
    req(fit)
    df = t(get_mima(fit()))[-1,]
    df = df[nrow(df):1,]
    return(df)
  }
  

  output$whole_range <- renderTable({
   mima_fun()
  }, rownames = T)
  
  ## barplot
  # output$sliders_plot <- renderPlot({
  #  req(fit(),f_scaled(),objectives(),input$obj1,input$obj2,input$obj3,input$obj4)
  #  
  #   matchi =  reactive({scaled_abs_match(
  #     minval_s = c(input$obj1[1], input$obj2[1], input$obj3[1], input$obj4[1]),
  #     maxval_s = c(input$obj1[2], input$obj2[2], input$obj3[2], input$obj4[2]),
  #     abs_tab = fit(),allobs = objectives(),scal_tab = f_scaled(),
  #     smll = F)})
  #   
  #   pldat<- prep_diff_bar(abs_tab=fit(),red_tab=matchi(),allobs= objectives(), neg_var=input$sel_neg)
  # 
  #   plot_diff_bar(pldat,obj_choices=objectives())
  # 
  # })
  
  ## scatter plot
  scat_fun = function(){
    req(fit(), objectives(),f_scaled())
    scat_abs = scaled_abs_match(minval_s=c(input$obj1[1],input$obj2[1],input$obj3[1],input$obj4[1]),
                                maxval_s=c(input$obj1[2],input$obj2[2],input$obj3[2],input$obj4[2]),
                                abs_tab = fit(),scal_tab = f_scaled(),
                                allobs = objectives(),smll=F)
    
    if(!is.null(er())){
      rom = which(apply(scat_abs, 1, function(row) all(row == er())))
      col = rep("grey",nrow(scat_abs))
      col[rom] = "red"
      sizz = rep(2.5, nrow(scat_abs))
      sizz[rom] = 3
    }else{col = rep("grey",nrow(scat_abs))
    
    sizz = rep(2.5, nrow(scat_abs))}
    
    mima = get_mima(fit())

    plot_scatter = plt_sc(dat = scat_abs, ranges=mima, col = col, size= sizz)
    
    grid.arrange(grobs = plot_scatter, nrow = 2, ncol = 3)}
  
  
    output$scatter_plot <- renderPlot({ scat_fun()})
  
    
  output$download_scat_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      
      paste(input$scat_plot_savename,curt, ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1500, height=1000)
      scat_fun()
      dev.off()
    }
  )
  
  output$download_diff_plot <- downloadHandler(
    filename = function() {
      paste(input$diff_plot_savename, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 7, height = 5) #different approach, hard to turn into function
    }
  )
  
  
  ### Data Prep ####
  
  if (!file.exists("../data/pareto_fitness.txt")) {shinyjs::show}
  if (!file.exists("../data/sq_fitness.txt")){shinyjs::show(id = "sq_avail")
                                              shinyjs::disable("plt_sq")}else{shinyjs::enable("plt_sq")}
  
  file_data1 <- reactiveVal(NULL)
  file_data2 <- reactiveVal(NULL)
  file_data3 <- reactiveVal(NULL)
  file_data4 <- reactiveVal(NULL)
  file_data5 <- reactiveVal(NULL)
  file_data6 <- reactiveVal(NULL)
  
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
  
  observeEvent(input$file5, { file <- input$file5
  if (is.null(file)) {return(NULL)}
  file_data5(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$file6, { file <- input$file6
  if (is.null(file)) {return(NULL)}
  file_data6(list(path = file$datapath, name = file$name))})
  
  observeEvent(input$shapefile, { file <- input$shapefile
  if (is.null(file)) {return(NULL)}
  shapefile(list(path = file$datapath, name = file$name))})
 
  
  observeEvent(input$files_avail,{
     
     save_filename1 <- file_data1()$name
     save_filename2 <- file_data2()$name
     save_filename3 <- file_data3()$name
     save_filename4 <- file_data4()$name
     save_filename5 <- file_data5()$name
     save_filename6 <- file_data6()$name
     
     save_path1 <- file.path(save_dir, save_filename1)
     save_path2 <- file.path(save_dir, save_filename2)
     save_path3 <- file.path(save_dir, save_filename3)
     save_path4 <- file.path(save_dir, save_filename4)
     save_path5 <- file.path(save_dir, save_filename5)
     save_path6 <- file.path(save_dir, save_filename6)
     

     file.copy(file_data1()$path, save_path1, overwrite = TRUE)
     file.copy(file_data2()$path, save_path2, overwrite = TRUE)
     file.copy(file_data3()$path, save_path3, overwrite = TRUE)
     file.copy(file_data4()$path, save_path4, overwrite = TRUE)
     file.copy(file_data5()$path, save_path5, overwrite = TRUE)
     file.copy(file_data6()$path, save_path6, overwrite = TRUE)
     
     #cm shapefile
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
     
     #basin shapefile
     bas_req = c(".shp",".shx", ".dbf", ".prj")
     basfile <- input$basfile
     basfile_names <- basfile$name
     basfile_paths <- basfile$datapath
     missing_basfile_components <- bas_req[!sapply(bas_req, function(ext) any(grepl(paste0(ext, "$"), basfile_names)))]
     
     # copy basfile components if none are missing
     if (length(missing_basfile_components) == 0) {
       lapply(seq_along(basfile_paths), function(i) {
         save_path <- file.path(save_dir, basfile_names[i])
         if (!file.exists(save_path)) {
           file.copy(basfile_paths[i], save_path, overwrite = TRUE)
         }
       })
     }
     
     required_files <- c("../data/pareto_genomes.txt","../data/hru.con",   "../data/measure_location.csv",
                         "../data/hru.shp","../data/hru.shx", "../data/hru.dbf", "../data/hru.prj",
                         "../data/basin.shp","../data/basin.shx", "../data/basin.dbf", "../data/basin.prj",
                         "../data/rout_unit.con",
                         "../data/pareto_fitness.txt")
     
     checkFiles <- sapply(required_files, function(file) file.exists(file))
     
     if(all(checkFiles)& file.exists("../input/object_names.RDS")){run_prep_possible$files_avail = T}
     
     output$fileStatusMessage <- renderText({
       if (all(checkFiles) & file.exists("../input/object_names.RDS")) {
         HTML("All Files found.")
         
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
  #cannot run if already exists, has to be deleted manually
  if (!file.exists("../input/var_corr_par.csv") | !file.exists("../input/hru_in_optima.RDS") | !file.exists("../input/all_var.RDS")) {
                 shinyjs::show(id = "runprep_show")
    observe({     if (run_prep_possible$files_avail) {  shinyjs::enable("runprep")} else{  shinyjs::disable("runprep")
     } })
    
    
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
       req(assigned_objnames,fit()) #fit() is proxy for file connection
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
        if(dp_done() & file.exists("../input/var_corr_par.csv")){
          tags$strong("The data preparation was successful. You can now continue with the Correlation and Principal Component Analysis. You will not need this tab again.")
        }else{verbatimTextOutput("rscriptcmd")}
        
        })
      output$rscriptcmd <- renderText({
        paste(script_output(), collapse = "\n")
      })
      
      observe({ 
      if(length(list.files(c(save_dir,output_dir), full.names = TRUE))==0){ #do not show reset option if there haven't been files uploaded
        shinyjs::hide(id="reset")
       
      }else{
      output$reset_prompt <- renderText({
        HTML(paste("<p style='color: red;'> If you would like to restart the app if it crashes or behaves inconsistently, you can hard reset it here. Clicking this button
                   deletes all files you provided. The contents of the Output folder are also deleted, please move or copy those files you would like to keep. For all changes to take effect please restart the app after each Hard Reset. Please proceed with caution!</p>"))
      })
      

      observeEvent(input$reset_btn, {
        if (dir.exists(save_dir) & dir.exists(input_dir)) {
          files1 <- list.files(save_dir, full.names = TRUE)
          files2 <- list.files(input_dir, full.names = TRUE)
          files3 <- list.files(output_dir, full.names = TRUE)
          
          sapply(files1, file.remove)
          sapply(files2, file.remove)
          sapply(files3, file.remove)
          
          remaining_files <- list.files(save_dir, full.names = TRUE)
          if (length(remaining_files) == 0) {
            status <- "All files have been deleted."
            
            file.copy("../data for container/nswrm_priorities.csv", input_dir, overwrite = TRUE)
            
            file.copy("../data for container/config.ini", input_dir, overwrite = TRUE)
            
            
          } else {
            status <- "Some files could not be deleted."
          }
        } else {
          status <- "Directory does not exist."
        }
        
        # Update the status text output
        output$reset_status <- renderText(status)
      })
      
      } })
      
  ### Correlation Analysis ####
      
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
        
        if (all(checkFiles) == F) {
          shinyjs::hide(id = "corr_content")
          shinyjs::show(id = "corr_notthere")
          shinyjs::hide(id = "corr_sidebar")
          neednames = ""
          whatsmissing = ""
          output$corr_notthere <- renderText({
            missing_files = required_files[!checkFiles]
            if ("../input/object_names.RDS" %in% missing_files && length(missing_files) != 1) {
              whatsmissing = "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>"
              
              missing_files <- missing_files[missing_files != "../input/object_names.RDS"]
              neednames = "To be able to proceed please also define the objective names in the previous tab."
              if ("../input/all_var.RDS" %in% missing_files){missing_files <- missing_files[! missing_files %in% "../input/all_var.RDS"]
              }
            } else if ("../input/object_names.RDS" %in% missing_files && length(missing_files) == 1){
              whatsmissing = "All files have been provided, please specify the objective names in the previous tab."
              
            } else if ("../input/all_var.RDS" %in% missing_files && length(missing_files) != 1){
              missing_files <- missing_files[! missing_files %in% "../input/all_var.RDS"]
              
              whatsmissing = "The following file(s) are missing and have to be provided in the Data Prep tab:<br/>"
              neednames = "Please also define the objective names in the previous tab."
              
            }else if ("../input/all_var.RDS" %in% missing_files && length(missing_files) == 1){
              missing_files <- missing_files[missing_files != "../input/all_var.RDS"]
              
              neednames = "Please (re)run the Data Preparation in the previous tab."
              
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
      
      
      ## actual CORRELATION tab
      observeEvent(input$tabs == "correlation_analysis",{ 
        if(file.exists("../data/measure_location.csv")) {
          mes = read.csv("../data/measure_location.csv")
          mes <<- unique(mes$nswrm)

          nm <<- length(mes)
        }
        
        ## pull corr from file
        if(file.exists("../output/correlation_matrix.csv")){
          
          corr <<- read.csv("../output/correlation_matrix.csv", row.names = 1) #global because of re-rendering of plot

          ## correlation Plot (does not change for different thresholds)
          output$corrplot <- renderPlot({plt_corr(corr)})
        }else{ shinyjs::hide("show_conf")}
        
        } )
      
      output$download_corr_plot <- downloadHandler(
        filename = function() {
          curt = format(Sys.time(), "_%Y%m%d")
          
          paste(input$corr_plot_savename,curt, ".png", sep = "")
        },
        content = function(file) {
          png(file, width = 1500, height=1000)
          plt_corr(corr)
          dev.off()
        }
      )
      
      
        ## make new corr
      observeEvent(input$run_corr,{
          shinyjs::show("show_conf") #show confirm selection button once correlation has run
        
          req(input$selements)
          all_var <<- readRDS("../input/all_var.RDS")
          
          write_corr(vars = input$selements,cor_analysis = T, pca = F)
          
          check_align()#run a short check if all var_corr_par are in ini (sometimes they don't pass convert_optain) 
          
          ## run the Python script
          py_script <- "../python_files/correlation_matrix.py"
          cmd <- paste("python", py_script)
          
          ## capture python output
          result <- system(cmd, intern = TRUE)
    
          corr <<- read.csv("../output/correlation_matrix.csv", row.names = 1) #global because of re-rendering of plot
          output$corrplot <- renderPlot({plt_corr(corr)})
          
          
  ## events tied to a change in threshold, however also tied to change in selected variables, therefore also observe run
  observeEvent(input$thresh,{
    
    # reprint highest correlation table marking removed 
    output$corrtable <- renderDT({
      req(corr)
      
      find_high_corr(corr,threshold=input$thresh, tab=T, strike=NULL)}) #tab = T means this returns the full table, =F is for pulling variables
   
    # top table with selected elements
    output$selements <- renderTable({
      
      sel_vars = input$selements
      opt_pca <- optain_pca_content[sel_vars]
      
      data.frame(Variable = sel_vars,Description = opt_pca)
      
      },colnames = F)
  })
   # Drop Down Menu with subset of those with selected threshold
    observe({updateSelectInput(session, "excl",choices = find_high_corr(corr,threshold=input$thresh, tab=F))})
  })
  
  ## on clicking confirm selection the config ini is updated
  observeEvent(input$confirm_selection,{
    pca_remove(input$excl)
   
    output$corrtable <- renderDT({
      datatable(find_high_corr(corr,threshold = input$thresh,tab = T,strike = input$excl),escape = FALSE)}) #tab = T means this returns the full table, =F is for pulling variables
    
    if (is.null(pca_remove())) {
      pca_content = all_var
    } else{
      pca_content = all_var[-which(all_var %in% pca_remove())]
    }
    
    saveRDS(pca_content,file = "../input/pca_content.RDS") #required for PCA
    
    max_pca(get_num_pca()) #set max number of pca here (requires pca_content to exist)
    updateNumericInput(session, "pca_max", value = max_pca(), max=max_pca()) #requires pca_content to exist
    
    pca_in$data = pca_content
    
    write_corr(pca_content = pca_in$data,
               pca = T,
               cor_analysis = F)#this is also called into the pca tab on startup
    
    nonoval = paste(pca_remove(), collapse = ", ")
    
  # display confirmed selection in the Correlation Analysis tab
   if(is.null(pca_remove())){
     conf_text = HTML(paste0("All variables will be considered in the Clustering.","<br>"," If you change your mind please select variables above"))
   }else{conf_text =HTML(paste0("Removed variables: ","<b>", nonoval,"</b>")) }
   output$confirmed_selection <- renderText({conf_text})
  
   
   
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
    
    if(!file.exists("../input/pca_content.RDS") || !any(file.exists(list.files(path = output_dir, pattern = "correlation.*\\.csv$", full.names = TRUE)))
    ){shinyjs::hide("everything_cluster_sidebar")
      shinyjs::hide("everything_cluster_mainpanel")
     output$no_cluster <- renderText({HTML("Please run the correlation analysis first before proceeding with the clustering!")})
    }else{shinyjs::hide("no_cluster")
      shinyjs::show("everything_cluster_sidebar") #this is needed as previously turned off and somehow that sticks
      shinyjs::show("everything_cluster_mainpanel")
    }
    
    if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
   
    max_pca(get_num_pca())
    updateNumericInput(session, "pca_max", value = max_pca(), max=max_pca()) #requires pca_content to exist
      
    
    preselected = read_config_plt(obj = T, axis = F)
   
    choices = c("off", choices)
    all_choices(choices)

    isolate({axiselected(read_config_plt(obj = F, axis = T))})
    
    #update other plots including "off"
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
    
    #current selections
    selected1 <- input$element1
    selected2 <- input$element2
    selected3 <- input$element3
    selected4 <- input$element4
    
    #available choices for each dropdown
    choices1 <- setdiff(all_choices(), c(selected2, selected3, selected4))
    choices2 <- setdiff(all_choices(), c(selected1, selected3, selected4))
    choices3 <- setdiff(all_choices(), c(selected1, selected2, selected4))
    choices4 <- setdiff(all_choices(), c(selected1, selected2, selected3))
    
    #update the choices for each dropdown
    updateSelectInput(session, "element1", choices = choices1, selected = selected1)
    updateSelectInput(session, "element2", choices = choices2, selected = selected2)
    updateSelectInput(session, "element3", choices = choices3, selected = selected3)
    updateSelectInput(session, "element4", choices = choices4, selected = selected4)
  })
    
    observeEvent(input$confirm_axis,{ 
      pca_available$button1_clicked = TRUE
    
    isolate({axiselected(c(input$axisx,input$axisy,input$colour, input$size))})

    updateTextInput(session, "axisx",  value  = axiselected()[1])
    updateTextInput(session, "axisy", value = axiselected()[2])
    updateTextInput(session, "colour", value = axiselected()[3])
    updateTextInput(session, "size", value = axiselected()[4])
    
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
  observeEvent(input$set_choices,{
    pca_available$button2_clicked = TRUE
    
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
  
  ## confirm that axis is picked and label is written
  observe({
    if (pca_available$button1_clicked && pca_available$button2_clicked) {
      shinyjs::enable("runPCA")
      output$pca_available <- renderText("")  # Clear the notification
    } else {
      shinyjs::disable("runPCA")
      output$pca_available <- renderText({
        missing_buttons <- c()
        if (!pca_available$button1_clicked) missing_buttons <- c(missing_buttons, "Confirm Choice")
        if (!pca_available$button2_clicked) missing_buttons <- c(missing_buttons, "Confirm Axis Labels")

        paste("Please, ", paste(missing_buttons, collapse = " and ")," first!")
      })
    }
  })
  
  observeEvent(input$runPCA,{
    # python status
    output$pca_status <- renderText({pca_status()})
    pca_content <<- readRDS("../input/pca_content.RDS")
    
    output$pca_mess <- renderUI({
      HTML("
        <p>If all data was provided in the right format, the PCA outputs will open in separate windows - you can discard or save them as necessary.<br>
        The Silhouette Score is also provided -  you can use it to compare the cluster quality of multiple runs and you should generally aim for values of > 0.5</p>
      ")
      })
    
    isElementVisible(TRUE)
    
    ## prepare config.ini
    write_corr(pca_content = pca_content,pca=T, cor_analysis = F)# columns
    
    # command to run the Python script
    if(input$pcamethod=="k-means"){pca_script <- "../python_files/kmeans.py"}else{pca_script <- "../python_files/kmedoid.py"}
    
    run_python_script(path_script=pca_script,pca_status)
    
    })
  
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
  
  # align cluster number
  observe({
    minch = input$count_min
    maxch = input$count_max
    
    if(minch > maxch){
      updateNumericInput(session, "count_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "count_min", value = maxch)
    }
  })
  
  # align sd
  observe({
    minch = input$sd_min
    maxch = input$sd_max
    
    if(minch > maxch){
      updateNumericInput(session, "sd_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "sd_min", value = maxch)
    }
  })
  
  ## align number of pca
  observe({
    minch = input$pca_min
    maxch = input$pca_max
    
    if(minch > maxch){
      updateNumericInput(session, "pca_max",value=minch)
    }else if (maxch < minch) {
      updateNumericInput(session, "pca_min", value = maxch)
    }
  })
  
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
  
  ### Analysis Panel ####

  observeEvent(input$tabs == "analysis", { #this could be combined with the ahp tab for analysis
    
    if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
    
    preselected = read_config_plt(obj = T, axis = F)
    
    #update Analysis tab plot without "off"
    updateSelectInput(session, "x_var2",   choices = choices, selected = preselected[1])
    updateSelectInput(session, "y_var2",   choices = choices, selected = preselected[2])
    updateSelectInput(session, "col_var2", choices = choices, selected = preselected[3])
    updateSelectInput(session, "size_var2",choices = choices, selected = preselected[4])
    
    
    clus_out <- list.files(path = output_dir, pattern = "clusters_representativesolutions.*\\.csv$", full.names = TRUE)
    
    if(length(clus_out) == 0){
     shinyjs::hide("main_analysis")
     shinyjs::hide("plt_opti")
     shinyjs::runjs("toggleSidebar(false);")  #hide sidebar
     
     output$analysis_no_clustering <- renderText({HTML("the correlation analysis and the clustering have to run first before their results can be analysed")})
  
     }else{shinyjs::hide("analysis_no_clustering")
     shinyjs::runjs("toggleSidebar(true);")  #show sidebar
     shinyjs::show("main_analysis")
     shinyjs::show("plt_opti")
   }
      if(!file.exists("../input/object_names.RDS")) {
        shinyjs::hide(id = "analysis_random")
        shinyjs::hide(id= "meas_low")
        } 
 
      if(!file.exists("../data/measure_location.csv")){
        output$meas_low <- renderText({
          "measure_location.csv not found, please provide it in the data preparation tab."
        })}else{shinyjs::hide("meas_low")}
      
      observe({

        all_files <- list.files(output_dir, pattern = "clusters_representativesolutions.*\\.csv", full.names = TRUE)
        
        if(length(all_files)>1){
           file_info <- file.info(all_files)
           matching_files <-  all_files[which.max(file_info$mtime)]
        }else if(length(all_files)== 1){
          matching_files <- all_files
        }else{matching_files = NULL}
         
          check_files(matching_files)
        })

    observeEvent(check_files(),{
        if(!is.null(check_files())) {
          req(objectives())
          all_py_out <- file.info(check_files())

          current_py_out <- rownames(all_py_out)[which.max(all_py_out$mtime)]
          
          sols_data = read.csv(current_py_out)
          
          new_col_sol = c("optimum", objectives())
          
          sols(sols_data %>% rownames_to_column("optimum") %>%
                 dplyr::filter(!is.na(Representative_Solution)& Representative_Solution != "" & Representative_Solution != "outlier") %>% 
                 select(1:5) %>% #PCA content is hard to read/limited additional value for user
                 rename_with(~new_col_sol,everything()))
          
        }else{
          sols(data.frame(Message = "something went wrong - has the PCA run properly?"))
          # shinyjs::hide(id="plt_opti")
        }
        output$antab <- renderDT({
          req(sols())

          datatable(sols()%>%mutate(across(where(is.numeric), round, digits = 5)),
                    selection = list(mode = "multiple", target = 'row', max = 12), rownames= FALSE,
                    options = list(pageLength = 20, autoWidth = TRUE))
        })
      })
     
    output$par_plot_optima <- renderPlot({
      req(objectives(),sols(), input$x_var2)
      
      if(is.null(check_files())) { #sol is only useful if python has run
        sols(data.frame(Message = 'something went wrong - has the PCA run properly? You can check the output folder for files with names containing "cluster" or "representative solutions" or both '))
      }else{
         req(objectives(),sols())
        sol<<-sols()[,objectives()]
        
     
      if(!is.null(input$antab_rows_selected)){
        
        selected_row <- input$antab_rows_selected
        selected_data <- sols()[selected_row,]
        
      }else{selected_data <- NULL}
        
      plt_sc_optima(
        dat = sol,
        x_var = input$x_var2,
        y_var = input$y_var2,
        col_var = input$col_var2,
        size_var = input$size_var2,
        sel_tab = selected_data,
        add_whole = input$add_whole,
        an_tab = T,
        status_q = input$add_sq
      )
    }
    }) 
    
    output$download_par_plot <- downloadHandler(
      filename = function() {
        curt = format(Sys.time(), "_%Y%m%d")
        
        paste(input$par_plot_savename,curt, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 7, height = 5)
      }
    )
    
    output$tabtext = renderText({HTML("You can select up to 12 optima and compare the implementation of measures in the catchment.")})
    
    if(file.exists("../data/hru.shp")) {
      ##shp for location plot
      cm_clean(pull_shp_pure(layername = "basin"))
      ##shps for maps
      if (file.exists("../input/hru_in_optima.RDS")) {
        cm(
          pull_shp(
            layername = "hru",
            optims = sols(),
            hru_in_opt_path = "../input/hru_in_optima.RDS"
          )
        )
      }
      needs_buffer(pull_buffer())
    }
    if(file.exists("../data/hru.con")){lalo <<- plt_latlon(conpath = "../data/hru.con")}
   
  })
  
  comp_fun = function(){
    # if(!file.exists("../data/measure_location.csv")){return(NULL)}else{
    
    req(sols(),cm(),cm_clean()) #req is also used in functions
    selected_row <- input$antab_rows_selected
    
    selected_data <- sols()[selected_row,]
    
    buffs = needs_buffer()
    
    hru_sel = plt_sel(shp=cm(),opti_sel = selected_data$optimum)
    mes = read.csv("../data/measure_location.csv")
    
    col_sel = names(hru_sel)[grep("Optim",names(hru_sel))]  #variable length of columns selected
    
    nplots = length(col_sel)+1
    m1 = plt_lf(data=hru_sel, col_sel = col_sel, mes = unique(mes$nswrm),la = lalo[1],lo =lalo[2], buff_els=buffs)
    
    cm2 = plt_cm_pure(data=cm_clean(), la = lalo[1],lo =lalo[2])
    m <- c(list(cm2), m1)
    
    sync(m,sync = list(2:nplots),sync.cursor = F)
  }#}
  
  observeEvent(input$plt_opti,{
    selected_row <- input$antab_rows_selected
    
    if (is.null(selected_row)) {
      
      shinyjs::show(id = "no_row")
      output$no_row = renderText({paste("No row selected")})
    
    } else {
      shinyjs::hide(id = "no_row")
      is_rendering(TRUE) 
      output$comp_map <- renderUI({comp_fun()})
      
      output$plot_ready <- renderText({
        is_rendering(FALSE)  # Set rendering to FALSE after the plot is rendered
      })
      }
     
      # observe({
      #   print(names(input))
      # })
      # observe({
      #   all_ids <- names(input)
      #   bounds_id <- grep("^htmlwidget-[0-9]+_bounds$", all_ids, value = TRUE)
      #  
      #   numb <- bounds_id[1] #this is  "htmlwidget-1168_bounds"
      #  #UNSURE IF THIS CAN BE USED TO EXTRACT BOUNDS W/ NORTH AND WEST?
      #   output$mymap <- renderLeaflet({
      #     isolate({
      #       if ("mymap_center" %in% names(input)) {
      #         mapparams <- list(center = input$mymap_center,
      #                           zoom = input$mymap_zoom)
      #       } else {
      #         mapparams <- list(center = list(lng=0, lat=30),
      #                           zoom = 4) #setting the view over ~ center of North America
      #       }
      #     })
      #     leaflet() %>%
      #       setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom)  %>% 
      #       addTiles(options = providerTileOptions(noWrap = TRUE)) 
      #   })
      #   
      #   if (!is.null(bounds)) {
      #     leafletProxy("map2") %>%
      #       clearShapes() %>%
      #       addRectangles(
      #         lng1 = bounds$west, lat1 = bounds$south,
      #         lng2 = bounds$east, lat2 = bounds$north,
      #         fillOpacity = 0.2, color = "red", weight = 2
      #       )
      #   }
      #   
      # })
   
 
  })
  
  observe({
    shinyjs::toggle("plot_spinner", condition = is_rendering())
  })
  
  
  # output$download_meas_plot <- downloadHandler(
  #   filename = function() {
  #     paste(input$meas_plot_savename, ".html", sep = "")
  #   },
  #   content = function(file) {
  #     htmlwidgets::saveWidget(comp_fun(), file)
  #   }
  # )

  
  
  ### AHP ####
  observeEvent(input$tabs == "ahp",{
    if(!file.exists("../data/pareto_fitness.txt")){ #check if fit() has been created yet
      output$nothing_ran_ahp <- renderText({HTML("please provide the pareto_fitness.txt in either the Data Preparation or the Visualising the Pareto Front tab.")})
    }else{ shinyjs::hide("nothing_ran_ahp")
      shinyjs::runjs("toggleSidebar(false);")  # Hide sidebar
    }      
    if (!file.exists("../data/sq_fitness.txt")){shinyjs::disable("show_status_quo")}else{shinyjs::enable("show_status_quo")} 
      
      if(!file.exists("../input/object_names.RDS")) {
      choices = "Please select objectives in Data Preparation Tab"
      
      ids_to_hide <- c( "pareto_weighted", "random_ahp2", "random_ahp", "ahp_analysis", "ahp_weights","sel_wgt")
      
      lapply(ids_to_hide, shinyjs::hide)
    
    } else{
      choices = readRDS("../input/object_names.RDS")
    }
    ahp_choices(choices)
    preselect = ahp_choices()
    
    preselect1 = preselect[1]
    preselect2 = preselect[2]
    preselect3 = preselect[3]
    preselect4 = preselect[4]
    
    updateSelectInput(session,inputId = "x_var",choices = choices,selected = preselect1)
    updateSelectInput(session,inputId = "y_var",choices = choices, selected = preselect2)
    updateSelectInput(session,inputId = "col_var", choices = choices, selected = preselect3)
    updateSelectInput(session,inputId = "size_var", choices = choices, selected = preselect4)
    
    })
  
  
  criteria_choices <- reactive({
    req(fit())
    colnames(fit())
  })

  # Generate the UI for selecting the first criterion dynamically based on the uploaded data
  output$criterion1_ui <- renderUI({
    choices <- criteria_choices()
    selectInput("criterion1", "Select the first objective (x-axis)", choices = choices)
  })

  ## select the second criterion dynamically based on the uploaded data
  output$criterion2_ui <- renderUI({
    req(input$criterion1)
    choices <- setdiff(criteria_choices(), input$criterion1)
    selectInput("criterion2", "Select the second objective (y-axis)", choices = choices)
  })

  observeEvent(input$criterion1, {
    updateSelectInput(session, "criterion2", choices = setdiff(criteria_choices(), input$criterion1))
  })


  observeEvent(list(input$criterion2,input$criterion1),{# input$plot_sc,{
    req(fit(),input$criterion1, input$criterion2)
    
    x <- fit()[,input$criterion1]
    y <- fit()[,input$criterion2]
    
    ## linear model
    model <- lm(y ~ x)
  
    output$scatterPlot <- renderPlot({
      
      plt_scat2(dat= fit(), x= input$criterion1, y=input$criterion2)

    })
    
    output$relation <- renderTable({
      metrics_df <- data.frame(
        Metric = c("R<sup>2</sup>", "Pearson's r"),  # HTML for R²
        Value = c(round(summary(model)$r.squared, 3), round(cor(x, y), 3))
      )
    
      metrics_df
    }, rownames = F, colnames = F, sanitize.text.function = function(x) x)
  })
  

  ## AHP sliders
  output$sliders_ui <- renderUI({
    req(objectives())
    sliders <- list()
    num_criteria <- length(objectives())
    
    for (i in 1:(num_criteria - 1)) {
      for (j in (i + 1):num_criteria) {
        slider_id <- paste0("c", i, "_c", j)
        sliders[[slider_id]] <- sliderTextInput(
          inputId = slider_id,
          label =paste0(objectives()[j]," vs. ",objectives()[i]),  # No label for the slider
          choices = c(paste0(objectives()[j]," - ",9:2),"Equal",paste0(2:9," - ",objectives()[i])),
          
          selected = "Equal",
          grid = TRUE,
          hide_min_max = FALSE,
          animate = FALSE,
          width = "100%"
          
        )
      }
    }
    
    do.call(tagList, sliders)
  })
  
  calculate_weights = reactive({
    req(objectives())
    n_criteria <- length(objectives())
    
    comparison_matrix <- matrix(1, nrow = n_criteria, ncol = n_criteria,dimnames = list(objectives(),objectives()))
    
    for (i in 1:(n_criteria - 1)) {
      for (j in (i + 1):n_criteria) {
        slider_id <- paste0("c", i, "_c", j)
        
        value = input[[slider_id]]
        if(is.null(value) || value=="Equal"){
          comparison_value =1
          comparison_matrix[j, i] <- comparison_value
          comparison_matrix[i, j] <- 1 / comparison_value
        }else{ 
        parts <- strsplit(value, " - ")[[1]]
        
        if (length(parts) == 2) {
            # first part is numeric
          if (grepl("^\\d+$", parts[1])) {
            comparison_value <- as.numeric(parts[1])
            comparison_matrix[i, j] <- comparison_value
            comparison_matrix[j, i] <- 1 / comparison_value
            
          } else {
            # first part is objective
            comparison_value <- as.numeric(parts[2])
            comparison_matrix[j, i] <- comparison_value
            comparison_matrix[i, j] <- 1 / comparison_value
          }
        }}
      }
    }
    
      coma(comparison_matrix)
      
      normalized_matrix <- comparison_matrix / colSums(comparison_matrix)

      weights <- rowMeans(normalized_matrix)

      weights <- weights/sum(weights)
      
      weights
  })

 
  output$weights_output <- renderTable({
                                       req(calculate_weights())
                                       wgt=(t(calculate_weights()))
                                       wgt
                                       }, colnames = T)
  
    output$best_option_output <- renderTable({
      
    req(sols(),fit(),initial_update_done(),range_controlled())
      
    if (input$best_cluster) {
      df = subset(sols(),select= -optimum) #best option out of optima
      
      df = match_abs(minval=c(input$obj1_ahp[1],input$obj2_ahp[1], input$obj3_ahp[1], input$obj4_ahp[1]),
                     maxval=c(input$obj1_ahp[2],input$obj2_ahp[2], input$obj3_ahp[2], input$obj4_ahp[2]),
                     abs_tab = df, ranger = range_controlled())

    } else{
        #best option out of whole pareto front (=default)
      df = match_abs(minval=c(input$obj1_ahp[1],input$obj2_ahp[1], input$obj3_ahp[1], input$obj4_ahp[1]),
                     maxval=c(input$obj1_ahp[2],input$obj2_ahp[2], input$obj3_ahp[2], input$obj4_ahp[2]),
                     abs_tab = fit(), ranger = range_controlled())
    }
    
    if (!all(names(calculate_weights()) %in% colnames(df))) {
      return("Dataframe columns do not match criteria names.")
    }

      
    if(nrow(df)==0 || ncol(df) == 0){
      bo = "None of the points fulfill these criteria. Please select different data ranges!"
    }else{  
    
    
    weights <- calculate_weights()
    
    #calculate max score based on scaled values
    min_fit <- apply(fit(), 2, min)
    max_fit <- apply(fit(), 2, max)
    
    df_sc <- as.data.frame(mapply(function(col_name, column) {
      rescale_column(column, min_fit[col_name], max_fit[col_name])
    }, colnames(fit()), df, SIMPLIFY = FALSE))
    
    # Calculate the final score based on the principal components
    df$Score <- rowSums(df_sc * weights)
    
    best_option_index <<- which.max(df$Score)
    df$Score <- NULL #drop Score column
    best_option(df[best_option_index, ])
    
    bo = best_option()
    
    }
    bo
  })
  
 
  observe({
    
  output$weights_plot <- renderPlot({
    req(objectives(),fit(),best_option(),input$x_var,sols(),range_controlled())
    sol<<-sols()[,objectives()]
    bo = best_option()
    df = match_abs(minval=c(input$obj1_ahp[1],input$obj2_ahp[1], input$obj3_ahp[1], input$obj4_ahp[1]),
                   maxval=c(input$obj1_ahp[2],input$obj2_ahp[2], input$obj3_ahp[2], input$obj4_ahp[2]),
                   abs_tab = fit(), ranger = range_controlled())
    
    plt_sc_optima(dat=df,x_var=input$x_var,y_var=input$y_var,
                  col_var=input$col_var,size_var=input$size_var,high_point=bo,extra_dat = sol,
                  plt_extra = input$show_extra_dat, status_q = input$show_status_quo,an_tab = T)
    
    })
  
  output$download_weights_plot <- downloadHandler(
    filename = function() {
      curt = format(Sys.time(), "_%Y%m%d")
      
      paste(input$weights_plot_savename,curt, ".png", sep = "")
    },
    content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 7, height = 5)
    }
  )
  })
  
  
  observe({
    req(coma(), calculate_weights())
    
    ## consistency checks
    ci = consistency_index(coma())

    cr = ci/0.89 #value found online, determined through random matrices
    
  output$consistency_check = renderText({

    slider_ids =c(input[["c1_c2"]],input[["c1_c3"]],input[["c1_c4"]],input[["c2_c3"]],
                  input[["c2_c4"]],input[["c3_c4"]])
    
    se = sum(slider_ids == "Equal") #if majority on equal, large preferences amplify mathematical inconsistency
    
    
    if(se > 3){inconsistencies = paste("No major inconsistencies.")}else if(cr <= 0.15){
      inconsistencies = paste("No major inconsistencies, the inconsistency ratio is:", round(cr, 3))
    }else{inconsistencies = paste("Potential inconsistencies, the inconsistency ratio is:", round(cr, 3))}
    
    inconsistencies
  })
  
  })
  
  observeEvent(input$plt_bo,{
  req(best_option(),needs_buffer())
    
  bo = best_option() 
  bo = bo %>% rownames_to_column("optimum")
   
    ##shps for maps
    if (file.exists("../input/hru_in_optima.RDS")) {
      cm(
        pull_shp(
          layername = "hru",
          optims = bo,
          hru_in_opt_path = "../input/hru_in_optima.RDS"
        )
      )
    }

    if(file.exists("../data/hru.con")){lalo <<- plt_latlon(conpath = "../data/hru.con")}
     needs_buffer(pull_buffer())
  
  
  single_meas_fun = function(){
    req(bo,needs_buffer(),lalo,cm())
    
    hru_one = plt_sel(shp=cm(),opti_sel = bo$optimum)
    mes = read.csv("../data/measure_location.csv")
    
    col_sel = names(hru_one)[grep("Optim",names(hru_one))] 

    m1 = plt_lf(data=hru_one,  mes = unique(mes$nswrm),la = lalo[1],lo =lalo[2],
                buff_els=needs_buffer(),col_sel=col_sel)

    return(m1)

    }

    output$plt_bo_measure = renderUI({single_meas_fun()})
  })
}