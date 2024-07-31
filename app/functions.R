######################## FUNCTIONS #################################
# comments: 
# Project: Clustering Pareto Solutions/Multi-objective visualisation
####################################################################

#### Correlation Analysis Functions ####

## correlation plot
plt_corr = function(corr,labelcol = "black",labelorder='alphabet',meth = 'square', tpe = 'lower'){
corma = as.matrix(corr)
plt = corrplot(corma, method = meth, order =labelorder,tl.col = labelcol, type=tpe,diag=FALSE)
return(plt)
}

## correlation table
find_high_corr <- function(cor_matrix, threshold = 0.75, tab = T,strike = NULL) {
  var_names <- colnames(cor_matrix)
  
  # empty dataframe
  high_cor_pairs <- data.frame(
    variable1 = character(),
    variable2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(nrow(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > abs(threshold)) {
        high_cor_pairs <- rbind(high_cor_pairs, data.frame(
          variable1 = var_names[i],
          variable2 = var_names[j],
          Correlation = cor_matrix[i, j],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  higg = high_cor_pairs %>% 
    arrange(desc(abs(Correlation)))%>%mutate(Correlation = round(Correlation,2))
  
  if(!is.null(strike)){
    higg[which(higg$variable1 %in% strike,arr.ind = T),] = sapply(higg[which(higg$variable1 %in% strike,arr.ind = T),], strike_through)
    higg[which(higg$variable2 %in% strike,arr.ind = T),] = sapply(higg[which(higg$variable2 %in% strike,arr.ind = T),], strike_through)
    
  }
  yolo = unique(c(higg$variable1,higg$variable2)) #used to fill drop down menu
  
  if(tab==T){return(higg)}else(return(yolo))
}


#### Write Config Functions ####

## pca and correlation update
write_corr = function(vars,
                      measures = mes,
                      cor_analysis = F,
                      pca_content = all_var,
                      pca = T) {
  config <- read.ini("../input/config.ini")
  if (cor_analysis && pca) {
    stop("cannot write both PCA and Correlation ini at the same time, set pca = T OR cor_analysis = T, not both")
  }
  if (cor_analysis) {
    varmes = NULL
    
    if ("moran" %in% vars) {
      varmes = append(varmes, paste(mes, "moran", sep = "_"))
    }
    if ("linE" %in% vars) {
      varmes = append(varmes, "linE") #linE is the only variable that is not calculated for each measure
    }
    if ("sit" %in% vars) {
      varmes = append(varmes, paste(mes, "sit", sep = "_"))
    }
    if ("siim" %in% vars) {
      varmes = append(varmes, paste(mes, "siim", sep = "_"))
    }
    
    config[[1]]$col_correlation_matrix = paste(varmes, collapse = ", ")
    
    write.ini(config, "../input/config.ini")
  } 
  if (pca) {
    config[[1]]$columns = paste(pca_content, collapse = ", ")
    write.ini(config, "../input/config.ini")
    
  }
  
}

##
write_pca_ini <- function(var1 = "", var2 = "", var3 = "", var4 = "",
                          var1_lab= "", var2_lab = "", var3_lab = "", var4_lab = "") {
  config <- read.ini("../input/config.ini")
  
  config[[5]]$var_1 <- ifelse(var1 == "off", "", var1)
  config[[5]]$var_2 <- ifelse(var2 == "off", "", var2)
  config[[5]]$var_3 <- ifelse(var3 == "off", "", var3)
  config[[5]]$var_4 <- ifelse(var4 == "off", "", var4)
  
  off_count <- sum(c(var1, var2, var3, var4) == "off")
  config[[5]]$num_variables_to_plot <- 4 - off_count
  config[[5]]$var_1_label <- ifelse(var1_lab == "off", "", var1_lab)
  config[[5]]$var_2_label <- ifelse(var2_lab == "off", "", var2_lab)
  config[[5]]$var_3_label <- ifelse(var3_lab == "off", "", var3_lab)
  config[[5]]$var_4_label <- ifelse(var4_lab == "off", "", var4_lab)
  write.ini(config, "../input/config.ini") 
  
}

##
write_quali_ini = function(var1 = "", var2 = "", var3 = "", var4 = ""){
   config <- read.ini("../input/config.ini")
  
    conf_clust = NULL
    if (var1 != "off") {
      conf_clust = append(conf_clust, var1)
    }
    if (var2 != "off") {
      conf_clust = append(conf_clust, var2)
    }
    if (var3 != "off") {
      conf_clust = append(conf_clust, var3)
    }
    if (var4 != "off") {
      conf_clust = append(conf_clust, var4)
    }
    
    config$Qualitative_Clustering$qualitative_clustering_columns = paste(conf_clust, collapse = ", ")
    write.ini(config, "../input/config.ini")

}

##
write_cluster<- function(min_cluster=0,max_cluster=0,fixed_cluster_boolean="true",fixed_clusters=7){
  config <- read.ini("../input/config.ini")
  
  config[[2]]$fixed_clusters_boolean = fixed_cluster_boolean
  config[[2]]$fixed_clusters = fixed_clusters
  
  config[[2]]$min_clusters = min_cluster
  config[[2]]$max_clusters = max_cluster
  
  write.ini(config, "../input/config.ini")
}

##
write_outl <- function(handle_outliers_boolean="false",deviations_min=3,deviations_max=3,
                        count_min=3,count_max=3,outlier_to_cluster_ratio=0.5){
  config <- read.ini("../input/config.ini")
  
  config[[3]]$handle_outliers_boolean = handle_outliers_boolean
  config[[3]]$deviations_min = deviations_min
  config[[3]]$deviations_max = deviations_max
  config[[3]]$count_min = count_min
  config[[3]]$count_max = count_max
  config[[3]]$outlier_to_cluster_ratio = outlier_to_cluster_ratio
  
  write.ini(config, "../input/config.ini")
  
}

## 
write_pcanum = function(pcamin,pcamax){
  config <- read.ini("../input/config.ini")
  
  config[[4]]$min_components = pcamin
  config[[4]]$max_components = pcamax
  write.ini(config, "../input/config.ini")
  
  
}

#### Read Config Functions ####

## config for pca on startup
read_pca = function(){
  config <- read.ini("../input/config.ini")
  
  if(!is.null(config[[1]]$columns)){pca_col_incl <- unlist(strsplit(config[[1]]$columns,", "))
  pca_col <- pca_col_incl[order(pca_col_incl)]}else{pca_col <- NULL}
  
  return(pca_col)
}

## 
read_config_plt = function(obj=T,axis=F){
  config <- read.ini("../input/config.ini")
  
  if(obj){
  var1 = config[[5]]$var_1
  var2 = config[[5]]$var_2
  var3 = config[[5]]$var_3
  var4 = config[[5]]$var_4}else if(axis){
    var1 = config[[5]]$var_1_label
    var2 = config[[5]]$var_2_label
    var3 = config[[5]]$var_3_label
    var4 = config[[5]]$var_4_label
  }else{stop("either obj or axis has to be TRUE")}
  
  return(c(var1,var2,var3,var4))
  
}


#### Table/Output Formatting Functions ####

## count number of decimals
num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}

## formatting with strike through
strike_through <- function(x) {
  sprintf('<span style="text-decoration: line-through;">%s</span>', x)
}

## extract objective ranges
get_obj_range = function(filepath = "../data/pareto_fitness.txt",colnames=paste0("objective", seq(1, 4))){
  stopifnot(is.character(filepath))
  
  pf = read.table(filepath,sep=",")
  colnames(pf) = colnames
  
  range_df <- data.frame(column = character(), min = numeric(), max = numeric(), stringsAsFactors = FALSE)
  
  for (col_name in colnames(pf)) {
    min_val <- min(pf[[col_name]], na.rm = TRUE)
    max_val <- max(pf[[col_name]], na.rm = TRUE)
    range_df <- rbind(range_df, data.frame(column = col_name, min = min_val, max = max_val, stringsAsFactors = FALSE))
  }
  return(range_df)##
}

#### Python Caller ####

run_python_script <- function(path_script="",pca_status) {
  p <- processx::process$new(
    "python",
    c(path_script),
    stdout = "|", stderr = "|"
  )
  
  while (p$is_alive()) {
    new_output <- p$read_output_lines()
    new_output <- c(new_output, p$read_error_lines())
    if (length(new_output) > 0) {
      pca_status(paste(pca_status(), paste(new_output, collapse = "\n"), sep = "\n"))
    }
    Sys.sleep(0.1)
  }
  
  final_output <- p$read_all_output_lines()
  final_output <- c(final_output, p$read_all_error_lines())
  if (length(final_output) > 0) {
    pca_status(paste(pca_status(), paste(final_output, collapse = "\n"), sep = "\n"))
  }
}

#### Plotting the optima
## get linear elements requiring a buffer
pull_buffer = function(prios= "../input/nswrm_priorities.csv"){
  priodf = read.csv(prios)
  strct_obj = priodf%>%filter(mngmt ==0)%>%select(nswrm)%>%pull()#structural measures
  return(strct_obj)
}

## get map extent
plt_latlon = function(conpath){
  conny = read.table(conpath,skip = 1,header = T)
  lon_map = mean(conny$lon)
  lat_map = mean(conny$lat)
  return(c(lat_map,lon_map))
}


## make large dataset
pull_shp = function(layername = "hru", optims){
  if(file.exists(paste0("../data/",layername,".shp"))){
    
    hio = readRDS("../input/hru_in_optima.RDS")
    hio = hio %>% rename_with( ~ str_remove(., "^V"), starts_with("V"))
    hio = hio %>% select(optims[["optimum"]], id)#subset to only optima remaining after clustering
    
    
    cm = read_sf(dsn = "../data/", layer = layername) #adapt path
    cm = cm %>% filter(id %in% hio$id)#remove all hrus that are never activated in this pareto front
    
    cm = st_buffer(cm, 0.0) #clean geometry
    cm = cm %>%select(id,geometry)
    
    cm = left_join(cm, hio, by = c("id")) %>% st_transform(., 4326)
    
    return(cm)}else{return(NULL)}
  
}

## merge hrus with optima
plt_sel = function(opti_sel, shp){

  plt_sel = shp %>% dplyr::select(id, geometry, all_of(opti_sel)) %>% rename(optim =
                                                                            opti_sel)
  plt_sel = st_make_valid(plt_sel) # too slow annoyingly
  
  return(plt_sel)
}

## plot leaflet
plt_lf = function(dat=hru_sel,cols="optim", mes, lo, la, buff_els){#hru_sel created with plt_sel() and buff_els created with pull_buffer()
  # st_agr(dat) <- "constant" # Set spatial attribute relationship for speed-up

  dispal = colorFactor("Spectral", domain = mes, na.color = "lightgrey")

 lf_map <- leaflet(data = dat) %>%
   setView(lng = lo, lat = la, zoom = 11) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%

    addPolygons(fillColor = ~dispal(dat[[cols]]), fillOpacity = 0.7,
                color = "lightgrey", weight = 1,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = ~dat[[cols]]) %>%
    addLegend("bottomright", pal = dispal,
              title = "measures",values=mes)


 relevant_data <- dat[dat[[cols]] %in% buff_els, ]

 buffered_data <- st_buffer(relevant_data, dist = 80)

 lf_map <- lf_map %>%
   addPolygons(data = buffered_data,
               fillColor = NA,
               color = ~dispal(relevant_data[[cols]]), weight = 1, dashArray = "3",
               fillOpacity = 0.2,
               highlightOptions = highlightOptions(color = ~dispal(relevant_data[[cols]]), weight = 2, bringToFront = TRUE))

 return(lf_map)
  }

#### Plotting the exploration tab

## barplot THIS NEEDS DYNAMIC TESTING FOR NEG VALUES!
prep_diff_bar = function(abs_tab,red_tab,allobs, neg_var=NULL){
  #full dataset/all optima
  
  absk = abs_tab%>%pivot_longer(everything())%>%group_by(name)%>%
    summarise(min = min(value),
              max = max(value),
              median = median(value),
              mean = mean(value))%>% 
    column_to_rownames(var = "name")
  
  
  #reduced (new) table of optima  
  redk = red_tab%>% pivot_longer(everything())%>%group_by(name)%>%
    summarise(min = min(value),
              max = max(value),
              median = median(value),
              mean = mean(value))%>% 
    column_to_rownames(var = "name")
  
  
  #dumbest way possible for doing this
  pct = as.data.frame(array(NA,dim = c(4,length(allobs))),row.names = rownames(redk))
  colnames(pct) = allobs
  colnames(pct) == colnames(redk)
  
  pct = 100*(redk-absk)/absk
  
  # correcting for values on a negative scale 
  if(!is.null(neg_var)){
  pct[neg_var,] = (pct[neg_var,])*-1}
  
  
  return(pct)
}


## plot (return of prep_diff_bar)
plot_diff_bar= function(pct,obj_choices=NULL){
  pl2 <- pct %>% rownames_to_column(var = "objective")  %>%mutate(objective=factor(objective)) %>%
    mutate(objective=forcats::fct_relevel(objective,obj_choices))%>%
    pivot_longer(-objective)%>%
    
    ggplot(aes(x = name, y = value, fill = objective)) +
    geom_bar(position = "dodge",
             stat = "identity",
             alpha = 0.75) + labs(x = 'Objective', y = 'percentage change (%)') +
    theme_bw() +
    theme_minimal() +
    scale_fill_manual(values = c( "#FFC61E", "#009ADE","#AF58BA", "#F28522", "#FF1F5B")) +
    geom_text(aes(label = objective),size=10,
              colour = "black",
              position = position_dodge(width = 1), vjust = -0.5) +
    theme(
      plot.title = element_text(size = 19L),
      axis.text.y = element_text(size = 17L),
      axis.text.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.title.x = element_blank(),
      legend.position = "none"
    ) +
    theme(plot.background = element_rect(fill = NA, color = NA))+scale_y_continuous(limits=c(-10,10))
  
  return(pl2)
}


## parallel axis plot
plot_parline = function(datt,sizz=rep(.5, length(unique(datt$id))),colols=rep("grey50", length(unique(datt$id)))){
  
  pl1 <- ggplot(datt, aes(x = name, y = value,group=id,size=id, color=id)) +   # group = id is important!
    geom_line(
      aes(group = id),
      alpha = 0.5,
      lineend = 'round',
      linejoin = 'round'
    ) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 19L),
          axis.text.y = element_text(size = 15L),
          axis.text.x = element_text(size = 16L),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_blank()
    )+scale_y_continuous(limits = c(0,1))+
    scale_x_discrete(expand = c(0.04, 0.05)) + labs(x = "Factors", y = "Scaled Values")+
    scale_size_manual(values = sizz) +
    scale_color_manual(values = colols)
  
  return(pl1)
  
}

#### Other Functions ####

## default max number of pc
get_num_pca <- function() {
  pcc <- readRDS("../input/pca_content.RDS")
  return(length(pcc))
}


## display selected pca settings
pca_settings = function(input){
  settings <- paste0("<ul>",
                     "<li><strong>",input$element1,"</strong> is shown on the x-axis","</li>",
                     "<li>", "The x-axis label is: \"<strong>",input$axisx,"</strong>\"</li>",
                     "<li><strong>", input$element2,"</strong> is shown on the y-axis", "</li>",
                     "<li>", "The y-axis label is: \"<strong>",input$axisy,"</strong>\"</li>",
                     "<li>", "The colour hue is defined by <strong>", input$element3, "</strong></li>",
                     "<li>", "The colour label is: \"<strong>",input$colour,"</strong>\"</li>",
                     "<li>", "The size of the data points is defined by: <strong>", input$element4, "</strong></li>",
                     "<li>", "The size label is: \"<strong>",input$size,"</strong>\"</li>",
                     "<li>", "A range of <strong>",input$pca_min,"</strong> to <strong>",input$pca_max,"</strong> principal components is tested.","</li>","</ul>"
  )
  
  # conditional settings
  if (input$clusyn == "Yes" &
      input$outlyn =="No") {
    #only cluster
    clus <- paste0(
      "<ul>",
      "<li>",
      "A range of <strong>",
      input$clus_min,
      "</strong> to <strong>",
      input$clus_max,
      "</strong> clusters is tested.",
      "</li>",
      "</ul>"
    )
    settings <- paste(settings, clus, collapse= "<br>")
  }else if (input$clusyn == "No" & input$outlyn == "No"){
    clus <- paste0(
      "<ul>", "<li>","Using a fixed number of <strong>",input$clus_fix,"</strong> of clusters","</li>",
      "<li> Outliers are not considered </li></ul>")
    settings <- paste(settings, clus, collapse= "<br>")
  } else if (input$clusyn == "Yes" & input$outlyn == "Yes") {
    #both
    clus <- paste0(
      "<ul>",
      "<li>",
      "A range of <strong>",
      input$clus_min,
      "</strong> to ",
      input$clus_max,
      "</strong> clusters is tested.",
      "</li>",
      "</ul>"
    )
    outly <- paste0(
      "<ul>",
      "<li>",
      "A range of <strong>",
      input$count_min,
      "</strong> to <strong>",
      input$count_max,
      "</strong> extreme variables is tested for removing clusters.",
      "</li>",
      "<li>",
      "The standard deviations tested range from <strong>",
      input$sd_min,
      "</strong> to <strong>",
      input$sd_max,
      "</strong></li>",
      "<li>","The tested ratio of number of ouliers to cluster size is: <strong> ",input$outlier_ratio, "</strong></li>",
      "</ul>"
    )
    settings <- paste(settings, clus, outly, collapse = "<br> ")
  } else if (input$clusyn == "No" & input$outlyn == "Yes") {
    outly <- paste0(
      "<ul>",
      "<li>","Using a fixed <strong>",input$clus_fix,"</strong> of clusters","</li>",
      "<li>",
      "A range of <strong>",
      input$count_min,
      "</strong> to <strong>",
      input$count_max,
      "</strong> extreme variables is tested for removing clusters.",
      "</li>",
      "<li>",
      "The standard deviations tested range from <strong>",
      input$sd_min,
      "</strong> to <strong>",
      input$sd_max,
      "</strong></li>",
      "<li>","The tested ratio of number of ouliers to cluster size is: <strong> ",input$outlier_ratio, "</strong></li>",
      "</ul>"
    )
    settings <- paste(settings, outly,collapse = "<br>")}
    
  return(settings)
}

## return the original value and the position of scaled value in the original dataset
scaled_abs_match = function(minval_s=c(0,0,0,0),
                            maxval_s=c(1,1,1,1),
                            scal_tab=NULL,abs_tab=NULL,
                            allobs=NULL,smll = TRUE){ 
  
  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  
  # locate values in scaled dataframe 
  for(i in 1:length(allobs)){
    
    sca_max <- scal_tab[which.min(abs(scal_tab[[allobs[i]]]-maxval_s[i])),]
    df["max",allobs[i]] = abs_tab[rownames(sca_max),allobs[i]]  
    
    sca_min <- scal_tab[which.min(abs(minval_s[i]-scal_tab[[allobs[i]]])),]
    df["min",allobs[i]] = abs_tab[rownames(sca_min),allobs[i]]
  }
  
  # consider interactions between objectives (some are not attainable anymore)
  # reduced dataframe of absolute values within all objective ranges
  ch = abs_tab
  
  for(k in 1:length(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<= valma & .data[[allobs[k]]]>=valmi)
  }
  
  # retain only min and max
  cw = as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(cw) = allobs
  
  
  for (l in 1:length(allobs)) {
    if(length(ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1 ||
       length(ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1) {
      cw["max", allobs[l]] = NA
      cw["min", allobs[l]] = NA
    }else{
      
      cw["max", allobs[l]] = ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
      cw["min", allobs[l]] = ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
    }
    
    
  }
  #when smll is set to false the table with all absolute values is returned
  if(smll){return(cw)}else{return(ch)} 
  
}


## similar to ch in scaled_abs_match, matching input scaled data with a scaled dataframe
match_scaled = function(minval_s=c(0,0,0,0),
                        maxval_s=c(1,1,1,1),
                        scal_tab = NULL,
                        allobs=c('HC', 'HQ', 'P', 'AP')){
  
  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  
  # locate values in scaled dataframe 
  for(i in 1:length(allobs)){
    
    sca_max <- scal_tab[which.min(abs(scal_tab[[allobs[i]]]-maxval_s[i])),]
    df["max",allobs[i]] = scal_tab[rownames(sca_max),allobs[i]]  
    
    sca_min <- scal_tab[which.min(abs(minval_s[i]-scal_tab[[allobs[i]]])),]
    df["min",allobs[i]] = scal_tab[rownames(sca_min),allobs[i]]
  }
  
  # surely this should be easier to achieve...
  ch = scal_tab
  
  for(k in 1:length(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<valma & .data[[allobs[k]]]>valmi)
  }
  if(dim(ch)[1]==0){stop("no optima fulfill these conditions")}else{return(ch)}
  
}

## find row of data containing specific value in specific column
find_row = function(dat, colname, val, absdat){
  
  closest_index <- which.min(abs(dat[[colname]] - val))
 
  return(absdat[closest_index, ]) #this works as fit and f_scaled have the same origin
}