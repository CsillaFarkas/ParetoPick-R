################### FUNCTIONS #####################

#### Correlation Analysis Functions ####

## plot
plt_corr = function(corr,labelcol = "black",labelorder='alphabet',meth = 'square', tpe = 'lower'){
corma = as.matrix(corr)
plt = corrplot(corma, method = meth, order =labelorder,tl.col = labelcol, type=tpe,diag=FALSE)
return(plt)
}

## table
find_high_corr <- function(cor_matrix, threshold = 0.75, tab = T,strike = NULL) {
  # Get the row and column names of the correlation matrix
  var_names <- colnames(cor_matrix)
  
  # Initialize an empty data frame to store the pairs
  high_cor_pairs <- data.frame(
    variable1 = character(),
    variable2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(nrow(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > abs(threshold)) {
        # Add the pair and the correlation to the data frame
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
  
  if(!is.null(config[[1]]$columns)){pca_col_incl = unlist(strsplit(config[[1]]$columns,", "))
  pca_col = pca_col_incl[order(pca_col_incl)]}else(pca_col = NULL)
  
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
