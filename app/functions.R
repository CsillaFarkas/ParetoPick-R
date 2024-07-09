################### FUNCTIONS #####################

## Correlation Analysis - Plot
# RColorBrewer::brewer.pal(4, "RdYlBu")
plt_corr = function(corr,labelcol = "black",labelorder='alphabet',meth = 'square', tpe = 'lower'){
corma = as.matrix(corr)
plt = corrplot(corma, method = meth, order =labelorder,tl.col = labelcol, type=tpe,diag=FALSE)
return(plt)
}

## Correlation Analysis - Table
find_high_corr <- function(cor_matrix, threshold = 0.75, tab = T) {
  # Get the row and column names of the correlation matrix
  var_names <- colnames(cor_matrix)
  
  # Initialize an empty data frame to store the pairs
  high_cor_pairs <- data.frame(
    variable1 = character(),
    variable2 = character(),
    Correlation = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through the upper triangle of the correlation matrix
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
    arrange(desc(abs(Correlation)))
  
  yolo = unique(c(higg$variable1,higg$variable2)) #used to fill drop down menu
  
  if(tab==T){return(higg)}else(return(yolo))
}

## Update Config for PCA and Correlation
write_corr = function(vars,
                      measures = mes,
                      configs = config,
                      cor_analysis = F,
                      pca_content = all_var,
                      pca = T) {
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
    
    configs[[1]]$col_correlation_matrix = paste(varmes, collapse = ", ")
    
    write.config(configs, file.path = "../input/config.ini", write.type = "ini")
  } 
  if (pca) {
    configs[[1]]$columns = paste(pca_content, collapse = ", ")
    write.config(configs, file.path = "../input/config.ini", write.type = "ini")
    
  }
  
}

# Write empty var for those where "off"
write_pca_ini <- function(configs = config, var1 = "", var2 = "", var3 = "", var4 = "") {
  configs[[5]]$var_1 <- ifelse(var1 == "off", "", var1)
  configs[[5]]$var_2 <- ifelse(var2 == "off", "", var2)
  configs[[5]]$var_3 <- ifelse(var3 == "off", "", var3)
  configs[[5]]$var_4 <- ifelse(var4 == "off", "", var4)
  
  off_count <- sum(c(var1, var2, var3, var4) == "off")
  configs[[5]]$num_variables_to_plot <- 4 - off_count
  
  configs[[6]]$qualitative_clustering_columns = c(var1, var2, var3, var4)
  
  
  write.config(configs, file.path = "../input/config.ini", write.type = "ini")
  
}

## Read Config for PCA Table on startup 
read_pca = function(con = config){
  if(!is.null(config[[1]]$columns)){pca_col_incl = unlist(strsplit(config[[1]]$columns,", "))
  pca_col = pca_col_incl[order(pca_col_incl)]}else(pca_col = NULL)
  
  return(pca_col)
}

read_config_plt = function(con = config){
  var1 = config[[5]]$var_1
  var2 = config[[5]]$var_2
  var3 = config[[5]]$var_3
  var4 = config[[5]]$var_4
  
  return(c(var1,var2,var3,var4))
  
}

## Count number of decimals
num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}

## Extract the Objective ranges
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
  
  
  return(range_df)
}
