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

## config for correlation
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
    configs[[1]]$colums = paste(pca_content, collapse = ", ")
    write.config(configs, file.path = "../input/config.ini", write.type = "ini")
    
  }
  
}

