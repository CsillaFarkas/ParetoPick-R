################### FUNCTIONS #####################

## Correlation Analysis - Plot
plt_corr = function(corr,labelcol = "black",labelorder='alphabet'){
corma = as.matrix(corr)
plt = corrplot(corma, method = 'color', order =labelorder,tl.col = labelcol)
return(plt)
}

## Correlation Analysis - Table
find_high_corr <- function(cor_matrix, threshold = 0.75) {
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
      if (cor_matrix[i, j] > threshold) {
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
  
  return(high_cor_pairs)
}
