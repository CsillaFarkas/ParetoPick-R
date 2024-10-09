######################## FUNCTIONS #################################
# comments: foo1 function loaded straight into global.R
# Project: Clustering Pareto Solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
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
                      pca = T,inipath="../input/config.ini") {
  
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  
  config <- read.ini(inipath)
 
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
    if ("share_con" %in% vars) {
      varmes = append(varmes, paste(mes, "share_con", sep = "_"))
    }
    if ("share_tot" %in% vars) {
      varmes = append(varmes, paste(mes, "share_tot", sep = "_"))
    }
    if ("channel_frac" %in% vars) {
      varmes = append(varmes, paste(mes, "channel_frac", sep = "_"))
    }
    
    config[[1]]$col_correlation_matrix = paste(varmes, collapse = ", ")
    
    write.ini(config, inipath)
  } 
  if (pca) {
    config[[1]]$columns = paste(pca_content, collapse = ", ")
    write.ini(config, inipath)
    
  }
  
}
## check if some variables have been removed by convert_optain
check_align = function(inipath="../input/config.ini",var_path="../input/var_corr_par.csv"){
  
  if (!file.exists(inipath) | !file.exists(var_path)) {
    return(NULL)  
  } 
  
  config <- read.ini(inipath)
  
  written <- config[[1]]$col_correlation_matrix
  written <- strsplit(written, ", ")[[1]]
  
  var_corr = read.csv(var_path)
  var_corr = colnames(var_corr)[5:ncol(var_corr)]
  
  che = setdiff(written,var_corr)
  if(length(che)==0){return(NULL)}else{
    written = setdiff(written, che)
    config[[1]]$col_correlation_matrix = paste(written, collapse = ", ")
    write.ini(config, inipath)
  }
}

  
  
  
 

##
write_pca_ini <- function(var1 = "", var2 = "", var3 = "", var4 = "",
                          var1_lab= "", var2_lab = "", var3_lab = "", var4_lab = "",inipath="../input/config.ini") {
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  config <- read.ini(inipath)
  
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
  write.ini(config, inipath) 
  
}

## write only units
write_uns <- function(var1_lab= "", var2_lab = "", var3_lab = "", var4_lab = "",inipath="../input/config.ini") {
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
  config <- read.ini(inipath)
  
  config[[5]]$var_1_label <- ifelse(var1_lab == "off", "", var1_lab)
  config[[5]]$var_2_label <- ifelse(var2_lab == "off", "", var2_lab)
  config[[5]]$var_3_label <- ifelse(var3_lab == "off", "", var3_lab)
  config[[5]]$var_4_label <- ifelse(var4_lab == "off", "", var4_lab)
  write.ini(config, inipath) 
  
}

##
write_quali_ini = function(var1 = "", var2 = "", var3 = "", var4 = "",inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)  
  } 
   config <- read.ini(inipath)
  
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
    write.ini(config, inipath)

}

##
write_cluster<- function(min_cluster=0,max_cluster=0,fixed_cluster_boolean="true",fixed_clusters=7,
                         inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)  
  }
  config <- read.ini(inipath)
  
  config[[2]]$fixed_clusters_boolean = fixed_cluster_boolean
  config[[2]]$fixed_clusters = fixed_clusters
  
  config[[2]]$min_clusters = min_cluster
  config[[2]]$max_clusters = max_cluster
  
  write.ini(config, inipath)
}

##
write_outl <- function(handle_outliers_boolean="false",deviations_min=3,deviations_max=3,
                        count_min=3,count_max=3,outlier_to_cluster_ratio=0.5,inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)  
  }
  
  config <- read.ini(inipath)
  
  config[[3]]$handle_outliers_boolean = handle_outliers_boolean
  config[[3]]$deviations_min = deviations_min
  config[[3]]$deviations_max = deviations_max
  config[[3]]$count_min = count_min
  config[[3]]$count_max = count_max
  config[[3]]$outlier_to_cluster_ratio = outlier_to_cluster_ratio
  
  write.ini(config, inipath)
  
}

## 
write_pcanum = function(pcamin,pcamax,inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)
  }
  config <- read.ini(inipath)
  
  config[[4]]$min_components = pcamin
  config[[4]]$max_components = pcamax
  write.ini(config, inipath)
  
  
}

#### Read Config Functions ####

## config for pca on startup
read_pca = function(inipath="../input/config.ini"){
  if (!file.exists(inipath)) {
    return(NULL)
  }
  config <- read.ini(inipath)
  
  if(!is.null(config[[1]]$columns)){pca_col_incl <- unlist(strsplit(config[[1]]$columns,", "))
  pca_col <- pca_col_incl[order(pca_col_incl)]}else{pca_col <- NULL}
  
  return(pca_col)
}

## 
read_config_plt = function(obj=T,axis=F,inipath="../input/config.ini"){
  
  if (!file.exists(inipath)) {
    return(NULL)
  }
  
  config <- read.ini(inipath)
  
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

## Micha's smartly rounding functions :)
round_signif <- function(x) {
   ifelse(abs(x) >= 100, round(x, 0),  signif(x, 2)) #alternatively round(x,num.decimals(x))
}


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
  stopifnot(file.exists(filepath))
  
  pf = read.table(filepath,sep=",")
  colnames(pf) = colnames
  
  range_df <- data.frame(column = character(), min = numeric(), max = numeric(), stringsAsFactors = FALSE)
  
  for (col_name in colnames) {
    min_val <- min(pf[[col_name]], na.rm = TRUE)
    max_val <- max(pf[[col_name]], na.rm = TRUE)
    range_df <- rbind(range_df, data.frame(column = col_name, min = min_val, max = max_val, stringsAsFactors = FALSE))
  }
  return(range_df)##
}

## split labels
word_splitter <- function(words, segment_length = 6) {
  sapply(words, function(word) {
    segments <- strsplit(word, "")[[1]]  # Split the word into characters
    n <- length(segments)                  # Get the number of characters
    formatted_word <- character(0)         # Initialize an empty character vector
    
    for (i in seq(1, n, by = segment_length)) {
      segment <- paste(segments[i:min(i + segment_length - 1, n)], collapse = "")
      formatted_word <- c(formatted_word, segment)
    }
    
    return(paste(formatted_word, collapse = "-\n"))
  })
}

add_perc <- function(df1, df2) {
  
  df1 = as.matrix(df1)
  df2 = as.matrix(df2)
  
  df1_new <- df1
 
  for (i in seq_len(nrow(df1))) {
    for (j in seq_len(ncol(df1))) {
     
      pd = (df1[i,j] - df2[i,j])/df2[i,j]
      
      if(round(pd * 100, 2) !=0 & !is.na(pd) & !is.nan(pd) & !is.infinite(pd) ){
        if(df2[i,j] < 0){pd = pd*(-1)}
        
        if(pd <= 0){dumb_bracket = " ("}else{dumb_bracket = " (+"}
        
        df1_new[i, j] <- paste0(df1[i, j], dumb_bracket, round(pd * 100, 2), "%)")
      }
    }
  }
  
  return(df1_new)
}


#### Python Caller ####

run_python_script <- function(path_script="",pca_status) {
  pca_status("")
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

#### Plotting the optima ####
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

## whole basin for location plot
pull_shp_pure = function(layername = "basin"){
  if(file.exists(paste0("../data/",layername,".shp"))){

    cm = read_sf(dsn = "../data/", layer = layername) #adapt path
    
    cm = st_buffer(cm, 0.0) #clean geometry
    cm = cm %>%st_transform(., 4326)
    
    return(cm)}else{return(NULL)}
  
}

## make large dataset
pull_shp = function(layername = "hru", optims, hru_in_opt_path){
  if(file.exists(paste0("../data/",layername,".shp"))){
    
    hio = readRDS(hru_in_opt_path)
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
  new_names <- paste0("Optimum_",opti_sel)
  plt_sel = shp %>% dplyr::select(id, geometry, all_of(opti_sel)) %>%  rename_with(.fn = ~ new_names, .cols = all_of(opti_sel))
  plt_sel = st_make_valid(plt_sel) # too slow annoyingly
  return(plt_sel)
}
## plot boxplots
plt_boxpl_clus = function(dat, sel, all_obs){
  clus <- dat %>%
    pivot_longer(
      cols = 2:5
    )
  
  plts=list()
  
  summ <- clus %>%
    group_by(name) %>%
    summarize(n = n())
  
  colli = c( "#FFC61E", "#009ADE","#AF58BA", "#F28522")
  labs = length((unique(clus$optimum)))
  
  for(i in 1:4){
    
    coll = colli[i]
    
    mini = mima[i,2]
    maxi = mima[i,3]
    labspo =median(clus[clus$name == all_obs[i], ]$value)
    
    p =ggplot(clus[clus$name == all_obs[i], ], aes(x = name, y = value)) +
      geom_violin(fill = coll) +
      ylim(mini, maxi)+ 
      theme_bw() + theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "none"
      )+
      annotate("text",x=1, y=labspo, label =labs )
    
    plts[[i]] = p
  }
  return(plts)
}


## plot leaflet w/ specific column
plt_lf <- function(data, mes, lo, la, buff_els, col_sel) {
  man_col = c("#66C2A5" ,"#4db818","#965c1d", "#F7A600", "#03597F" ,"#83D0F5","#FFEF2C","#a84632","#b82aa5","#246643")
  man_col = man_col[1:length(unique(mes))]
  dispal = colorFactor(palette = man_col, domain = unique(mes), na.color = "lightgrey")
  
  m <- vector("list", length = length(col_sel))
  
  for (i in seq_along(col_sel)) {
    col = col_sel[i]
    
    #buffer
    relevant_data <- data[data[[col]] %in% buff_els, ]
    buffered_data <- st_buffer(relevant_data, dist = 80)
    
    m[[i]] =  leaflet(data = data) %>%
      setView(lng = lo, lat = la, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%#poviders$Esri.NatGeoWorldMap, $Stadia.StamenToner, $OpenTopoMap
      addPolygons(
        fillColor = ~ dispal(data[[col]]),
        fillOpacity = 0.8,
        color = "lightgrey",
        weight = 1,
        popup = ~ paste0("Value: ", data[[col]]),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = ~ data[[col]]
      ) %>%
      addControl(
        html = paste(col, "</b>"),
        position = "topright",
        className = "map-title"
      ) %>%
      addPolygons(
        data = buffered_data,
        fillColor = NA,
        color = ~ dispal(relevant_data[[col]]),
        weight = 1,
        dashArray = "3",
        fillOpacity = 0.2,
        highlightOptions = highlightOptions(
          color = ~ dispal(relevant_data[[col]]),
          weight = 2,
          bringToFront = TRUE
        )
      ) %>% 
      addLegend("bottomright", pal = dispal, values = data[[col]], na.label = "no change")
  }
  return(m)
}


## cm for location
plt_cm_pure = function(data = cm,
                       la = lalo[1],
                       lo = lalo[2]) {
  p = leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% #poviders$Esri.NatGeoWorldMap, $Stadia.StamenToner, $OpenTopoMap
    setView(lng = lo, lat = la, zoom = 10) %>%
    addPolygons(fillColor = "white",color = "black",weight=0.7)
  
  return(p)
}


## legend plot
# plt_leg = function(mes){
#   dispal = colorFactor("Spectral", domain = mes, na.color = "lightgrey")
#   
#   leaflet() %>% addLegend( pal = dispal, title = "measures", values = mes, opacity = 1)
# }


## scatterplot in AHP, comparing two objectives
plt_scat2 = function(dat, x, y){
   ggplot(dat, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(color="grey50",size=1.1)+
    geom_smooth(method = "loess", se = FALSE,colour="darkblue")  +
    theme_bw() + theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "lightgray", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16))
}

#### Plotting the exploration tab

## plot (return of prep_diff_bar)
# plot_diff_bar= function(pct,obj_choices=NULL){
#   pl2 <- pct %>% rownames_to_column(var = "objective")  %>%mutate(objective=factor(objective)) %>%
#     mutate(objective=forcats::fct_relevel(objective,obj_choices))%>%
#     pivot_longer(-objective)%>%
#     
#     ggplot(aes(x = name, y = value, fill = objective)) +
#     geom_bar(position = "dodge",
#              stat = "identity",
#              alpha = 0.75) + labs(x = 'Objective', y = 'percentage change (%)') +
#     theme_bw() +
#     theme_minimal() +
#     scale_fill_manual(values = c( "#FFC61E", "#009ADE","#AF58BA", "#F28522", "#FF1F5B")) +
#     geom_text(aes(label = str_wrap(objective,width=8)),size=4,
#               colour = "black",
#               position = position_dodge(width = 1), vjust = -0.5) +
#     theme(
#       plot.title =  element_blank(),
#       axis.text.y = element_text(size = 15),
#       axis.text.x = element_text(size = 15),
#       axis.title.y = element_text(size =18),
#       axis.title.x = element_blank(),
#       legend.position = "none"
#     ) +
#     theme(plot.background = element_rect(fill = NA, color = NA))+scale_y_continuous(limits=c(-10,10))
#   
#   return(pl2)
# }


## parallel axis plot
plot_parline = function(datt,sizz=rep(.5, length(unique(datt$id))),colols=rep("grey50", length(unique(datt$id))),sq=NULL){
  if(!is.null(sq)){
  sq$id <- as.factor(rep("10000",nrow(sq)))
    
  datt = rbind(datt, sq)
  
  colols <- c(colols, "cyan")
  sizz <- c(sizz,  .5)
 
  }
   
  pl1 <- ggplot(datt, aes(x = name, y = value,group=id,size=id, color=id)) +   # group = id is important!
    
      annotate("rect", xmin=1, xmax=4, ymin=0,    ymax=0.3333333, alpha=0.1, fill="#dc3545") +
      annotate("rect", xmin=1, xmax=4, ymin=0.3333333, ymax=0.6666667,  alpha=0.15, fill="#fd7e14") +
      annotate("rect", xmin=1, xmax=4, ymin=0.6666667, ymax=1,    alpha=0.2, fill="#28a745") +
    
    annotate("text", x = 4.04, y = 0.1666666, label = "worst",  angle=90, size = 6) + # Adjust hjust for alignment
    annotate("text", x = 4.04, y = 0.5, label = "medium",  angle=90, size = 6) +
    annotate("text", x = 4.04, y = 0.8333333, label = "best",  angle=90, size = 6) +
    
    geom_line(
      aes(group = id),
      alpha = 0.5,
      lineend = 'round',
      linejoin = 'round'
    ) + theme_bw()+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_blank(),
          axis.text.y = element_text(size = 15L),
          axis.text.x = element_text(size = 14L),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_blank()
    )+
    scale_y_continuous(limits = c(0,1)) +
    scale_x_discrete(expand = c(0.02, 0.02),labels = function(x) str_wrap(x, width = 10)) + 
    labs(x = "Factors", y = "Scaled Values") +
    scale_size_manual(values = sizz) +
    scale_color_manual(values = colols)+
    coord_cartesian(clip = "off") #prevent labels to be cut off
  
  if("#FF5666" %in% colols){ #double the trouble, triple the fun
   ids= which(colols != "grey50") 
  pl1 = pl1 + geom_line(data=datt[which(datt$id %in% ids),],aes(x = name, y = value),color = "#FF5666", size=1)
    }
  return(pl1)
  
}

## scatter plot in play around
plt_sc = function(dat, ranges,col=rep("grey",nrow(dat)),size=rep(1.8, nrow(dat))){
  plots <- list()
  vars <- colnames(dat)
  num_vars <- length(vars)

  plot_index <- 1
  for (i in 1:(num_vars - 1)) {
    for (j in (i + 1):num_vars) {
      
      xcol = vars[i]
      ycol = vars[j]
      
      
      x_min = ranges[xcol,2]
      x_max = ranges[xcol,3]
        y_min = ranges[ycol,2]
        y_max = ranges[ycol,3]
     
       
      # p <- ggplot(dat, aes_string(x = vars[i], y = vars[j])) +
     p = ggplot(dat, aes(x = .data[[xcol]], y = .data[[ycol]]))+
       geom_point(size=size,color= col)+ 
        theme_bw() + theme(
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 16)
        ) +  coord_cartesian(clip = "off") #prevent labels to be cut off
 
     
      
      
      #correct for negative scale aesthetics
      if(x_min < 0 && y_min <0){
        # both negative
        p <- p + scale_x_continuous(labels = function(x) {rem_min(x)},limits = c(x_min, x_max)) + 
          scale_y_continuous(labels = function(y) {rem_min(y)},limits = c(y_min, y_max))
        xma = " (negative)"
        yma = " (negative)"
        
      } else if (x_min < 0) {
        p <- p + scale_x_continuous(labels = function(x) {rem_min(x)},limits = c(x_min, x_max))
        xma = " (negative)"
      } else if (y_min < 0) {
        p <- p + scale_y_continuous(labels = function(y) {rem_min(y)},limits = c(y_min, y_max))
        yma = " (negative)"
        
      }else{
        #no neg values
        p = p +
          scale_x_continuous(limits = c(x_min, x_max)) +
          scale_y_continuous(limits = c(y_min, y_max))
      }
      
     plots[[plot_index]] <- p
     plot_index <- plot_index + 1
    }
  }

return(plots)
}

## scatter plot in analysis tab and AHP tab

plt_sc_optima <- function(dat, x_var, y_var, col_var, size_var, high_point = NULL, pareto_path = "../data/pareto_fitness.txt",sq_path ="../data/sq_fitness.txt",
                          extra_dat = NULL, #highlight optima in AHP tab
                          an_tab = F,
                          plt_extra=F, #potentially redundant tbf
                          sel_tab = NULL, #highlight table selection Analysis tab
                          add_whole = F, #add the whole pareto front Analysis tab
                          status_q = FALSE) {
  
  if(!file.exists(pareto_path)){return(NULL)}
  
  xma = yma = NULL

  
  #plot with main data
  p = ggplot(dat, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[col_var]], size = .data[[size_var]])) +
    geom_point(shape = 21, stroke = 0.5 ) +
    viridis::scale_fill_viridis(alpha = 0.8, name = col_var) +  
    scale_size(range = c(1, 10), name = size_var) +     
    theme_bw() + 
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "lightgray", size = 0.3),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.position = "right", 
          legend.text = element_text(size=13.5),
          legend.title = element_text(size=15))
  
  all_extra_data = NULL
  
 
  if (add_whole) {
    whole <- read.table(pareto_path, header = FALSE, stringsAsFactors = FALSE, sep = ',')
    colnames(whole) <- colnames(dat)
    whole$set <- "Whole front" #pulled above, only applied in analysis tab
    all_extra_data <- rbind(all_extra_data,whole)
    
  }
  
  
  if (!is.null(extra_dat) && plt_extra) {
    extra_dat$set <- "cluster solutions"
    all_extra_data <- rbind(all_extra_data,extra_dat)
    
  }
  
  if (!is.null(high_point)) {
    high_point$set <- "AHP - best option"
    all_extra_data <- rbind(all_extra_data,high_point)
    
  }

  if (!is.null(sel_tab)) {
    sel_tab$set <- "Selection"
   
    if(!is.null(all_extra_data)){all_extra_data <- rbind(all_extra_data,sel_tab)}else{all_extra_data = sel_tab}
    
  }
  
  if (status_q) {
    st_q <- read.table(sq_path, header = FALSE, stringsAsFactors = FALSE, sep = ' ')
    names(st_q) <- names(dat)
    st_q$set <- "Status Quo"
    all_extra_data <- rbind(all_extra_data,st_q)
  }

  if (!is.null(all_extra_data)) {
    
    p <- p +
      geom_point(data = all_extra_data, aes(x = .data[[x_var]], y = .data[[y_var]], shape = set, color = set, size = .data[[size_var]]), 
                  stroke = 1.8, show.legend = TRUE, alpha=0.7) +
      scale_shape_manual(values = c("Whole front" = 21,"cluster solutions" = 21, "AHP - best option" = 22, "Selection" =21, "Status Quo" = 17),name="") + 
      scale_color_manual(values = c( "Whole front" = "lightgrey","cluster solutions" = "cyan", "AHP - best option" = "#FF4D4D", "Selection" = "black", "Status Quo" = "#FF00FF"),name="") + 
      guides(color = guide_legend(override.aes = list(size = 5))
             ,shape = guide_legend(override.aes = list(size = 5))
              )
  }
 
  #correct for negative scale aesthetics
  if (any(dat[[x_var]] < 0) && !(an_tab)) {
    p <- p + scale_x_continuous(labels = function(x) {
      rem_min(x)
    })
    xma = " (inverted scale!)"
  }
  
  if (any(dat[[y_var]] < 0)&& !(an_tab)) {
    p <- p + scale_y_continuous(labels = function(y) {
      rem_min(y)
    })
    yma = " (inverted scale!)"
    
  }
  
  #Analysis tab requires control of limits to show selection as part of whole front
  if(an_tab){
    
    #pull fit(), establish limits
    whole <- read.table(pareto_path, header = FALSE, stringsAsFactors = FALSE, sep = ',')
    colnames(whole) <- colnames(dat)
    
    #control for negative data
    #both negative
    if(any(dat[[x_var]] < 0) && any(dat[[y_var]] < 0)){
      
      p <- p + scale_x_continuous(labels = function(x) {rem_min(x)},
                                  limits= c(range(whole[[x_var]])[1],range(whole[[x_var]])[2]))+
               scale_y_continuous(labels = function(y) {rem_min(y)},
                                  limits= c(range(whole[[y_var]])[1],range(whole[[y_var]])[2]))
      
      yma = " (inverted scale!)"
      xma = " (inverted scale!)"
      
      #only y_scale
     }else if(any(dat[[y_var]] < 0)){
       
       p <- p + scale_y_continuous(labels = function(y) {rem_min(y)},
                                   limits= c(range(whole[[y_var]])[1],range(whole[[y_var]])[2]))
       
       yma = " (inverted scale!)"
       
      #only x_scale
     }else if(any(dat[[x_var]] < 0)){ 
       p <- p + scale_x_continuous(labels = function(x) {rem_min(x)},
                                   limits= c(range(whole[[x_var]])[1],range(whole[[x_var]])[2]))
       xma = " (inverted scale!)"
       
       #neither
     }else{p <- p + scale_x_continuous(limits= c(range(whole[[x_var]])[1],range(whole[[x_var]])[2]))+
      scale_y_continuous(limits= c(range(whole[[y_var]])[1],range(whole[[y_var]])[2])) }
  }
  
  p = p +labs(x=paste0(x_var,xma), y = paste0(y_var,yma))
  
  
  return(p)
}



#### AHP Functions ####

## consistency index for AHP
consistency_index <- function(m) {
  eig <- eigen(m)$values
  lambda_max <- Re(eig[which.max(Re(eig))])
  n <- nrow(m)
  return((lambda_max - n) / (n - 1))
}

## find main inconsistencies
check_inconsistencies <- function(comparison_matrix, weights) {
  n <- nrow(comparison_matrix)
  inconsistencies <- data.frame(
    # LastChangedElement = character(),
    Level = numeric(),
    weight = numeric(),
    stringsAsFactors = FALSE
  )
  
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        if (i != j && j != k && i != k) {
          if (comparison_matrix[i, j] > 1 && comparison_matrix[j, k] > 1 && comparison_matrix[i, k] <= 1) {
            inconsistencies <- rbind(inconsistencies, data.frame(
              # LastChangedElement = paste0("(", i, ", ", j, ") -> (", j, ", ", k, ")"),
              Level = comparison_matrix[i, j] * comparison_matrix[j, k],  
              weight = weights[i],
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  ordered_i <- inconsistencies[order(-inconsistencies$Level), ]
  ordered_i$Level = NULL
  return(ordered_i)
}

#### Rescaling and matching Functions ####
## return the original value and the position of scaled value in the original dataset
scaled_abs_match = function(minval_s=c(0,0,0,0),
                            maxval_s=c(1,1,1,1),
                            scal_tab=NULL,abs_tab=NULL,
                            allobs=NULL,smll = TRUE,at=F){ 
  
 
  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  
  # locate values in scaled dataframe 
  for(i in seq_along(allobs)){ #this does not have to run for those where we only want abs_tab/output(ch)
    
    sca_max <- scal_tab[which.min(abs(scal_tab[[allobs[i]]]-maxval_s[i])),]
    df["max",allobs[i]] = abs_tab[rownames(sca_max),allobs[i]]  
    
    sca_min <- scal_tab[which.min(abs(minval_s[i]-scal_tab[[allobs[i]]])),]
    df["min",allobs[i]] = abs_tab[rownames(sca_min),allobs[i]]
  }
  
  # consider interactions between objectives (some are not attainable anymore)
  # reduced dataframe of absolute values within all objective ranges
  ch = abs_tab
  
  for(k in seq_along(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<=valma & .data[[allobs[k]]]>=valmi)

  }

  
  # retain only min and max
  cw = as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(cw) = allobs
  
  
  for (l in seq_along(allobs)) {
    if(length(ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1 ||
       length(ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)) > 1) {
      cw["max", allobs[l]] = NA
      cw["min", allobs[l]] = NA
    }else{
      
      cw["max", allobs[l]] = ch %>% slice_max(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
      cw["min", allobs[l]] = ch %>% slice_min(.data[[allobs[l]]]) %>% select(allobs[[l]]) %>% slice(1)
    }
    
    
  }
  
  if(at){rownames(cw) = c("best","worst")}
  
  #when smll is set to false the table with all absolute values is returned
  if(smll){return(cw)}else{return(ch)} 
  
}


## similar to ch in scaled_abs_match, matching input scaled data with a scaled dataframe
match_scaled = function(minval_s=c(0,0,0,0),
                        maxval_s=c(1,1,1,1),
                        scal_tab = NULL,
                        allobs){
  
  df <- as.data.frame(array(NA,dim=c(2,length(allobs))),row.names = c("max","min"))
  colnames(df) = allobs
  
  # locate values in scaled data frame 
  for(i in seq_along(allobs)){
    
    sca_max <- scal_tab[which.min(abs(scal_tab[[allobs[i]]]-maxval_s[i])),]
    df["max",allobs[i]] = scal_tab[rownames(sca_max),allobs[i]]  
    
    sca_min <- scal_tab[which.min(abs(minval_s[i]-scal_tab[[allobs[i]]])),]
    df["min",allobs[i]] = scal_tab[rownames(sca_min),allobs[i]]
  }
  
  # surely this should be easier 
  ch = scal_tab
  
  for(k in seq_along(allobs)){
    valma = df["max",k]
    valmi = df["min",k]
    ch =  ch %>% filter(.data[[allobs[k]]]<=valma & .data[[allobs[k]]]>=valmi)
  }

  if(dim(ch)[1]==0){stop("no optima fulfill these conditions")}else{return(ch)}
  
}

## subset dataframe based on (slider) selection
match_abs <- function(minval, maxval, abs_tab, ranger = NULL) {
  n_cols <- ncol(abs_tab)
  
  if(!is.null(ranger)){#undo the scaling which was done for the slider visibility
    
    indices <- which(names(abs_tab) %in% ranger)

    maxval[indices] = maxval[indices] / 1000
    minval[indices] = minval[indices] / 1000
  }
  
  filter_conditions <- lapply(seq_len(n_cols), function(i) {
    abs_tab[[i]] >= minval[i] & abs_tab[[i]] <= maxval[i]
  })
  
  combined_filter <- Reduce(`&`, filter_conditions)
  
  abs_filter <- abs_tab %>% filter(combined_filter)
  
  return(abs_filter)
}

## rescale
rescale_column <- function(column, min_val, max_val) {
  if (min_val == max_val) {
    return(rep(NA, length(column)))  # or some other appropriate value/handling
  }
  rescale(column, to = c(0, 1), from = c(min_val, max_val))
}

#### Other Functions ####

get_mima = function(df){
  min_values <- sapply(df, min, na.rm = TRUE)
  max_values <- sapply(df, max, na.rm = TRUE)
  
  min_max_df <- data.frame(
    Variable = names(min_values),
    worst = min_values,
    best = max_values,
    stringsAsFactors = FALSE
  )
  
  return(min_max_df)
}

## check sliders and adapt var_corr_par accordingly
check_sliders <- function(input_vals, default_vals, ranger = NULL) {  #input_vals as list made from input$ranx
  touched <- sapply(1:4, function(s) {
    !all(input_vals[[s]] == default_vals[[s]])
  })
  
  
  if (any(touched)) {
    #check which var_corr_par are available, if previously touched take fresh one, store it, change it and safe it under new name
    if (file.exists("../input/var_corr_par_bu.csv")) {
      whole = read.csv("../input/var_corr_par_bu.csv", check.names = F)
    } else{#if never been reduced we read in original and create back up
      whole = read.csv("../input/var_corr_par.csv", check.names = F)
      write.csv(whole, file = "../input/var_corr_par_bu.csv", row.names = F) #now its changed a backup is needed
    }
    
    trs = whole
    
    if (!is.null(ranger)) {#basically what match_abs is doing too plus more columns
      indics <- which(names(trs) %in% ranger)
    }else{indics = NULL}
    
    for (k in which(touched)) {
      valma = input_vals[[k]][2]
      valmi = input_vals[[k]][1]
      
      if (k %in% indics) {
       valma = valma / 1000
       valmi = valmi / 1000
      }
      trs =  trs %>% filter(trs[[k]] <= valma & trs[[k]] >= valmi)
    }
    
   
    
    write.csv(trs,file="../input/var_corr_par.csv",row.names = F)
    
  } else{return(NULL)}
  
}

## remove minus (required for nicer plotting)
rem_min <- function(x) {
  gsub("-", "", scales::number(x))
}

## used with rem_min in plots
all_neg <- function(column) {
  all(column < 0)
}

## default max number of pc
get_num_pca <- function(pc_path = "../input/pca_content.RDS") {
  if(!file.exists(pc_path)){return(NULL)}
  pcc <- readRDS(pc_path)
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
      "<li>","Outliers are tested.","</li>",
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
      "<li>","Outliers are tested.","</li>",
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