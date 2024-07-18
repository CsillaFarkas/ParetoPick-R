####################  Convert OPTAIN ###############################################
# MISSING: potentially some dynamism to make this run for other catchments
# creates a .csv to be used in the Correlation and PCA
# each row one Pareto-optimal solution
# 1.- 4. = objectives to be maximised
# 5 - end = variables to be considered in the clustering (=all_var provided separately)
# used files: pareto_genomes.txt, hru.con, measure_location.csv
# Project: Clustering of pareto front to reduce objective space
####################################################################################

# rm(list=ls())
print(paste0("loading required packages..."),quote=F)
suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(shinythemes)
  library(shinydashboard)
  library(tidyverse)
  library(readr)
  library(ggtext)
  library(viridis)
  library(patchwork)
  library(here)
  library(purrr)
  library(rnaturalearthdata)
  library(mapview)
  library(leafsync)
  library(leaflet)
  library(tmap)
  library(sf)
  library(ggplot2)
  library(plotly)
  library(sp)
  library(spdep)
  library(geosphere)
  library(geohashTools)
})


## Genomes
  gen = read.table("../data/pareto_genomes.txt", header=F,stringsAsFactors=FALSE,sep = ',')
  gen=as.data.frame((t(gen))) #now the rownumber is the measures/AEP and the columns are the points on optima
  
  print("check: read pareto_genomes.txt...",quote=F)

  #get number of optima
  nopt = length(gen)
  gen <- gen %>%
    mutate(id = c(1:nrow(gen)), .before = V1) #id is number of AEP

# genome_hru matches AEP with hrus, several hrus for each AEP 
  genome_hru <- read.csv('../data/measure_location.csv')

  print("check: read measure_location.csv...",quote=F)
#Separate values in obj_id/every hru its own column
  genome_hru_separate <- genome_hru %>%
     separate(obj_id, paste0("hru_sep_", 1:35), sep = ',', remove = FALSE)#hru = obj_id in separate columns

  gen_act_hru <- genome_hru_separate %>%
     left_join(gen, by = "id") #id = individual AEPs

# Pivot 
  gen_act <- gen_act_hru %>%
   pivot_longer(cols = paste0("hru_sep_", 1:35), names_to = "code", values_to = "name_new") %>% #name_new = hru separated
   relocate(name_new, .after = obj_id)%>%drop_na(name_new)

# Eliminate space before some "name_new"
  gen_act$name_new <- str_remove(gen_act$name_new, " ") 
  prios <- read.csv("../input/nswrm_priorities.csv") ## this comes with the package

  gen_act_prio= gen_act %>%
    left_join(prios, by = c("nswrm" = "nswrm")) %>%
    relocate(priority, .after = nswrm)%>%arrange(priority)
  
  print("check: assigned priorities...",quote=F)


#### Land use Cover of HRUs ####
 ## 1. create a dataframe defining all land uses of hru across pareto optima
 # empty hru dataframe
  hru = data.frame(id=unique(gen_act_prio$name_new)) 
  hru[paste0("V",1:nopt)]=NA
  
  # reduce the df size for efficiency
  gen_check = gen_act_prio %>% select(-c(id,name,obj_id))

  print("calculating: land use allocation in optima under priorities...",quote=F)
  
# loop through optima
for(op in paste0("V", 1:nopt)){ #instable looping, Cordi...
  
  # only consider activated hrus
  all_act = gen_check %>%select(c(all_of(op),name_new, nswrm,priority))%>%filter(.data[[op]]==2)%>%group_by(name_new)
  
  # without conflicting use 
  no_confl = all_act%>%filter(n()==1)%>%ungroup()
  
  if(nrow(no_confl) > 0){
    
    hru <- hru %>%
      left_join(no_confl %>% select(name_new, nswrm), by = c("id" = "name_new")) %>%
      mutate(!!op := ifelse(!is.na(nswrm), nswrm, !!sym(op))) %>% 
      select(-nswrm)
  }
  
  
  # With conflicting use (no ungroup and regroup needed)
  confl_use <- all_act %>% filter(n() > 1) %>% 
    filter(priority == min(priority)) %>% 
    distinct(name_new, nswrm)
  
  
  if(nrow(confl_use) > 0) {
    hru <- hru %>%
      left_join(confl_use%>% select(name_new, nswrm), by = c("id" = "name_new")) %>%
      mutate(!!op := ifelse(!is.na(nswrm), nswrm, !!sym(op))) %>%
      select(-nswrm)
  }
  print(paste0("check: calculated land use allocation in optima ",op,"..."),quote=F)
  
}
  ## hru represents the connection between the optima and plot
  hru <- hru%>%mutate(id = as.integer(id))
  saveRDS(hru,file= "../input/hru_in_optima.RDS")
  print(paste0("check: made hru land use available for future plotting..."),quote=F)
  
  
  ## Moran's, share in total and activated area and linE
  con = read.table("../data/hru.con",header=T,skip=1)
  
  # could also use id as is the same as obj_id in con
  hru_donde <- con %>% select(obj_id,area,lat,lon)%>% inner_join(hru, by = c("obj_id"="id")) # Pareto front in columns
   
  # empty measures dataframe
  meas = unique(gen_act_prio$nswrm)
  
  ## Local Moran's i
    mit_i = hru_donde %>% select(obj_id, lat, lon)
  # Calculate pairwise distances using the Haversine formula
   dist_matrix <- distm(mit_i[, c('lon', 'lat')], fun = distHaversine)
  
  # Create spatial weights matrix using inverse distances
    inv_dist_matrix <- 1 / dist_matrix
    diag(inv_dist_matrix) <- 0  # Set the diagonal to zero to avoid infinity
  
  # Convert to a listw object for spatial analysis
   weights_listw <- mat2listw(inv_dist_matrix, style = "B")
   print("check: produced spatial weights object...",quote=F)
  # this weight object is used to calculate spatial autocorrelation across different measures, using the area they cover as input value

  # empty dataframe
    mesur = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
    colnames(mesur) = meas  #replace with <meas>_moran below
    rownames(mesur) = paste0("V", 1:nopt)
  
    #also needed for calculation of area share
    hru_copy = hru_donde %>% select(paste0("V", 1:nopt))
    print("calculating: Moran's I...",quote=F)
    
  # Moran's per measure/land use (setting all others to 0 and taking the area)
    for (op in paste0("V", 1:nopt)) {
      #   #how much area was covered by individual measures (hedge and linear stuff of course very little)
      opti = hru_donde %>% select(c(all_of(op), area))
      
      for (m in meas) {
        if (m %in% opti[[op]]) {
          #check if land use is part of optimum (pond sometimes is not in Schwarzer Schoeps)
          
          moran_area =  opti %>% mutate(mor = ifelse(.data[[op]] == m, area, 0)) %>%
            replace(is.na(.), 0)
          
          mesur[op, m] = median(localmoran(moran_area$mor, weights_listw)[, "Ii"])#median or mean?
          
        }else{mesur[op, m] = 0}
      }
      print(paste0("check: calculated Moran's I for Optimum ", op,"..."),quote=F)
    }
  # change col names
  colnames(mesur) = paste(colnames(mesur),"moran",sep="_")
  mesur = mesur %>%mutate(id = row_number())

  ## Moran's across all land uses/measures
  # for(op in paste0("V", 1:nopt)){
  #   mesur[op,"moran"]=mean(localmoran(hru_copy[[op]],weights_listw)[,"Ii"])# mean is debatable
  # }
  
  # replace empty/no measure with 0
  # hru_copy[is.na(hru_copy)] <- 0
  # hru_copy = as.data.frame(sapply(hru_copy, as.numeric)) #
  # 
  # for (op in paste0("V", 1:nopt)) {
  #   mesur[op, "moran"] = mean(localmoran(hru_copy[[op]], weights_listw)[, "Ii"])# mean is debatable
  # }
  
  
  ## linE - number of management versus number of structural measures in each optimum
  mngmt_obj= prios%>%filter(mngmt ==1)%>%select(nswrm)%>%pull() #management measures
  strct_obj = prios%>%filter(mngmt ==0)%>%select(nswrm)%>%pull()#structural measures
  
  lin = as.data.frame(array(NA, dim = c(nopt,1)))
  names(lin) = "linE"
  rownames(lin) = paste0("V", 1:nopt)
  
  for(op in paste0("V", 1:nopt)){
    opti = hru_donde %>% select(all_of(op))%>%group_by(.data[[op]])%>%mutate(count=n())
    
    mngmt = opti%>%filter(.data[[op]] %in% mngmt_obj)%>%ungroup()%>%select(count)%>%distinct()%>%pull()
    
    strc = opti %>% filter(.data[[op]] %in% strct_obj)%>%distinct()%>%ungroup()%>%select(count)%>%sum()

    lin[op,]= (strc/mngmt)*100 #just a nicer value
    print(paste0("caculated linE for Optimum ",op,"..."),quote=F)
    }
  
  lin = lin %>%mutate(id = row_number())
  
  
  ## Area per measure, required for calculating share below
  # empty dataframe
   arre = as.data.frame(array(NA, dim =c(nopt,length(meas)))) # Pareto front in rows
   colnames(arre) =meas  
   rownames(arre) = paste0("V", 1:nopt)
  
  for (op in paste0("V", 1:nopt)) {
    #how much area was covered by individual measures (hedge and linear stuff of course very little)
    opti = hru_donde %>% select(c(all_of(op), area))
    
    for (m in meas) {
      if (m %in% opti[[op]]) {
        #check if land use is part of optimum (pond sometimes is not in Schwarzer Schoeps)
        
        arre[op, m] = opti %>% filter(.data[[op]] == m) %>%
          mutate(tot = sum(area)) %>% distinct(tot) %>% pull()
        
      }
      else{
        arre[op, m] = 0
      }
    }
    print(paste0("caculated area share of measures across Optimum ",op,"..."),quote=F)
    
  }

  
  ## share in total catchment area
  totar = sum(con$area)
  sit = arre %>% mutate(across(meas,~ (.x/totar)*100))%>%
    rename_at(vars(meas),~paste0(., "_sit"))%>%mutate(id = row_number())
 
  ## share in implemented catchment area
  siim = arre %>% mutate(allarea = rowSums(across(everything()), na.rm =T))%>%
    mutate(across(.cols = 1:length(meas),~ (.x/allarea)*100)) %>%select(-allarea)%>%
    rename_at(vars(meas),~paste0(., "_siim"))%>%mutate(id = row_number())

  
  ## merge with pareto fitness, # I assume the first row is the first pareto V1??
  fit = read.table("../data/pareto_fitness.txt", header=F,stringsAsFactors=FALSE,sep = ',')
  yolo = readRDS("../input/object_names.RDS")
  names(fit) = yolo# c('HC', 'HQ', 'P', 'AP')
  fit$id = 1:nrow(fit)
  print("check: read pareto_fitness.txt, assigned names...",quote=F)
  
  test_clu = fit %>%left_join(lin,by="id")%>%left_join(siim, by = "id") %>%left_join(sit, by ="id") %>% left_join(mesur, by="id")%>% select(-id)%>%replace(is.na(.), 0)
   
  write.csv(test_clu, "../input/var_corr_par.csv",  row.names = FALSE, fileEncoding = "UTF8")  
  print("check: printed output ---> /input/var_corr_par...",quote=F)

  all_var = colnames(test_clu)[5:ncol(test_clu)]  ##assuming four variables here
  saveRDS(all_var,file = "../input/all_var.RDS") #required for PCA
  print("check: provided variable names ---> /input/all_var...",quote=F)
  
  