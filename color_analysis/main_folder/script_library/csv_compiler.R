csv_list <- c(choose.files())
template <- read.csv(csv_list[[1]], header=F)
container <- dplyr::select(template, V1) 
for (i in 1:length(csv_list)){
  csv <- read.csv(csv_list[[i]], header = F)
  container <- dplyr::full_join(container, csv, by="V1") 
  
}

####Broken######
#%>%
  #dplyr::mutate_all(~replace(container, is.na(.), 0))
  
write.csv2(container, file = "beh_prob.csv")
  

