csv <- impose_list_y[[1]][["sizes"]]
for (i in 1:length(impose_list_y)){
csv <- dplyr::bind_cols(csv, impose_list_y[[i+1]][["sizes"]])}
write.csv2(csv, file = "new_yellow_faces.csv")
