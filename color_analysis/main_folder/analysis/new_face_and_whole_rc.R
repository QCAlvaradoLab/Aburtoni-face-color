# Loading the required packages ####
library(recolorize)
library(patternize)
library(raster)
setwd("C:/Users/mhack/Dropbox (Medgar Evers College)/Dyad/Matt Peroš/Dyad Color Analysis/images")
b_face <- readRDS(file.choose())
y_face <- readRDS(file.choose())

#######Redo for whole body alignment###############
{list_T <- tools::file_path_sans_ext(dir("Cropped/T Blue/whole", ".jpg"))
imageList_T <- makeList(list_T, type = "image",
                        prepath = "Cropped/T Blue/whole/",
                        extension = ".jpg")
list_NT <- tools::file_path_sans_ext(dir("Cropped/NT Blue/whole", ".jpg"))
imageList_NT <- makeList(list_NT, type = "image",
                         prepath = "Cropped/NT Blue/whole/",
                         extension = ".jpg")
imageList_B <- c(imageList_T, imageList_NT)

list_T <- tools::file_path_sans_ext(dir("Cropped/T Yellow/whole", ".jpg"))
imageList_T <- makeList(list_T, type = "image",
                        prepath = "Cropped/T Yellow/whole/",
                        extension = ".jpg")
list_NT <- tools::file_path_sans_ext(dir("Cropped/NT Yellow/whole", ".jpg"))
imageList_NT <- makeList(list_NT, type = "image",
                         prepath = "Cropped/NT Yellow/whole/",
                         extension = ".jpg")
imageList_Y <- c(imageList_T, imageList_NT)
}

{aligned_images <- alignReg(imageList_B, resampleFactor = NULL, target = imageList_B[[2]],
                           ####maskOutline = outline, 
                           plotTransformed = TRUE)
saveRDS(aligned_images, file = 'rds/aligned_blue_whole.rds')
b_whole <- readRDS('rds/aligned_blue_whole.rds')
b_whole <- aligned_images
}

{aligned_images <- alignReg(imageList_Y, resampleFactor = NULL, target = imageList_Y[[2]],
                           ####maskOutline = outline, 
                           plotTransformed = TRUE)
saveRDS(aligned_images, file = 'rds/aligned_yellow_whole.rds')
y_whole <- readRDS('rds/aligned_yellow_whole.rds')
}



{{for (i in 1:length(b_face)) {
  img <- brick_to_array(b_face[[i]])
  blur <- blurImage(img, blur_function = "blur_anisotropic",
                    amplitude = 20, sharpness = 0.05, plotting = T)
  b_face [[i]] <- array_to_RasterStack(blur, type = "brick", alpha_mask = T, return_alpha = T)}
}
{for (i in 1:length(y_face)) {
  img <- brick_to_array(y_face[[i]])
  blur <- blurImage(img, blur_function = "blur_anisotropic",
                    amplitude = 20, sharpness = 0.05, plotting = T)
  y_face [[i]] <- array_to_RasterStack(blur, type = "brick", alpha_mask = T, return_alpha = T)}
}

{for (i in 1:length(b_whole)) {
  img <- brick_to_array(b_whole[[i]])
  blur <- blurImage(img, blur_function = "blur_anisotropic",
                    amplitude = 20, sharpness = 0.05, plotting = T)
  b_whole [[i]] <- array_to_RasterStack(blur, type = "brick", alpha_mask = T, return_alpha = T)}
}
{for (i in 1:length(y_whole)) {
  img <- brick_to_array(y_whole[[i]])
  blur <- blurImage(img, blur_function = "blur_anisotropic",
                    amplitude = 20, sharpness = 0.05, plotting = T)
  y_whole [[i]] <- array_to_RasterStack(blur, type = "brick", alpha_mask = T, return_alpha = T)}
}
}
################################################################################
{
{imgs <- lapply(b_whole, brick_to_array)
names(imgs) <- names(b_whole)
extent_list <- lapply(b_whole, extent)
{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = 10,
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 25,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = blue vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/b_whole_fit.rds")
  rc_list <- readRDS("rds/b_whole_fit.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors, which will be the final amount of colors used in the palette. Colors
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 30)
  
  # make an empty matrix for storing the new palette
  palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))
  
  # for every color in cluster_list...
  for (i in 1:length(cluster_list)) {
    
    # get the center indices 
    idx <- cluster_list[[i]]
    
    # get the average value for each channel, using cluster size to get a weighted average
    ctr <- apply(all_palettes, 2, 
                 function(j) weighted.mean(j[idx], 
                                           w = all_sizes[idx]))
    
    # store in the palette matrix
    palette[i, ] <- ctr
  }
  # This saves an image of the color palette into our folder
  png('30 bins/output/whole/blue/b_whole_pal.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/b_whole_pal.rds")
  #palette <- readRDS(file.choose())
  # And plot it to check it out.
  plotColorPalette(palette)
  
  {
    #palette <- readRDS(file.choose())
    #palette <- rbind(palette, c(0.75, 1, 0.25)) #p<0.05 green
    palette <- rbind(palette, c(0.25, 1, 0.25)) #p<0.01 green
    #palette <- rbind(palette, c(0.90, .1, 0.1)) #p<0.05
    palette <- rbind(palette, c(0.60, .1, 0.001)) #p<0.01
    plotColorPalette(palette)
    saveRDS(palette, "rds/fc_b_whole_pal.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS("rds/b_whole_fit.rds")
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/b_whole_fit.rds")
  impose_whole_B <- readRDS("rds/b_whole_fit.rds")
  csv <- impose_whole_B[[1]][["sizes"]]
  for (i in 1:length(impose_whole_B)){
    csv <- dplyr::bind_cols(csv, impose_whole_B[[i+1]][["sizes"]])}
  write.csv2(csv, file = "30 Bins/output/whole/blue/blue_whole_sizes.csv")
}}
{for (i in 1:length(impose_list_B)){
  png(paste0("30 Bins/output/whole/blue/clusters/", i , ".png"))
  plotColorClusters(impose_list_B[[i]]$centers, impose_list_B[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = i, color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}}

###############################################################################

{imgs <- lapply(y_whole, brick_to_array)
names(imgs) <- names(y_whole)
extent_list <- lapply(y_whole, extent)
{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = 10,
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 25,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = yellow vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/y_whole_fit.rds")
  rc_list <- readRDS("rds/y_whole_fit.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors, which will be the final amount of colors used in the palette. Colors
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 30)
  
  # make an empty matrix for storing the new palette
  palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))
  
  # for every color in cluster_list...
  for (i in 1:length(cluster_list)) {
    
    # get the center indices 
    idx <- cluster_list[[i]]
    
    # get the average value for each channel, using cluster size to get a weighted average
    ctr <- apply(all_palettes, 2, 
                 function(j) weighted.mean(j[idx], 
                                           w = all_sizes[idx]))
    
    # store in the palette matrix
    palette[i, ] <- ctr
  }
  # This saves an image of the color palette into our folder
  png('30 bins/output/whole/yellow/y_whole_pal.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/y_whole_pal.rds")
  #palette <- readRDS(file.choose())
  # And plot it to check it out.
  plotColorPalette(palette)
  
  {
    #palette <- readRDS(file.choose())
    #palette <- rbind(palette, c(0.75, 1, 0.25)) #p<0.05 green
    palette <- rbind(palette, c(0.25, 1, 0.25)) #p<0.01 green
    #palette <- rbind(palette, c(0.90, .1, 0.1)) #p<0.05
    palette <- rbind(palette, c(0.60, .1, 0.001)) #p<0.01
    plotColorPalette(palette)
    saveRDS(palette, "rds/fc_y_whole_pal.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS("rds/y_whole_fit.rds")
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/y_whole_fit.rds")
  impose_whole_Y <- readRDS("rds/y_whole_fit.rds")
  csv <- impose_whole_Y[[1]][["sizes"]]
  for (i in 1:length(impose_whole_Y)){
    csv <- dplyr::bind_cols(csv, impose_whole_Y[[i+1]][["sizes"]])}
  write.csv2(csv, file = "30 Bins/output/whole/yellow/yellow_whole_sizes.csv")
}}
{for (i in 1:length(impose_whole_Y)){
  png(paste0("30 Bins/output/whole/yellow/clusters/", i , ".png"))
  plotColorClusters(impose_whole_Y[[i]]$centers, impose_whole_Y[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = i, color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}}
}
#############################Next Up!!!!!!!!###################################################
{
{imgs <- lapply(b_face, brick_to_array)
names(imgs) <- names(b_face)
extent_list <- lapply(b_face, extent)
{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = 10,
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 25,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = blue vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/b_face_fit.rds")
  rc_list <- readRDS("rds/b_face_fit.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors, which will be the final amount of colors used in the palette. Colors
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 30)
  
  # make an empty matrix for storing the new palette
  palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))
  
  # for every color in cluster_list...
  for (i in 1:length(cluster_list)) {
    
    # get the center indices 
    idx <- cluster_list[[i]]
    
    # get the average value for each channel, using cluster size to get a weighted average
    ctr <- apply(all_palettes, 2, 
                 function(j) weighted.mean(j[idx], 
                                           w = all_sizes[idx]))
    
    # store in the palette matrix
    palette[i, ] <- ctr
  }
  # This saves an image of the color palette into our folder
  png('30 bins/output/face/blue/b_face_pal.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/b_face_pal.rds")
  #palette <- readRDS(file.choose())
  # And plot it to check it out.
  plotColorPalette(palette)
  
  {
    #palette <- readRDS(file.choose())
    #palette <- rbind(palette, c(0.75, 1, 0.25)) #p<0.05 green
    palette <- rbind(palette, c(0.25, 1, 0.25)) #p<0.01 green
    #palette <- rbind(palette, c(0.90, .1, 0.1)) #p<0.05
    palette <- rbind(palette, c(0.60, .1, 0.001)) #p<0.01
    plotColorPalette(palette)
    saveRDS(palette, "rds/fc_b_face_pal.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS("rds/b_face_fit.rds")
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/b_face_fit.rds")
  impose_face_B <- readRDS("rds/b_face_fit.rds")
  csv <- impose_face_B[[1]][["sizes"]]
  for (i in 1:length(impose_face_B)){
    csv <- dplyr::bind_cols(csv, impose_face_B[[i+1]][["sizes"]])}
}}
   write.csv2(csv, file = "30 Bins/output/face/blue/blue_face_sizes.csv")
{for (i in 1:length(impose_list_B)){
  png(paste0("30 Bins/output/face/blue/clusters/", i , ".png"))
  plotColorClusters(impose_face_B[[i]]$centers, impose_face_B[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = i, color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}}

###############################################################################

{imgs <- lapply(y_face, brick_to_array)
names(imgs) <- names(y_face)
extent_list <- lapply(y_face, extent)
{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = 10,
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 25,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = yellow vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/y_face_fit.rds")
  rc_list <- readRDS("rds/y_face_fit.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors, which will be the final amount of colors used in the palette. Colors
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 30)
  
  # make an empty matrix for storing the new palette
  palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))
  
  # for every color in cluster_list...
  for (i in 1:length(cluster_list)) {
    
    # get the center indices 
    idx <- cluster_list[[i]]
    
    # get the average value for each channel, using cluster size to get a weighted average
    ctr <- apply(all_palettes, 2, 
                 function(j) weighted.mean(j[idx], 
                                           w = all_sizes[idx]))
    
    # store in the palette matrix
    palette[i, ] <- ctr
  }
  # This saves an image of the color palette into our folder
  png('30 bins/output/face/yellow/y_face_pal.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/y_face_pal.rds")
  #palette <- readRDS(file.choose())
  # And plot it to check it out.
  plotColorPalette(palette)
  
  {
    #palette <- readRDS(file.choose())
    #palette <- rbind(palette, c(0.75, 1, 0.25)) #p<0.05 green
    palette <- rbind(palette, c(0.25, 1, 0.25)) #p<0.01 green
    #palette <- rbind(palette, c(0.90, .1, 0.1)) #p<0.05
    palette <- rbind(palette, c(0.60, .1, 0.001)) #p<0.01
    plotColorPalette(palette)
    saveRDS(palette, "rds/fc_y_face_pal.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS("rds/y_face_fit.rds")
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/y_face_fit.rds")
  impose_face_Y <- readRDS("rds/y_face_fit.rds")
  csv <- impose_face_Y[[1]][["sizes"]]
  for (i in 1:length(impose_face_Y)){
    csv <- dplyr::bind_cols(csv, impose_face_Y[[i+1]][["sizes"]])}
  write.csv2(csv, file = "30 Bins/output/face/yellow/yellow_face_sizes.csv")
}}
{for (i in 1:length(impose_face_Y)){
  png(paste0("30 Bins/output/face/yellow/clusters/", i , ".png"))
  plotColorClusters(impose_face_Y[[i]]$centers, impose_face_Y[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = i, color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}}
blue_whole <-  readRDS('rds/b_whole_fit.rds')
blue_face <- readRDS('rds/b_face_fit.rds')
yellow_whole <- readRDS('rds/y_whole_fit.rds')
yellow_face <- readRDS('rds/y_face_fit.rds')


impose_list <- blue_face
}
##################Pot#####################
{
pos <- c(29)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[24]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == 20)] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/whole/pot/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################flee#####################
pos <- c(2,10)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/whole/flee/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}
##################lateral#####################
pos <- c(16,17,18)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/whole/lateral/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################lead#####################
pos <- c(16)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/whole/lead/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

impose_list <- yellow_whole


impose_list <- blue_face

##################chases_male#####################
pos <- c(7)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/face/chases_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################pot#####################
pos <- c(3)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/face/pot/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}
##################flee#####################
pos <- c(23,28)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/blue/face/flee/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}



##################bite_male#####################
pos <- c(1,2,17)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/whole/bite_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################chases_male#####################
pos <- c(3)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/whole/chases_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################bite_male#####################
pos <- c(3,4,5,11)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/whole/bite_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}


##################lateral#####################
pos <- c(30)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/whole/lateral/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################quiver_male#####################
pos <- c(4,7,18)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/whole/quiver_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

impose_list <- yellow_face

##################bite_male#####################
pos <- c(1,3,8,13,30)
neg <- c(21)
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/bite_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################dig#####################
pos <- c(21)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/dig/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################bite_male#####################
pos <- c(24,29)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/bite_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}
##################chases_male#####################
pos <- c(6,24)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/chases_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################pot#####################
pos <- c(21)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/pot/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################flee#####################
pos <- c()
neg <- c(29)
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/flee/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}
##################quiver_male#####################
pos <- c(5)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/quiver_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################quiver_male#####################
pos <- c(2)
neg <- c()
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    for (k in 1:length(neg)) {
      rc <- impose_list_1[[i]]
      rc$pixel_assignments[which(rc$pixel_assignments == neg[[k]])] <- 32
      #impose_list[[i]] <- rc
    }
    
    #rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/fc/yellow/face/quiver_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

}

##########################################Patternize##############################################
####################################BF Chases female T/NT#########################################
{
outline <-read.table(file.choose(), header=F)
lines <- choose.files()


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/chases_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/chases_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/chases_female_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/chases_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/chases_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/chases_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/chases_female_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/chases_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF Flee T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/flee/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/flee/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/flee_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/flee_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/flee/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/flee/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/flee_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/flee_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF pot T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/pot_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/pot_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/pot_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/face/pot_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}
}

####################################YF Bite female T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_female_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_female_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF Bite male T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_male_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_male_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/bite_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#############Next#######################YF Bite male T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/chases_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/chases_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/chases_male_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/chases_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/chases_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/chases_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/chases_male_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/chases_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF dig T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/dig/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/dig/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/dig_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/dig_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/dig/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/dig/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/dig_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/dig_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF Flee T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/flee/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/flee/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/flee_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/flee_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines=lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/flee/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/flee/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/flee_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/flee_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF pot T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/pot/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/pot/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/pot_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/pot_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/pot/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/pot/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/pot_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/pot_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF Quiver female T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/quiver_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/quiver_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_female_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/quiver_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/quiver_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_female_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################YF Quiver male T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/quiver_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/quiver_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_male_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/quiver_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/quiver_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_male_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/face/quiver_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

outline <-read.table(file.choose(), header=F)
lines <- choose.files()

####################################BW h2h T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/h2h/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/h2h/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/h2h_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/h2h_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/h2h/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/h2h/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/h2h_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/h2h_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF Flee T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/flee/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/flee/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/flee_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/flee_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/flee/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/flee/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/flee_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/flee_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF pot T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/pot/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/pot/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/pot_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/pot_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/pot/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/pot/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/pot_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/pot_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF lead T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/lead/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/lead/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/lead_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/lead_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/lead/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/lead/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/lead_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/blue/whole/lead_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

outline <-read.table(file.choose(), header=F)
lines <- choose.files()

####################################YW bite_female T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[3]],
                plotTransformed = F )
t<-imageList[[3]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/bite_female_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'T_11', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/bite_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[3]],
                plotTransformed = F )
t<-imageList[[3]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/bite_female_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/bite_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}



####################################YW Bite male T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/chases_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/chases_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[3]],
                plotTransformed = F )
t<-imageList[[3]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/chases_male_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/chases_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/chases_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/chases_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[3]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/chases_male_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/chases_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[3]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF lateral T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/lateral/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/lateral/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/lateral_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/lateral_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/lateral/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/lateral/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/lateral_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/lateral_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

####################################BF quiver_male T/NT#########################################

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/quiver_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/quiver_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/quiver_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/quiver_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]


RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')

{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'Nt_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}





