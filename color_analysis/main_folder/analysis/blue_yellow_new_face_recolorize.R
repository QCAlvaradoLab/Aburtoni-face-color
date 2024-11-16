# Loading the required packages ####
library(recolorize)
library(patternize)
library(raster)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


list_T <- tools::file_path_sans_ext(dir("Cropped/T Blue/face", ".jpg"))
imageList_T <- makeList(list_T, type = "image",
                      prepath = "Cropped/T Blue/face/",
                      extension = ".jpg")
list_NT <- tools::file_path_sans_ext(dir("Cropped/NT Blue/face", ".jpg"))
imageList_NT <- makeList(list_NT, type = "image",
                        prepath = "Cropped/NT Blue/face/",
                        extension = ".jpg")
imageList <- c(imageList_T,imageList_NT)
outline <-read.table(file.choose(), header=F)
aligned_images <- alignReg(imageList, resampleFactor = NULL, target = imageList[[14]],
                maskOutline = outline, plotTransformed = TRUE)
saveRDS(aligned_images, file = 'rds/aligned_new_blue_faces.rds')
list_T <- tools::file_path_sans_ext(dir("Cropped/T Yellow/face", ".jpg"))
imageList_T <- makeList(list_T, type = "image",
                        prepath = "Cropped/T Yellow/face/",
                        extension = ".jpg")
list_NT <- tools::file_path_sans_ext(dir("Cropped/NT Yellow/face", ".jpg"))
imageList_NT <- makeList(list_NT, type = "image",
                         prepath = "Cropped/NT Yellow/face/",
                         extension = ".jpg")
imageList <- c(imageList_T,imageList_NT)
aligned_images <- alignReg(imageList, resampleFactor = NULL, target = imageList[[2]],
                           maskOutline = outline, plotTransformed = TRUE)
saveRDS(aligned_images, file = 'rds/aligned_new_yellow_faces.rds')

##########Blue Faces#############
aligned_blue <- readRDS('rds/aligned_new_blue_faces.rds')
imgs <- lapply(aligned_blue, brick_to_array)
names(imgs) <- names(aligned_blue)
extent_list <- lapply(aligned_blue, extent)

{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = c(15,25,40),
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 15,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = blue vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/new_blue_face_fits.rds")
  rc_list <- readRDS("rds/new_blue_face_fits.rds")
  
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
  png('new_blue_face_palette.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/new_blue_face_palette.rds")
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
    saveRDS(palette, "rds/new_fc_blue_face_palette.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS('rds/new_blue_face_fits.rds')
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/new_blue_face_fits.rds")
  impose_whole_B <- readRDS("rds/new_blue_face_fits.rds")
  layout(matrix(1:4, nrow = 1)); par(mar = rep(1, 4))
}

for (i in 1:length(impose_list)){
  png(paste0("30 Bins/output/face_clusters/blue/", i , ".png"))
  plotColorClusters(impose_list[[i]]$centers, impose_list[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = list[[i]], color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}


##########Yellow Faces#############
aligned_yellow <- readRDS('rds/aligned_new_yellow_faces.rds')
imgs <- lapply(aligned_yellow, brick_to_array)
names(imgs) <- names(aligned_yellow)
extent_list <- lapply(aligned_yellow, extent)

{rc_list <- lapply(imgs,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = c(15,25,40),
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 15,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = yellow vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/new_yellow_face_fits.rds")
  rc_list <- readRDS("rds/new_yellow_face_fits.rds")
  
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
  png('new_yellow_face_palette.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/new_yellow_face_palette.rds")
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
    saveRDS(palette, "rds/new_fc_yellow_face_palette.rds")
    #palette <- readRDS ("rds/fcpalette.rds")
  }
  
  impose_list <- readRDS('rds/new_yellow_face_fits.rds')
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                       adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/new_yellow_face_fits.rds")
  impose_list_y <- readRDS("rds/new_yellow_face_fits.rds")
  layout(matrix(1:4, nrow = 1)); par(mar = rep(1, 4))
}

for (i in 1:length(impose_list)){
  png(paste0("30 Bins/output/face_clusters/yellow/", i , ".png"))
  plotColorClusters(impose_list[[i]]$centers, impose_list[[i]]$sizes, scaling = 25, plus = 1,
                    xlab = "b", ylab = "a", zlab = "Lum",
                    main = list[[i]], color_space = "sRGB",  phi = 20,
                    theta = 20, alpha = 0.3)
  dev.off()}

library(recolorize)
library(patternize)
library(raster)
setwd("C:/Users/mhack/Dropbox (Medgar Evers College)/Dyad/Matt Peroš/Dyad Color Analysis/images")

impose_list <- readRDS("30 bins/rds/blue_whole_fcfits.rds")

##################Chases male#####################
pos <- c()
neg <- c(18)
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
    {png(paste0("30 bins/output/fc/blue/whole/chases_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################Dig#####################
pos <- c(17)
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
    {png(paste0("30 bins/output/fc/blue/whole/dig/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################Flee#####################
pos <- c(5,9,27,19)
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

impose_list <- readRDS("30 bins/rds/yellow_whole_fcfits.rds")

##################Bite female#####################
pos <- c(2,4)
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
    {png(paste0("30 bins/output/fc/yellow/whole/bite_female/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}


##################Bite male#####################
pos <- c(13,29)
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

##################Lateral#####################
pos <- c(18,23)
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
    {png(paste0("30 bins/output/fc/yellow/whole/lateral_display/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################Quiver Male#####################
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
    {png(paste0("30 bins/output/fc/yellow/whole/quiver_male/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

impose_list <- readRDS("rds/new_blue_face_fits.rds")

##################Chases female#####################
pos <- c(18)
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

##################Pot#####################
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
    {png(paste0("30 bins/output/fc/blue/face/pot/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################Flee#####################
pos <- c(15,26)
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

impose_list <- readRDS("rds/new_yellow_face_fits.rds")

##################Bite female#####################
pos <- c(4,5,8,15)
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
    {png(paste0("30 bins/output/fc/yellow/face/bite_female/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }
    #dev.off()
  }}}

##################Dig#####################
pos <- c(27)
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


###########################Run#########Chases Male T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/chases_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/chases_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/chases_male_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/chases_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

###########################Run#########Chases Male NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/chases_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/chases_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/chases_male_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/chases_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}
##############Run######################Dig T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/dig/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/dig/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/dig_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/dig_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

##############Run######################Dig NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/dig/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/dig/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/dig_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/dig_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

########################Run############Chases Male T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/flee/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/flee/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/flee_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

##########################Run##########Chases Male NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/whole/flee/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/whole/flee/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/flee_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/whole/flee_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}



#########################Re-run###########Face Chases Female T#########################################
#outline_face <-read.table(file.choose(), header=F)
#lines <- choose.files()

list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Run###########Face Chases female NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#######################Run#############Face Flee T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/flee/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/flee/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/flee_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/flee_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#######################Run#############Face Flee NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/flee/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/flee/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/flee_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/flee_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

######################Run##############Face Pot T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#######################Run#############Face Pot NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/blue/face/pot/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/blue/face/pot/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/blue/face/pot_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

################################################################################
#########################Ready###########Bite female T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_female_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite female NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_female_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_male_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/bite_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/bite_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_male_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/bite_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/chases_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/chases_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/chases_male_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/chases_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/chases_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/chases_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/chases_male_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/chases_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Lateral display T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/lateral_display/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/lateral_display/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/lateral_display_t_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/lateral_display_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Lateral display NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/lateral_display/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/lateral_display/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/lateral_display_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/lateral_display_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/quiver_male/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/quiver_male/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Bite male NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/whole/quiver_male/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/whole/quiver_male/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_nt_pos.png"))
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
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/whole/quiver_male_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Face Bite female T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_female/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_female/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/bite_female_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/bite_female_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Face Bite female NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/bite_female/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/bite_female/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/bite_female_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/bite_female_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}
#########################Ready###########Face Bite female T#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/dig/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/dig/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/dig_t_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/dig_t_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

#########################Ready###########Face Bite female NT#########################################


list <- tools::file_path_sans_ext(dir("30 bins/output/fc/yellow/face/dig/nt/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/fc/yellow/face/dig/nt/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

#outline <-read.table(file.choose(), header=F)
#lines <- choose.files()
RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/dig_nt_pos.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}

RGB <- c(153,26,0)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
#colfunc <- colorspace::choose_palette()
{png(paste0("30 bins/output/fc/yellow/face/dig_nt_neg.png"))
  plotHeat(sum, list, plotCartoon = T, outline = outline_face, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, 
           colpalette= RColorBrewer::brewer.pal(8,'Greens'), refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  
  dev.off()
}
