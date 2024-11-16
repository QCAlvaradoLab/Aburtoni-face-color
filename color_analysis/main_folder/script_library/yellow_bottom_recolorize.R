############################Yellow Bottom###################################



library(recolorize)
#Patternize is what we'll use to actually analyze the color and make heat maps.
library(patternize)
#This is a color package for the heat maps that I like using. It's color blindness friendly.
library(viridisLite)
#Dependency
library(raster)


{rc_list <- lapply(imgs_Y_bottom,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins = c(15,40,25),
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 25,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = blue vs yellow
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/Yfits_bottom.rds")
  rc_list <- readRDS("rds/Yfits_bottom.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors, which will be the final amount of colors used in the palette. Colors
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 15)
  
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
  png('Ypalette_bottom.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/Ypalette_bottom.rds")
  
  # And plot it to check it out.
  plotColorPalette(palette)
  
  # and apply the palette to our images
  impose_list <- lapply(imgs_Y_bottom, function(i) imposeColors(i, palette, 
                                                             adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/Yfits_bottom.rds")
  impose_list <- readRDS("rds/Yfits_bottom.rds")
  layout(matrix(1:4, nrow = 1)); par(mar = rep(1, 4))
  
}

{for (i in 1:length(impose_list)){
  for (j in 1:15) {
    # This will save the color channel over a B+W image of the subject.
    {png(paste0("output/overlay/Y/bottom/",j,"/",i , ".png"))
      splitByColor(impose_list[[i]], plot_method = "overlay", layers = j)
      dev.off()}
  # This will save the color channel as white against black.
  {png(paste0("output/binary/Y/bottom/",j,"/", i , ".png"))
    splitByColor(impose_list[[i]], plot_method = "binary", layers = j)
    dev.off()}
  #This will solely save the color channel against black
  {png(paste0("output/colormask/Y/bottom/",j,"/", i , ".png"))
    splitByColor(impose_list[[i]], plot_method = "colormask", layers = j)
    dev.off()}}}
  #{png(paste0("output/recolored/bottom/",i ,".png"))
  # recoloredImage(impose_list[[i]])
  #  dev.off()}
  
}
