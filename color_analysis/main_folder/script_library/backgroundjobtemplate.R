#######################Recolorize Background Job Template######################
# 


library(recolorize)
#Patternize is what we'll use to actually analyze the color and make heat maps.
library(patternize)
#This is a color package for the heat maps that I like using. It's color blindness friendly.
library(viridisLite)
#Dependency
library(raster)


{rc_list <- lapply(imgs_,
                   function(i)
                     # What this is doing is creating a loop for each of our images 
                     recolorize2(i, bins =30,
                                 #or i, bins = c(L,a,b)
                                 #this is the # of bins we want for each 
                                 #color channel. The true number is 15^3
                                 cutoff = 15,
                                 # This is the color distance that we want to compress to. 
                                 #Lower number = less compression 
                                 plotting = F, color_space = 'Lab'))
  # L = "lightness" a = red vs green, and b = blue vs blue
  # Alternate option is RGB, but Lab is more faithful to human vision
  # Save it!!
  saveRDS(rc_list, "rds/fits.rds")
  rc_list <- readRDS("rds/fits.rds")
  
  # Get all palettes and color sizes
  all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))
  all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes / sum(i$sizes)))
  
  # Cluster colors = final amount of colors used in the palette.
  # 
  cluster_list <- hclust_color(all_palettes, n_final = 10)
  
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
  png('palette.png')
  plotColorPalette(palette)
  dev.off()
  
  # We can save our palette work here.
  saveRDS(palette, "rds/palette.rds")
  
  # And plot it to check it out.
  plotColorPalette(palette)
  
  # and apply the palette to our images
  impose_list <- lapply(imgs, function(i) imposeColors(i, palette, 
                                                               adjust_centers = FALSE))
  # Then save our work
  saveRDS(impose_list, "rds/fits.rds")
  impose_list <- readRDS("rds/fits.rds")
  #layout(matrix(1:4, nrow = 1)); par(mar = rep(1, 4))
  
}

{for (i in 1:length(impose_list)){
  #The following for-loop should be as large as your palette size. Make sure
  #to make corresponding folders for each channel. My folders all have a
  #folder that serves as a default template for ease of set up.
  for (j in 1:10) {
    # This will save the color channel over a B+W image of the subject.
    {png(paste0("output/overlay/groupfolder1/groupfolder2/",j,"/",i , ".png"))
      splitByColor(impose_list[[i]], plot_method = "overlay", layers = j)
      dev.off()}
    # This will save the color channel as white against black.
    {png(paste0(("output/binary/groupfolder1/groupfolder2/",j,"/", i , ".png"))
      splitByColor(impose_list[[i]], plot_method = "binary", layers = j)
      dev.off()}
    #This will solely save the color channel against black
    {png(paste0("output/colormask/groupfolder1/groupfolder2/",j,"/", i , ".png"))
      splitByColor(impose_list[[i]], plot_method = "colormask", layers = j)
      dev.off()}}
  #This section breaks off the 2nd for-loop because it includes a recolorized 
  #image for all channels, so you just need one for each individual.
  {png(paste0("output/recolored/groupfolder1/groupfolder2/",i ,".png"))
    plot(impose_list[[i]],sizes= TRUE)
    dev.off()}
  
}}
