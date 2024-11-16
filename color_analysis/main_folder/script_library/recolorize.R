# Loading the required packages ####
# Recolorize is necessary to give all of the images in the data set a singular color palette
library(recolorize)
#Patternize is what we'll use to actually analyze the color and make heat maps.
library(patternize)
#This is a color package for the heat maps that I like using. It's color blindness friendly.
library(viridisLite)
#Dependency
library(raster)

#We'll set our working directory to the main folder where all our data is. Be sure to change this when analyzing other stuff.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Here's we're loading up the image names based on their location and file type. 
list <- tools::file_path_sans_ext(dir("img", ".jpg"))
# Then we're going to put the full file names into a List object so recolorize/patternize can read it.
imageList <- makeList(list, type = "image",
                      prepath = "img",
                      extension = ".jpg")

## Here, we're gonna align all of our images so they're all on a comparable map.
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[1]],
                maskOutline = NULL, plotTransformed = TRUE)

# Then save all of that work to a saved object file.
saveRDS(reg, file = 'rds/reg_aligned.rds')
# We can load the imageList_aligned object without doing all that work all over again:
NT_B_aligned <- readRDS(file.choose())
NT_Y_aligned <- readRDS(file.choose())
T_B_aligned <- readRDS(file.choose())
T_Y_aligned <- readRDS(file.choose())
imageList_aligned <- c(NT_B_aligned, NT_Y_aligned, T_B_aligned, T_Y_aligned)
# We're going to convert from RasterBricks to image arrays so our images can talk to recolorize:
imgs <- lapply(imageList_aligned, brick_to_array)
names(imgs) <- names(imageList_aligned)

# We're going to save raster extents:
extent_list <- lapply(imageList_aligned, extent)

################# fitting initial recolorize fits##########
# Okay, showtime. This is the actual recolorize function. To show you what 
#recolorize is doing, we'll be putting plotting = T, but you don't normally need 
#this and we can skip to save time. These curly brackets ({}) mean we're running 
#everything inside these brackets as one chunk of code together. 
{rc_list <- lapply(imgs,
                   function(i)
# What this is doing is creating a loop for each of our images 
                                recolorize2(i, bins = 15,
                                            #this is the # of bins we want for each 
                                            #color channel. The true number is 30^3
                                           cutoff = 20,
                                           # This is the color distance that we want to compress to. 
                                           #Lower number = less compression 
                                           plotting = F, color_space = 'Lab'))
                                           # L = "lightness" a = red vs green, and b = blue vs yellow
                                           # Alternate option is RGB, but Lab is more faithful to human vision
# Save it!!
saveRDS(rc_list, "rds/fits.rds")
rc_list <- readRDS("rds/fits.rds")

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
layout(matrix(1:4, nrow = 1)); par(mar = rep(1, 4))
}
# Here, we can begin to check our work and see if our model is fitting properly.

# Indicate which image 
i <- 4
rc <- impose_list[[i]]
plot(rc, sizes = F)

# If pixel assignments look off, or you think a color is actually closer to another,
# you can merge them here, where we're changing 7 to 8
rc$pixel_assignments[which(rc$pixel_assignments == 7)] <- 8

# this line would absorb speckles of color 2 below the size_condition threshold:
rc <- absorbLayer(rc, 5, size_condition = function(s) s <= 1500, plotting = TRUE)

# plot result and (if  it looks good) slot it back into the list:
plot(rc, sizes = F)
impose_list[[i]] <- rc

# We can also generate plots looking at the color clusters, so we can see 
# differences between individuals before actual analysis 
plotColorClusters(impose_list[[1]]$centers, impose_list[[1]]$sizes, scaling = 25, plus = 1,
                  xlab = "b", ylab = "a", zlab = "Lum",
                  main = "Color clusters", color_space = "sRGB",  phi = 45,
                  theta = 45, alpha = 0.3)
# save result:
saveRDS(impose_list, "rds/recolorize_list.rds")
impose_list <- readRDS("rds/recolorize_list.rds")

##Saving different color channels for each fish
# We might want to do this if we are okay with stopping here and exporting these
# color channels into FIJI or something else. You'll need to do this for each channel

for (i in 1:length(impose_list)){
  # This will save the color channel over a B+W image of the subject.
  {png(paste0("example1/Output/Overlay/Ch1/Ch1_", i , ".png"))
    splitByColor(impose_list[[i]], plot_method = "overlay", layers = 1)
    dev.off()}
  # This will save the color channel as white against black.
  {png(paste0("example1/Output/Binary/Ch1/Ch1_", i , ".png"))
    splitByColor(impose_list[[i]], plot_method = "binary", layers = 1)
    dev.off()}
  # This will solely save the color channel against black
  {png(paste0("example1/Output/Colormask/Ch1/Ch1_", i , ".png"))
    splitByColor(impose_list[[i]], plot_method = "colormask", layers = 1)
    dev.off()}
}
# Save each of the images so we can use them in patternize or ImageJ
png::writePNG(recoloredImage(impose_list[[1]]),
              target = "example1/Output/Recolored/sample_01.jpg")

## Currently, I'm working on reverse engineering the interactions between recolorize
## and patternize. For now, you can export the images as .jpgs and use them in patternize.

