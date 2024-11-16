# Loading the required packages ####
# Recolorize is necessary to give all of the images in the data set a singular color palette
library(recolorize)
#Patternize is what we'll use to actually analyze the color and make heat maps.
library(patternize)
#This is a color package for the heat maps that I like using. It's color blindness friendly.
library(viridisLite)
#Dependency
library(raster)

#We'll set our working directory to the main folder where all our data is. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Here's we're loading up the image names based on their location and file type. 
list <- tools::file_path_sans_ext(dir("images/folder1", ".jpg"))
# Then we're going to put the full file names into a List object so recolorize/patternize can read it.
imageList <- makeList(list, type = "image",
                      prepath = "images/folder1",
                      extension = ".jpg")

## Here, we're gonna align all of our images so they're all on a comparable map.
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[1]],
                maskOutline = NULL, plotTransformed = TRUE)

# Then save all of that work to a saved object file.
saveRDS(reg, file = 'rds/aligned_images.rds')

# We can load the imageList_aligned object without doing all that work all over again:
aligned_images <- readRDS(file.choose())


# We're going to convert from RasterBricks to image arrays so our images can talk to recolorize:
imgs <- lapply(aligned_images, brick_to_array)
names(imgs) <- names(aligned_images)

# We're going to save raster extents:
extent_list <- lapply(aligned_images, extent)

## Continue setting up multiple groups and when you're ready, set up the 
## background jobs template to have it all run in a batch!