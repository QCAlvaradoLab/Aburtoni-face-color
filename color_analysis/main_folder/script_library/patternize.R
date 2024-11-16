#RGB segmentation of green and red
#set the working directory to the samples folder
setwd("C:/Users/qbiol/Desktop/bluegill_recolorize")
#loading required packages
library(patternize)
library(viridisLite)
library(recolorize)
library(raster)

# convert back to patternize (including extent)
#patternize_list <- lapply(impose_list, recolorize_to_patternize)
#for (i in 1:length(patternize_list)) {
#  for (j in 1:length(patternize_list[[1]])) {
#    raster::extent(patternize_list[[i]][[j]]) <- extent_list[[i]]
#  }
#}

# Here's we're loading up the image names based on their location and file type. 
listV<- tools::file_path_sans_ext(dir("crop/dv/Output/RC/ventral", ".jpg"))
# Then we're going to put the full file names into a List object so recolorize/patternize can read it.
imageListV <- makeList(list, type = "image",
                       prepath = "crop/ventral",
                       extension = ".jpg")
imageList <- rbind(imageListD, imageListV)
## Here, we're gonna align all of our images so they're all on a comparable map.

imageList <- makeList(bg, type = "image",
                      prepath = "images/Par/standard",
                      extension = ".jpeg")
H_rlist <- readRDS("total/high_recolorize_list.rds")
M_rlist <- readRDS("images/mid_patternize_list.rds")
L_rlist <- readRDS("images/low_patternize_list.rds")
#here, we're selecting an example image from within our sample set for comparison
targetH<- imageList[[1]]
targetM<- imageList[[2]]
targetL<- imageList[[2]]

#reading in the outline of our fish as drawn from FIJI
outline <- read.table('med/crop/landmarks/BG1_M_calibrated.txt', h= F)
#palette loading
IDListP <- tools::file_path_sans_ext(dir("crop/rc", ".png"))
imageListP <- makeList(IDListP, prepath = 'crop/rc', extension = '.png', 'image')
palette <- imageListP[[1]]
paletteM <- imageListP[[3]]
paletteL <- imageListP[[2]]
#analysis of high color palette
RGB1 <- sampleRGB(palette, resampleFactor = NULL, crop = c(0,0,0,0), type= "point")#col 1
RGB1 <- c(186,160,46)
RGB2 <- sampleRGB(palette, resampleFactor = NULL, crop = c(0,0,0,0), type= "point")#col 2
RGB2 <- c(129,144,153)
#RGB3 <- sampleRGB(paletteH, resampleFactor = NULL, crop = c(0,0,0,0), type= "point")#col 3
#RGB3 <- c(88,55,18)
#heatmaps:col1
rastList_regRGB1 <- patK(patternize_list, k = 10, resampleFactor = NULL, plot = TRUE)
sumRast_regRGB1 <- sumRaster(rastList_regRGB1, med, type = 'K')
# determine the number of colors
n_layers <- length(patternize_list[[1]])
IDlist <- names(patternize_list)
which_colors = 'all'
# restructure the raster list by layer instead of image
patternize_list <- lapply(1:n_layers,
                          function(i) lapply(patternize_list,
                                             function(j) j[[i]]))

# redefine to only specified layers (if not "all")
if (sum(which_colors != "all") > 0) {
  if (!is.numeric(which_colors)) {
    stop("'which_colors' must be 'all' or a numeric vector indicating which
           colors should be used for the summed PCA")
  }
  H_rlist <- H_rlist[which_colors]
  n_layers <- length(which_colors)
}

# sum rasters after masking
summedRasters_02 <- lapply(patternize_list,
                           function(i) patternize::sumRaster(i, IDlist, type = "RGB"))

# make a list for storing the raster dataframes
rasDFlist <- vector("list", length = n_layers)

# make one dataframe per layer
for (l in 1:n_layers) {
  for (r in 1:length(patternize_list[[1]])) {
    
    # isolate layer from image
    layer <- patternize_list[[l]][[r]]
    
    # swap out NA values
    layer[is.na(layer)] <- 0
    
    # convert to a dataframe
    ras <- raster::as.data.frame(layer)
    
    # either start or append the dataframe for this layer
    if (r == 1) { rasDF <- ras } else { rasDF <- cbind(rasDF, ras) }
  }
  
  # set column names and add to the list
  colnames(rasDF) <- names(patternize_list[[l]])
  rasDFlist[[l]] <- rasDF
}

# make a stacked version for the full PCA
rasDFstack <- do.call(rbind, rasDFlist)



plotHeat(summedRasters_02, IDlist, plotCartoon = FALSE, refShape = 'meanShape', flipRaster = 'x', flipOutline = 'y', imageList = patternize_list, 
         cartoonOrder = 'under', colpalette = inferno(100), adjustCoords = TRUE)


######Here on is old code from when I just used plain old patternize#############


#heatmaps:col2
rasterList_regRGB2 <- patRegRGB(imageListH, targetH, RGB2, resampleFactor = 1, colOffset= 0.10, 
                                removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = FALSE, crop = c(204,400,150,300))
sumRast_regRGB2 <- sumRaster(rasterList_regRGB2, IDListH, type = 'RGB')
plotHeat(sumRast_regRGB2, IDListH, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListH, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListH[[2]], colpalette = inferno(100))

#heatmaps:col3
#rasterList_regRGB3 <- patRegRGB(imageListH, targetH, RGB3, resampleFactor = 2, colOffset= 0.06, 
#                                removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
#sumRast_regRGB3 <- sumRaster(rasterList_regRGB3, IDListH, type = 'RGB')
#plotHeat(sumRast_regRGB3, IDListH, plotCartoon = FALSE, refShape = 'target', 
#         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
#         flipOutline = 'y', imageList = imageListH, cartoonOrder = 'under', 
#         cartoonFill = 'black', refImage = imageListH[[2]], colpalette = inferno(100))
avg_rast <- (sumRast_regRGB2 / sumRast_regRGB1)
plotHeat(avg_rast, IDListH, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(204,400,200,300), 
         flipOutline = 'y', imageList = imageListH, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListH[[2]], colpalette = inferno(100))

#analysis of mid color palette
#heatmaps:col1
rastList_regRGB4 <- patRegRGB(imageListM, targetM, RGB1, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB4 <- sumRaster(rastList_regRGB4, IDListM, type = 'RGB')
plotHeat(sumRast_regRGB4, IDListM, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListM, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListM[[2]], colpalette = inferno(100))

#heatmaps:col2
rastList_regRGB5 <- patRegRGB(imageListM, targetM, RGB2, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB5 <- sumRaster(rastList_regRGB5, IDListM, type = 'RGB')
plotHeat(sumRast_regRGB5, IDListM, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListM, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListM[[2]], colpalette = inferno(100))

#heatmaps:col3
rastList_regRGB6 <- patRegRGB(imageListM, targetM, RGB3, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB6 <- sumRaster(rastList_regRGB6, IDListM, type = 'RGB')
plotHeat(sumRast_regRGB6, IDListM, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListM, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListM[[2]], colpalette = inferno(100))
#analysis of low color palette
#heatmaps:col1
rastList_regRGB7 <- patRegRGB(imageListL, targetL, RGB1, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB7 <- sumRaster(rastList_regRGB7, IDListL, type = 'RGB')
plotHeat(sumRast_regRGB7, IDListL, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListL, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListL[[2]], colpalette = inferno(100))

#heatmaps:col2
rastList_regRGB8 <- patRegRGB(imageListL, targetL, RGB2, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB8 <- sumRaster(rastList_regRGB8, IDListL, type = 'RGB')
plotHeat(sumRast_regRGB8, IDListL, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListL, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListL[[2]], colpalette = inferno(100))

#heatmaps:col3
rastList_regRGB9 <- patRegRGB(imageListL, targetL, RGB3, resampleFactor = 3, colOffset= 0.01, 
                              removebgR = 100, plot = 'compare', focal = F, sigma = 5, iterations = 2)
sumRast_regRGB9 <- sumRaster(rastList_regRGB9, IDListL, type = 'RGB')
plotHeat(sumRast_regRGB9, IDListL, plotCartoon = FALSE, refShape = 'target', 
         outline = outline, flipRaster = 'x', crop = c(0,0,0,0), 
         flipOutline = 'y', imageList = imageListL, cartoonOrder = 'under', 
         cartoonFill = 'black', refImage = imageListL[[2]], colpalette = inferno(100))
