########################Overlapping heatmaps#############################
# Loading the required packages ####
library(recolorize)
library(patternize)
library(raster)
setwd("C:/Users/mhack/Dropbox (Medgar Evers College)/Dyad/Matt Peroš/Dyad Color Analysis/images")
#setwd("C:/Users/Kimberly/Queens College Dropbox/Maral Tajerian/2023/Dyad/Matt Peros/Dyad Color Analysis/images")
outline <-read.table(file.choose(), header=F)
lines <- choose.files()
impose_list <- readRDS(file.choose())
{library(recolorize)
library(patternize)
library(raster)
setwd("C:/Users/mhack/Dropbox (Medgar Evers College)/Dyad/Matt Peroš/Dyad Color Analysis/images")
#setwd("C:/Users/Kimberly/Queens College Dropbox/Maral Tajerian/2023/Dyad/Matt Peros/Dyad Color Analysis/images")
pos <- 29
impose_list_1 <- impose_list
{for (i in 1:length(impose_list_1)) {
  for (j in 1:length(pos)) {
    rc <- impose_list_1[[i]]
    rc$pixel_assignments[which(rc$pixel_assignments == pos[[j]])] <- 31
    #impose_list_1[[i]] <- rc
    {png(paste0("30 bins/output/overlays/blue/whole/29/",i, ".png"))
      plotImageArray(constructImage(rc$pixel_assignments,rc$centers,background_color="white"))
      dev.off()
    }}
    #dev.off()
  }}}
##########################blue face#################################
library(viridisLite)
v <- viridis(3)
c1 <- c('#440154FF') #Color 5 :^D
c2 <- c("#21908CFF") #Color 6 :^D
c3 <- c("#FDE725FF") #color 7 :^D

{ library(recolorize)
  library(patternize)
  library(raster)
  
  list <- tools::file_path_sans_ext(dir("30 bins/output/overlays/yellow/face/19/t/", ".png"))
imageList <- makeList(list, type = "image",
                      prepath ="30 bins/output/overlays/yellow/face/19/t/",
                      extension = ".png")
reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                plotTransformed = F )
t<-imageList[[17]]

RGB <- c(64, 255, 64)
pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
sum <- sumRaster(pat,list, type = 'RGB')
dev.off()
par(bg=NA)
      plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= c7, refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
  }

 {list <- tools::file_path_sans_ext(dir("30 bins/output/overlays/yellow/face/19/nt/", ".png"))
  imageList <- makeList(list, type = "image",
                        prepath ="30 bins/output/overlays/yellow/face/19/nt/",
                        extension = ".png")
  reg <- alignReg(imageList, resampleFactor = NULL, target = imageList[[17]],
                  plotTransformed = F )
  t<-imageList[[17]]
  
  
  RGB <- c(64, 255, 64)
  pat<- patRegRGB(reg, t, RGB, resampleFactor=NULL, useBlockPercentage=100, 
                  colOffset=0.05, crop=c(0,0,0,0), removebgR=NULL, maskOutline=NULL, 
                  focal=FALSE, sigma=3, iterations=0, patternsToFile=NULL, plot = 'compare' )
  sum <- sumRaster(pat,list, type = 'RGB')
  dev.off()
  par(bg=NA)
            plotHeat(sum, list, plotCartoon = T, outline = outline, crop = c(0,0,0,0),
           cartoonID = 'NT_03', cartoonFill = F, refShape = 'target', legend.side = 4, 
           landList = NULL, adjustCoords = F, imageList = imageList, lines = lines,
           colpalette= c3, refImage=imageList[[17]],
           cartoonOrder = 'under', legendTitle = 'Similarity', flipRaster = 'x', flipOutline ="y", legend=F)
}
