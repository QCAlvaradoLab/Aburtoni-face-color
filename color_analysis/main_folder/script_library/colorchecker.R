setwd("C:/Users/qbiol/Desktop/colordemo")
#loading required packages
library(patternize)
library(viridis)
library(imager)
library(magick)
IDList <- tools::file_path_sans_ext(dir("example1/Original", ".JPG"))
prepath <- 'example1'
extension <- '.JPG'

colorChecker(IDList, prepath, extension, patchSize = 0.5)
