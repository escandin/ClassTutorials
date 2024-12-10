library(raster)
library(RStoolbox)
library(rgdal)
library(lulcc)

setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2019Spring/Class10/Lab10Materials")

first=raster("Pucallpa07CROP.tif")
last=raster("Pucallpa17CROP.tif")

classcolors= c("purple", "orange", "lightgreen", "yellow", "blue", "red")

plot(first, col=classcolors)
plot(last, col=classcolors)

# This produces a contingency table with the number of pixels per category
# in time 1 (columns) and in time 2 (rows)
change=crossTabulate(first, last, categories=seq(1,6))

# multiply by the pixel size to obtain the area. 
# the area of a landsat pixel is 30 x 30 m = 900 m2 = 0.09 ha
change_ha=change*0.09

# Total area in category 1 in time 1
sum(change_ha[1,])

# Total area in category 1 in time 2
sum(change_ha[,1])

# To export the change object as an excel compatible file

write.csv(change_ha, "change_ha.csv")
