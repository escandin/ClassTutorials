setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials/")

library(raster)
library(RStoolbox)
library(rgdal)
library(maptools)
dir()

imagename="LC08_L1TP_006066_20160804_20170322_01_T1"

untar(paste(imagename, "tar", sep="."))
meta=readMeta(paste(imagename, "MTL.txt", sep="_"))
inimage=stackMeta(meta)
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

e=drawExtent()
inimage=crop(inimage,e)

plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

e=drawExtent()
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)

# Collect training polygons. Make sure classname for each land cover category is spelled exactly the same
# Otherwise it will be considered as another class
calibdata=collectpoly(newstack=TRUE, classname="urban", stackcrs=crs(inimage)) # New object only
calibdata=collectpoly(classname="forest", 
                      stackname="calibdata", stackcrs=crs(calibdata)) # add polygon to existing object

table(calibdata$class) # This is to count the number of polygons per class

###### CAUTION!!!This function removes the last polygon that you entered.
calibdata=calibdata[-nrow(calibdata),]

save(calibdata, file="calibdata.RData") # save the file as an R object
writeOGR(calibdata, dsn=getwd(), layer="calibdata", driver="ESRI Shapefile")
#writeOGR(calibdata, dsn="outputs.GeoJSON", layer="calibdata", driver="GeoJSON")
