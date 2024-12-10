setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials")

library(raster)
library(RStoolbox)
library(rgdal)
library(maptools)
library(varSel)
library(rgl)
library(randomForest)
library(lulcc)
library(prettymapr)
library(randomcoloR)

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
calibdata=collectpoly(newstack=TRUE, classname="urban", 
                      stackcrs=crs(inimage)) # New object only
calibdata=collectpoly(classname="pasture", 
                      stackname="calibdata", stackcrs=crs(calibdata)) # add polygon to existing object

table(calibdata$class) # This is to count the number of polygons per class

plot(calibdata, add=T)

###### CAUTION!!!These functions delete polygons
# To delete the last polygon that you entered.
calibdata=calibdata[-nrow(calibdata),]

# To delete all polygons of one class
calibdata=calibdata[-which(calibdata$class=="bosque", arr.ind=TRUE),]

# To merge two categories
calibdata$class[which(calibdata$class=="pasto", arr.ind=TRUE)]="lago"

######################
# Plot spectral space for all categories
outdata=stack2df(rastack=inimage, spdf=calibdata,
                 classnames=c("urbano", "lago", "bosque", "pasto", "arena", "rio",  "secondary"))

plotSpectra(dataset=outdata, bandnames=c("B3_dn", "B4_dn", "B5_dn"), classfield=2,
            classlabels=unique(outdata$class_name),
            classcol=c("green", "blue", "orange", "gray", "pink", "red", "purple"))

legend3d("topleft", legend= as.factor(unique(calibdata$class)),
         col=c("green", "blue", "orange", "gray", "pink", "red", "purple"),
         pch=16, cex=1, inset=c(0.02))

#Separability analysis
JMdist(outdata[,2], X=outdata[,3:ncol(outdata)])

## PEFORM SUPERVISED AND UNSUPERVISED CLASSIFICATION
unsupervised=unsuperClass(inimage, nClasses=15)
plot(unsupervised$map, col=sample(colors(), 15))

# To split the training data into calibration and validation
supervised=superClass(inimage, calibdata, trainPartition=.8,
                      responseCol = "class" )
plot(supervised$map, col=sample(colors(), length(unique(calibdata$class))))

# To see the confusion matrix + accuracy results
supervised$validation$performance

# To save data
save(calibdata, file="calibdata.RData") # save the polygons as an R object
writeOGR(calibdata, dsn=getwd(), layer="calibdata", # save polygons as shape file
         driver="ESRI Shapefile")
# To export the classified map as geotiff
writeRaster(supervised$map, filename="supervised.tif", 
            filetype="GTiff")
writeRaster(unsupervised$map, filename="unsupervised.tif", 
            filetype="GTiff")
writeRaster(inimage, filename="inimage.tif", 
            filetype="GTiff")

##########################################
######### OPEN FILES #####################
##########################################
load("calibdata.RData")
supervised=raster("supervised.tif")
unsupervised=raster("unsupervised.tif")
inimage=stack("inimage")

# Create map with supervised classification
catnames=slot(supervised@data, name="attributes")[[1]] # check class names
mapcol=rev(c("red", "orange", "blue", "yellow", "purple", "green", "pink"))
plot(supervised, col=mapcol, axes=FALSE, legend=FALSE, box=FALSE)
legend("topright", legend = catnames$category[2:length(catnames$category)],
       fill = mapcol, cex=0.7, bg="white")#, inset=c(-0.1,0))

# Add north arrow, scalebar
prettymapr::addnortharrow(pos="bottomright", scale = 0.6, padin=c(0.5,0.1),
                          text.col = 'black', cols = c('black', 'black'))
prettymapr::addscalebar(pos="bottomleft", plotunit = 'm', widthhint = 0.25, lwd = 1, 
                        padin = c(0.5, 0.1), label.cex = 0.9)

# PLOT MAPS
# Original image
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)

################################################################################
################################################################################
# Reclassify unsupervised classification map (BONUS)

# Select an extent in the input image. 
# Then zoom into it to interpret the names of each unsupervised class by
# comparing the input RGB image with the classes assigned to different pixels

par(mfrow=c(1,1))
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")
e=drawExtent()
par(mfrow=c(2,1))
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)
plot(unsupervised$map, ext=e, axes=FALSE, col=unsupercol, legend=FALSE, 
     box=FALSE, add=T, alpha=1) #
click(unsupervised$map, n=5, id=T)

# Below are groups of three values. The third value is the class number that will be assigned  
# in the reclassified map to all pixels that are classified with a value laying with the range
# defined by the first two numbers.
# For example, all pixels that are classified as 7 and 8 in the original map, will be classified
# as 2 in the new map.

m=c(0.5,1.5,7, 1.5,3.5,2, 3.5,4.5,NA, 
    4.5,5.5,2, 5.5,6.5,6, 6.5,8.5,2,
    8.5,9.5,6, 9.5,10.5,4, 10.5,11.5,3,
    11.5,12.5,5, 
    12.5,13.5,NA, 13.5,14.5,NA, 14.5,15.5,1)
reclassmat=matrix(m, ncol=3, byrow=TRUE)

# This reclassifies the unsupervised map
unsupreclass=reclassify(unsupervised$map, reclassmat)

# Plot the unsupervised classification map
unsupreclass
catnames # check the names for the categories in the supervised classification
unsupercol=distinctColorPalette(k = 15) # These are the colors to be used for mapping the unsupervised map
classcol=sort(unique(values(unsupreclass))) #check the classes in the unsupervised map
plot(unsupreclass, col=mapcol, axes=FALSE, legend=FALSE, box=FALSE)
legend("topright", legend = catnames$category[2:length(catnames$category)],
       fill = mapcol, cex=0.7)#, inset=c(-0.1,0))
prettymapr::addnortharrow(pos="bottomright", scale = 0.6, padin=c(0.5,0.1),
                          text.col = 'black', cols = c('black', 'black'))
prettymapr::addscalebar(pos="bottomleft", plotunit = 'm', widthhint = 0.25, lwd = 1, 
                        padin = c(0.5, 0.1), label.cex = 0.9)

# Assessing change. Each row represents the classes in the earlier year 
# that were converted to another class in the second year represented by columns
change=crossTabulate(supervised, unsupreclass, categories=classcol)




spplot(temp, legend=FALSE, axes=FALSE) #ext=e, col=unsupercol, box=FALSE) #


