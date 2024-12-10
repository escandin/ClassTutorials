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

# To plot individual polygons for detecting typology errors.
poly =1# Polygon number
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=extent(calibdata[poly,]))
plot(calibdata[poly,], border="white", add=T)
print((calibdata$class[poly])) # prints the class name of the polygon

# Delete the problematic polygon
calibdata=calibdata[-poly,]

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
