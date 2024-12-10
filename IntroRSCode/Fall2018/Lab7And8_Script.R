setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials/")

library(raster)
library(RStoolbox)
library(rgdal)
library(maptools)
#install.packages("plot3D")
#install.packages("rgl")
#install.packages("varSel")
library(varSel)
library(rgl)
#library(plot3D)
dir()


################## LAB 7 ############################
######################################################
######################################################

imagename="LC08_L1TP_006066_20160804_20170322_01_T1"

#untar(paste(imagename, "tar", sep="."))
meta=readMeta(paste(imagename, "MTL.txt", sep="_"))
inimage=stackMeta(meta)
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

e=drawExtent()
inimage=crop(inimage,e)

plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

# Draw polygons
e=drawExtent()
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)

# Collect training polygons. Make sure classname for each land cover category is spelled exactly the same
# Otherwise it will be considered as another class
calibdata=collectpoly(newstack=TRUE, classname="urbano", stackcrs=crs(inimage)) # New object only
calibdata=collectpoly(classname="pasto", 
                      stackname="calibdata", stackcrs=crs(calibdata)) # add polygon to existing object

table(calibdata$class) # This is to count the number of polygons per class

###### CAUTION!!!This function removes the last polygon that you entered.
calibdata=calibdata[-nrow(calibdata),]

################## LAB 8 ############################
######################################################
######################################################

# Plot individual polygons
poly =1# Polygon number
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=extent(calibdata[poly,]))
plot(calibdata[poly,], border="white", add=T)
print((calibdata$class[poly])) # prints the class name of the polygon

# extract into a data frame
outdata=stack2df(inimage, calibdata,
                 c("bosque", "lago","rio", "arena", "pasto", "urbano"))


# Separability measure
JMdist(outdata[,1], X=outdata[,3:ncol(outdata)])

# plot classes
#plotSpectra(outdata, c("B5_dn", "B4_dn", "B3_dn"))
plotSpectra(outdata, c("B5_dn", "B4_dn", "B3_dn"), 
            classlabels=c("bosque", "lago","rio", "arena", "pasto", "urbano"),
            classfield=2,
            classcol=c("green", "blue", "gray", "pink", "orange", "red"))

legend3d("topleft", legend = c("bosque", "lago","rio", "arena", "pasto", "urbano"), 
         pch = 16, col = c("green", "blue", "gray", "pink", "orange", "red") 
         , cex=1, inset=c(0.02))

# Supervised classification
formula=as.factor(class_ID)~b2+b3+b4+b5

library(rpart)
treeclass=rpart(formula, data=calibdf, na.action=na.rpart,
                method='class')

library(rpart.plot)
rpart.plot(treeclass, type=3, cex=0.5)




save(calibdata, file="calibdata.RData") # save the file as an R object
writeOGR(calibdata, dsn=getwd(), layer="calibdata", driver="ESRI Shapefile")
#writeOGR(calibdata, dsn="outputs.GeoJSON", layer="calibdata", driver="GeoJSON")
load("calibdata.RData")
