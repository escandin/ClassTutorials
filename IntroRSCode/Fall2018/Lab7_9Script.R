setwd("C:/Data/Downloads/Lab8")

library(raster)
library(RStoolbox)
library(rgdal)
library(maptools)
library(varSel)
library(rgl)
library(randomForest)
dir()

imagename="LC08_L1TP_006066_20160804_20170322_01_T1"

untar(paste(imagename, "tar", sep="."))
meta=readMeta(paste(imagename, "MTL.txt", sep="_"))
inimage=stackMeta(meta)
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

e=drawExtent()
inimage=crop(inimage,e)

plotRGB(inimage, r=5, g=4, b=3, stretch="lin")

plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=drawExtent())
plot(calibdata, add=T)

# Collect training polygons. Make sure classname for each land cover category is spelled exactly the same
# Otherwise it will be considered as another class
calibdata=collectpoly(newstack=TRUE, classname="urban", 
                      stackcrs=crs(inimage)) # New object only
calibdata=collectpoly(classname="urbano", 
                      stackname="calibdata", stackcrs=crs(calibdata)) # add polygon to existing object

table(calibdata$class) # This is to count the number of polygons per class

###### CAUTION!!!These functions delete polygons
# To delete the last polygon that you entered.
calibdata=calibdata[-nrow(calibdata),]

# To delete all polygons of one class
calibdata=calibdata[-which(calibdata$class=="bosque", arr.ind=TRUE),]

# To merge two categories
calibdata$class[which(calibdata$class=="pasto", arr.ind=TRUE)]="lago"

# Plot spectral space for all categories
outdata=stack2df(rastack=inimage, spdf=calibdata,
                 classnames=c("urbano", "lago", "bosque", "pasto", "arena", "rio",  "secondary"))

plotSpectra(dataset=outdata, bandnames=c("B3_dn", "B4_dn", "B5_dn"), classfield=1,
            classlabels=unique(outdata$class_name),
            classcol=c("green", "blue", "orange", "gray", "pink", "red", "purple"))

legend3d("topleft", legend=as.factor(c("forest", "lake", "pasture",
                              "river", "sand", "urban")),
         col=c("green", "blue", "orange", "gray", "pink", "red"),
         pch=16, cex=1, inset=c(0.02))

# plotSpectra=function(dataset=outdata, bandnames=c("B3_dn", "B4_dn", "B5_dn"),
#                      classfield=1, classlabels= levels(dataset[,classfield]),
#                      classcol=sample(colors(), size=length(classlabels)))

#Separability analysis
JMdist(outdata[,1], X=outdata[,3:ncol(outdata)])

# remove one undesired class from an object
calibdata=calibdata[-which(calibdata$CLASS_NAME=="Algae"),]
calibdata$CLASS_NAME=factor(calibdata$CLASS_NAME)
calibdata$CLASS_NAME

# Split into calibration and validation polygons (a la brava)
validindex=c(1, 3, 4, 5, 6, 7, 19, 38, 43, 109, 115, 121, 148, 153, 213)
             
calibdata$CLASS_NAME[validindex]
validdata=calibdata[validindex,]
calibsubset=calibdata[-validindex,]
unique(validdata$CLASS_NAME)
unique(calibsubset$CLASS_NAME)
### Supervised classification
temp=superClass(inimage, calibsubset, validata, responseCol = "CLASS_NAME")
plot(temp$map, col=c("green", "blue", "orange", "gray", "pink", "red", "purple", "yellow", 
                        "black", "aquamarine", "coral", "chocolate","lavender", "tomato"))
tempuns= unsuperClass(inimage, nClasses=30)


### Unsupervised classification


save(calibdata, file="calibdata.RData") # save the file as an R object
load("calibdata.RData") # loads the data to the environment
writeOGR(calibdata, dsn=getwd(), layer="calibdata", driver="ESRI Shapefile")
#writeOGR(calibdata, dsn="outputs.json", layer="calibdata", driver="GeoJSON")
