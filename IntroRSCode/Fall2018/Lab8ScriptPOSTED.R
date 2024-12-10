setwd("C:/Data/Downloads/Lab8")

library(raster)
library(RStoolbox)
library(rgdal)
library(maptools)
library(varSel)
library(rgl)
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

###### CAUTION!!!This function removes the last polygon that you entered.
calibdata=calibdata[-nrow(calibdata),]

outdata=stack2df(rastack=inimage, spdf=calibdata,
                 classnames=c("forest", "lake", "pasture",
                              "river", "sand", "urban"))
plotSpectra(dataset=outdata, bandnames=c("B3_dn", "B4_dn", "B5_dn"), classfield=2,
            classlabels=c("forest", "lake", "pasture",
                         "river", "sand", "urban"),
            classcol=c("green", "blue", "orange", "gray", "pink", "red"))

legend3d("topleft", legend=as.factor(c("forest", "lake", "pasture",
                              "river", "sand", "urban")),
         col=c("green", "blue", "orange", "gray", "pink", "red"),
         pch=16, cex=1, inset=c(0.02))

# plotSpectra=function(dataset=outdata, bandnames=c("B3_dn", "B4_dn", "B5_dn"),
#                      classfield=1, classlabels= levels(dataset[,classfield]),
#                      classcol=sample(colors(), size=length(classlabels)))

#Separability analysis
JMdist(outdata[,2], X=outdata[,3:ncol(outdata)])

save(calibdata, file="calibdata.RData") # save the file as an R object
load("calibdata.RData") # loads the data to the environment
writeOGR(calibdata, dsn=getwd(), layer="calibdata", driver="ESRI Shapefile")
#writeOGR(calibdata, dsn="outputs.json", layer="calibdata", driver="GeoJSON")
