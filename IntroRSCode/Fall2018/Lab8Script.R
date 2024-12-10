setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials/")
unzip("Lab11MachineLearning.zip")

library(raster)
library(RStoolbox)
library(rgdal)

dir()

L8name="LC08_L1TP_006066_20160804_20170322_01_T1"

untar(paste(L8name, "tar", sep="."))
meta=readMeta(paste(L8name, "MTL.txt", sep="_"))
L8=stackMeta(meta)
plotRGB(L8, r=5, g=4, b=3, stretch="lin")

e=drawExtent()
L8rsz=crop(L8,e)
plotRGB(L8rsz, r=5, g=4, b=3, stretch="lin")

# mask input image
#ranges=matrix(c(-Inf,1,NA, 1,99999,1))
#msk=reclassify(inimagersz,ranges)
# msk=msk[[1]]*msk[[2]]*msk[[3]]*msk[[4]]*msk[[5]]*msk[[6]]
# inimagersz=inimagersz*msk
plotRGB(inimagersz, r=4, g=3, b=2, axes=TRUE, stretch="lin")

#### UNSUPERVISED IMAGE CLASSIFICATION
values = getValues(ndvi) # it extracts NDVI values in a numeric vector.
head(values) # shows the first 6 pixels values in the vector.
class.km = kmeans(na.omit(values), centers = 10, iter.max = 500, nstart = 3, algorithm="Lloyd") 
classified = ndvi
classified[] = class.km$cluster
plot(classified, main = 'Unsupervised classification of Landsat data')

# Reclassify the image by grouping original classes in a single category
# in my case I will group 2 and 5; 2, 3 and 10; 3 and 6; 4 and 8; and 5 and 7
reclass=c(1,2,1, 2,3,2, 3,4,3, 4,5,4, 5,6,5, 6,7,3, 7,8,5, 8,9,4, 9,10,3, 10,11,2) # This is the vector with the from, to, equal values
reclassed=reclassify(classified,reclass)
plot(reclassed)


#### SUPERVISED CLASSIFICATION
# import and explore input polygons

calibdata=readOGR(".", "outputs", 
                  p4s= "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
validdata=readOGR(".", "ValidPolyg", 
                  p4s= "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
names(calibdata)
calibdata$class
unique(calibdata$class)

# convert polygons into raster
calibrast=rasterize(calibdata, L8rsz, field=calibdata$class)
validrast=rasterize(validdata, inimagersz, field=validdata$CLASS_ID)
rm(calibdata,validdata,e,ranges,inimage,msk)

ranges=matrix(c(-Inf,1,NA, 1,99999,1))
calibmsk=reclassify(calibrast,ranges)
validmsk=reclassify(validrast,ranges)

extractval=function(inimage, msk){
  outvector=msk*inimage
  outvector=na.omit(getValues(outvector))
  return(outvector)
}

classtype=extractval(calibrast, calibmsk)
b1=extractval(inimagersz[[1]], calibmsk)
b2=extractval(inimagersz[[2]], calibmsk)
b3=extractval(inimagersz[[3]], calibmsk)
b4=extractval(inimagersz[[4]], calibmsk)
b5=extractval(inimagersz[[5]], calibmsk)
b6=extractval(inimagersz[[6]], calibmsk)
calibdf=data.frame(cbind(classtype,b1,b2,b3,b4,b5,b6))

classtype=extractval(validrast, validmsk)
b1=extractval(inimagersz[[1]], validmsk)
b2=extractval(inimagersz[[2]], validmsk)
b3=extractval(inimagersz[[3]], validmsk)
b4=extractval(inimagersz[[4]], validmsk)
b5=extractval(inimagersz[[5]], validmsk)
b6=extractval(inimagersz[[6]], validmsk)
validdf=data.frame(cbind(classtype,b1,b2,b3,b4,b5,b6))

########### classification
formula=as.factor(classtype)~b1+b2+b3+b4+b5+b6

################# decision tree
library(rpart)
treeclass=rpart(formula, data=calibdf, na.action=na.rpart,
                method='class')
library(rpart.plot)
rpart.plot(treeclass, type=3, cex=0.5)

################# random forest
library(randomForest)
rfor=randomForest(formula, data=calibdf, ntree=100, 
                  mtry=3, na.action=na.omit)
varImpPlot(rfor, sort=TRUE)


##################### validation
rforpred=predict(rfor, validdf, type="class")

library(caret)
conmatrix=confusionMatrix(validdf$classtype, rforpred)

################ classify image
names(inimagersz)=c("b1", "b2","b3","b4","b5","b6")
predrastfor=predict(inimagersz, model=rfor, type="class", na.rm=TRUE)

