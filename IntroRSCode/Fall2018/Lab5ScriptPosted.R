path=("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class5/Lab5Data/")
setwd(path)

library(RStoolbox)
library(raster)
library(rgdal)

dir()

### IN IMAGE FOR CLASS EXCERCISE
L8Name="LC08_L1TP_128037_20180513_20180517_01_T1"
srtmName="n33_e107_1arc_v3.tif"

# Decompress and stack landsat image
untar(paste(L8Name, "tar", sep="."))

meta=readMeta(paste(paste(L8Name, "MTL", sep="_"), "txt", sep="."))
L8=stackMeta(paste(paste(L8Name, "MTL", sep="_"), "txt", sep="."))
L8
plot(L8[[5]])

# Open and reproject srtm image
srtm=raster(srtmName)      
srtm
plot(srtm)

srtmPrj=projectRaster(srtm, L8)
plot(srtmPrj)

#find approximate geographic corners of the effective extent by clicking in the top-left and lower-right
# corners of the area that contains valid information
# These are the ones that will be used to resize the in srtmPrj image to that extent
corners=click(srtmPrj, n=2, xy=TRUE)
e=extent(corners[1,1], corners[2,1], corners[2,2], corners[1,2])

srtmRsz=crop(srtmPrj,e)
plot(srtmRsz)

L8Rsz=crop(L8, e)
plot(L8Rsz[[5]])

# Remove objects to release memory
rm(L8,srtm, srtmPrj)

# Mask images to a common vaid domain
m=c(-Inf,0,NA, 0,Inf,1)
reclassm=matrix(m, ncol=3, byrow=TRUE)
L8msk=reclassify(L8Rsz, reclassm)
L8msk=max(L8msk)
srtmsk=reclassify(srtmRsz, reclassm)

mask=L8msk1*srtmsk
plot(mask)

L8mskd=mask(L8Rsz,mask)
srtmskd=mask(srtmRsz, mask)
rm(mask, L8msk)

plot(L8mskd[[5]])
plot(srtmskd)

rm(L8msk,srtmsk, L8Rsz, srtmRsz)

# Calculate terrain variables from the DEM
slope=terrain(srtmskd, opt='slope')
aspect=terrain(srtmskd, opt='aspect')
terrainvar=stack(slope,aspect)
names(terrainvar)=c('slope', 'aspect')
rm(slope,aspect)

# calculate illumination
shade=topCor(L8mskd, dem=terrainvar, 
             metaData=(paste(paste(imageName, "MTL", sep="_"), "txt", sep=".")),
             method="illu")

plot(shade)

#### Apply topographic correction
L8cor=topCor(L8mskd, dem=terrainvar, 
             metaData=(paste(paste(imageName, "MTL", sep="_"), "txt", sep=".")),
             method="C")

pdf("BeforeAndAfter.pdf")
plotRGB(L8mskd, r=5, g=4, b=3, axes=TRUE, stretch="lin")
plotRGB(L8cor, r=5, g=4, b=3, axes=TRUE, stretch="lin")
dev.off()