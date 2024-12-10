setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials/")

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
rm(L8)

# Convert pixel values into ToA reflectance

L8ToA=radCor(L8rsz, metaData=meta, method="apref")

L8subset=L8ToA[[2:7]]
rm(L8ToA)
rm(L8rsz)
L8PCA=rasterPCA(L8subset, nComp=nlayers(L8subset))
plotRGB(L8PCA$map, r=1, g=2, b=3, stretch="lin")

L8indices=spectralIndices(L8subset, blue=1, green=2,
                          red=3, nir=4, swir2=5, swir3=6,
                          indices=c("NDVI", "NDWI", "NBRI"))

L8tascap=tasseledCap(L8subset, sat="Landsat8OLI")
plotRGB(L8tascap, r=1, g=2, b=3, stretch="lin")

L8allbands=stack(L8subset, L8indices, L8tascap)
rm(L8subset, L8indices, L8tascap, L8PCA)

e=drawExtent()
plotRGB(L8allbands, r=10, g=11, b=12, stretch="lin", ext=e)

urban=click(L8allbands, n=1, cell=TRUE)
forest=click(L8allbands, n=1, cell=TRUE)
lake=click(L8allbands, n=1, cell=TRUE)
river=click(L8allbands, n=1, cell=TRUE)
grass=click(L8allbands, n=1, cell=TRUE)
burnt=click(L8allbands, n=1, cell=TRUE)
sand=click(L8allbands, n=1, cell=TRUE)
oilpalm=click(L8allbands, n=1, cell=TRUE)

landcovers=data.frame(rbind(urban, forest, lake, river,
                            grass, burnt, sand, oilpalm))

rm(urban, forest, lake, river, grass, burnt, sand, oilpalm)
landcovers=as.data.frame(t(as.matrix(landcovers)))
landcovers=landcovers[2:nrow(landcovers),]

names(landcovers)=c('urban', 'forest', 'lake', 'river',
                    'grass', 'burnt', 'sand', 'oilpalm')

colors=c('red', 'green','blue', 'gray', 'yellow', 'black', 'purple', 'orange')

pdf("LCplot.pdf")
plot(landcovers[,1], type='l', col=colors[1], 
     ylab="pixel value", xlab="band", ylim=c(-0.6, 1.4), xaxt=
       "n")
lines(landcovers[,2], type='l', col=colors[2])
lines(landcovers[,3], type='l', col=colors[3])
lines(landcovers[,4], type='l', col=colors[4])
lines(landcovers[,5], type='l', col=colors[5])
lines(landcovers[,6], type='l', col=colors[6])
lines(landcovers[,7], type='l', col=colors[7])
lines(landcovers[,8], type='l', col=colors[8])
legend(1,1.4, legend=names(landcovers), col=colors, lty=1, cex=0.5)
axis(side=1, labels=c("B2", "B3", "B4", "B5", "B6", "B7", "NDVI",
                     "NDWI", "NBRI", "bright", "green", "wet"), 
     at=seq(1:nrow(landcovers)))
dev.off()
L8subset

