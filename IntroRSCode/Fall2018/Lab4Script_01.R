############################################################################################
## This code decompresses, plots and resizes a selected Landsat image.

# It then identifies the darkest pixels and uses that information along with the

# image metadata in order to apply different methods of atmospheric correction.
############################################################################################
# set path and wd
path="/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class4"
setwd(file.path(path, "Test"))
getwd()

# Open libraries
library(raster)
library(data.table)
library(RStoolbox)

#Decompress image
dir()
untar("LC08_L1TP_006066_20160804_20170322_01_T1.tar")

meta=readMeta("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class4/Test/LC08_L1TP_006066_20160804_20170322_01_T1_MTL.txt")
L8=stackMeta("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class4/Test/LC08_L1TP_006066_20160804_20170322_01_T1_MTL.txt")
L8

#Plot the image in false composite
plotRGB(L8, r=5, g=4, b=3, axes=TRUE, stretch="lin")

# Resize the image
# First, select the desired extent
click(L8, n=2, xy=TRUE)
ext=extent(520000, 610000, -1017000, -895000)
L8rsz=crop(L8, ext)

# Plot resized image
plotRGB(L8rsz, r=5, g=4, b=3, axes=TRUE, stretch="lin")
click(L8, n=10, cell=TRUE)

# Estimate the value of the darkest pixels in haze bands
haze=estimateHaze(L8rsz, hazeBand=1:5, 
                  darkProp=0.001, plot=TRUE)

# Apply different types of atmospheric correction. Check help by typing ?radCor
L8rad=radCor(L8rsz, metaData=meta, 
             method="rad", hazeValues=haze, hazeBands=1:5, 
             verbose=TRUE)

L8ToA=radCor(L8rsz, metaData=meta, 
             method="apref", hazeValues=haze, hazeBands=1:5, 
             verbose=TRUE)

L8dos=radCor(L8rsz, metaData=meta, 
             method="sdos", hazeValues=haze, hazeBands=1:5, 
             verbose=TRUE)



