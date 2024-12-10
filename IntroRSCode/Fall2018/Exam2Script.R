# set working directory
setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Exam2/Exam2DataInputs")

# load files
lc=raster("luprjmskdrscld")
tascap=stack("Tascap")

# plot rasters
plot(lc)
plotRGB(tascap, r=1, g=2, b=3, stretch="lin")

# Define an extent and plot the same extent in the tasseled cap and the classified map
e=drawExtent()
par(mfrow=c(2,1))
plotRGB(tascap, r=1, g=2, b=3, stretch="lin", ext=e)
plotRGB(tascap, r=1, g=2, b=3, stretch="lin", ext=e)
plot(lc, ext=e, axes=FALSE, legend=FALSE, 
     box=FALSE, add=T, alpha=1) #

# retrieve pixel values for the land covers
click(lc, n=10, id=T, xy=T) # n indicates the number of times you can click in the map

