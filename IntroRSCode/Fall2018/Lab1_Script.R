# Install and open libraries needed
install.packages("rasterVis")
library(rasterVis)
library(raster)
library(rgdal)
library(plot3D)

# How to create a raster file
cone <- function(x, y){
  sqrt(x^2+y^2)
}
x <- y <- seq(-1, 1, length= 100)
z <- outer(x, y, cone)
datarast=raster(z)
plot(datarast)

# Set working directory
setwd("/Users/Victor/Gitrepo/IntroRSCode")

# Import a raster file (in this case a DEM file in esri grid format)
# Download DEM data from: http://www.soest.hawaii.edu/coasts/data/
downloadURL=("ftp://soest.hawaii.edu/coastal/webftp/Kauai/dem/Kauai_DEM.zip")
filename="Kauai_DEM.zip"
download.file(downloadURL, filename)

# Unzip file
unzip(filename)

# Open the file in the R environment
KauaiDEM=raster("kaui_dem")
plot(KauaiDEM)

#Perspective plot
persp(KauaiDEM)
persp(r, theta=60, phi=0) 
# e.g. iteratate combinations of theta=0-60 and phi=0-60
plot3D(KauaiDEM, zfac=1)

# Import, subset and visualize vectors
# Import files from here: https://gdr.openei.org/submissions/540
downloadURL=("https://gdr.openei.org/files/540/Island_boundaries.xml")
filename="Island_boundaries.xml"
download.file(downloadURL, filename)

# read some attributes of the vector file
ogrListLayers(".")
ogrInfo(".", layer="Island_boundaries")

# import file to R
boundaries=readOGR(".", "Island_boundaries")

# read some information in the file
plot(boundaries, border="green")
class(boundaries)
crs(boundaries)
summary(boundaries)

#select boundaries for Kauai only
Kauai=which(boundaries$Island == 'Kauai')
boundKauai=boundaries[Kauai,]
plot(boundKauai, border="green")

# Import streets from: http://planning.hawaii.gov/gis/download-gis-data/
downloadURL=("http://files.hawaii.gov/dbedt/op/gis/data/kaustreets.shp.zip")
filename="kaustreets.shp.zip"
download.file(downloadURL, filename)
unzip(filename)
KauaiStreets=readOGR(".", "kau_centerlines")

# Import reserves from: http://planning.hawaii.gov/gis/download-gis-data/
downloadURL=("http://files.hawaii.gov/dbedt/op/gis/data/reserves.shp.zip")
filename="reserves.shp.zip"
download.file(downloadURL, filename)
unzip(filename)

reserves=readOGR(".", "reserves")
summary(reserves)
plot(reserves)
reservesKauai=reserves[which(reserves$Island=="Kauai"),]
plot(reservesKauai)

# plot raster and vector files
plot(KauaiDEM)
plot(boundKauai, add=T)
plot(reservesKauai, col='lightgreen', border = 'transparent',add=T)
plot(KauaiStreets, col='gray', add=T)

# save plots as PDF. It is possible to use svg or png: http://www.cookbook-r.com/Graphs/Output_to_a_file/
pdf("plots.pdf")
  plot(KauaiDEM)
  plot(boundKauai, add=T)
  plot(reservesKauai, col='lightgreen', border = 'transparent',add=T)
  plot(KauaiStreets, col='gray', add=T)
dev.off()

#####    IF WE HAVE TIME    ###############################
# Import a raster stack in GeoTiff format: http://www.soest.hawaii.edu/coasts/data/kauai/landsat.html
downloadURL=("ftp://ftp.soest.hawaii.edu/coastal/webftp/Kauai/landsat/geotiff.zip")
filename="geotiff.zip"
download.file(downloadURL, filename)
unzip(filename)

LandsatKauai=stack("kauai_niihau_lehua2.tif")
# retrieve information from the raster
nlayers(LandKauai) # shows the number of layers
crs(LandsatKauai) # shows the reference system
xres(LandsatKauai) # shows the spatial resolution in the x axis
yres(LandsatKauai) # shows the spatial resolution in the y axis
res(LandsatKauai) # shows the spatial resolution in the x,y axis
ncell(LandsatKauai) # shows the number of pixels in the image
dim(LandsatKauai) # rows, columns, bands
plot(LandsatKauai[[3]]) # plots band 5 (NIR)

# The file includes an area larger than the Island. 
# Let's resize the raster to the area of interest
extent(LandsatKauai)
e=extent(415000, 472000, 2415000, 2470000)
LandKauai=crop(LandsatKauai, e)

hist(LandKauai)

# Plot the raster as an RGB 
plotRGB(LandKauai, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", 
        main = "Landsat False Color Composite")

plotRGB(LandKauai, r = 3, g = 2, b = 1, axes = TRUE, stretch = "hist", 
        main = "Landsat False Color Composite")
# Show histograms and alternatives to specify the stretch settings


############################################################################################
# Homework: repeat the steps performed for the island of Kauai but applied to the island of Oahu
# Deliverables: Please submit the three figures specified below saved as pdf using the pdf function in R
# 1. A perspective plot of the topography of the island of Oahu
# 2. A digital elevation map of the island of Ouahu that includes the outline of the island in black color,
# the protected areas in light green and the roads in gray.
# 3. A False color composite using bands 3, 2 and 1 as RGB colors with a linear stretch
# 4. Submit a .R file with the script you used to derive the figures.

############################################################################################
############################################################################################

# This is the homework
# Import a raster file (in this case a DEM file in esri grid format)
# Download DEM data from: http://www.soest.hawaii.edu/coasts/data/
downloadURL=("ftp://soest.hawaii.edu/coastal/webftp/Oahu/dem/Oahu_DEM.zip")
filename="Oahu_DEM.zip"
download.file(downloadURL, filename)
# Unzip file
unzip(filename)

# Open the file in the R environment
OahuDEM=raster("Oahu_dem")
plot(OahuDEM)

#Perspective plot
persp(OahuDEM)
persp(OahuDEM, theta=30, phi=0) 
# e.g. iteratate combinations of theta=0-60 and phi=0-60
plot3D(OahuDEM, zfac=1)

# Import, subset and visualize vectors
# Import files from here: https://gdr.openei.org/submissions/540
downloadURL=("https://gdr.openei.org/files/540/Island_boundaries.xml")
filename="Island_boundaries.xml"
download.file(downloadURL, filename)

# read some attributes of the vector file
ogrListLayers(".")
ogrInfo(".", layer="Island_boundaries")

# import file to R
boundaries=readOGR(".", "Island_boundaries")

# read some information in the file
plot(boundaries, border="green")
class(boundaries)
crs(boundaries)
summary(boundaries)

#select boundaries for Kauai only
Oahu=which(boundaries$Island == 'Oahu')
boundOahu=boundaries[Oahu,]
plot(boundOahu, border="green")

# Import streets from: http://planning.hawaii.gov/gis/download-gis-data/
downloadURL=("http://files.hawaii.gov/dbedt/op/gis/data/oah_streets.shp.zip")
filename="oah_streets.shp.zip"
download.file(downloadURL, filename)
unzip(filename)
OahuStreets=readOGR(".", "oah_streets")

# Import reserves from: http://planning.hawaii.gov/gis/download-gis-data/
downloadURL=("http://files.hawaii.gov/dbedt/op/gis/data/reserves.shp.zip")
filename="reserves.shp.zip"
download.file(downloadURL, filename)
unzip(filename)

reserves=readOGR(".", "reserves")
summary(reserves)
plot(reserves)
reservesOahu=reserves[which(reserves$Island=="Oahu"),]
plot(reservesOahu)

# plot raster and vector files
plot(OahuDEM)
plot(boundOahu, add=T)
plot(reservesOahu, col='lightgreen', border = 'transparent',add=T)
plot(OahuStreets, col='gray', add=T)

# save plots as PDF. It is possible to use svg or png: http://www.cookbook-r.com/Graphs/Output_to_a_file/
pdf("plotsOahu.pdf")
  plot(OahuDEM)
  plot(boundOahu, add=T)
  plot(reservesOahu, col='lightgreen', border = 'transparent',add=T)
  plot(OahuStreets, col='gray', add=T)
dev.off()

# Import a raster stack in GeoTiff format: http://www.soest.hawaii.edu/coasts/data/oahu/landsat.html
downloadURL=("ftp://soest.hawaii.edu/coastal/webftp/Oahu/lansat/geotiff.zip")
filename="geotiff.zip"
download.file(downloadURL, filename)
unzip(filename)

LandsatOahu=stack("oahu.tif")
# retrieve information from the raster

plot(LandsatOahu[[3]]) # plots band 5 (NIR)

hist(LandsatOahu)

# Plot the raster as an RGB 
plotRGB(LandsatOahu, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", 
        main = "Landsat False Color Composite")

plotRGB(LandsatOahu, r = 3, g = 2, b = 1, axes = TRUE, stretch = "hist", 
        main = "Landsat False Color Composite")
# Show histograms and alternatives to specify the stretch settings