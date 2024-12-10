library(raster)
library(RStoolbox)
library(prettymapr)
library(rgdal)
library(stringr)
library(randomcoloR)
library(lulcc)

setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class6/Lab6Materials")

# Open inimage and calibdata
inimage=stack("inimage.tif")
load("calibdata.RData")

# Peform unsupervised and supervised classification
unsupervised=unsuperClass(inimage, nClasses=15)

# trainPartition splits calibdata into two random groups,
# one for calibration and the other for validation
# The number defines the proportion that will be used for calibration
supervised=superClass(inimage, calibdata, trainPartition=.8,
                      responseCol = "class" )

# retrieve the accuracy matrix
accumatrix=supervised$validation$performance$table
accumatrix
# saves accuracy matrix
save(accumatrix, file="accumatrix.RData")

# retrieves producer's and user's accuracy 
accuresults=data.frame(supervised$validation$performance$byClass)
users=accuresults$Pos.Pred.Val
producers=accuresults$Recall
overall=supervised$validation$performance$overall[1]
classnames=attributes(supervised$validation$performance$byClass)$dimnames[[1]]
classnames=str_remove(classnames, "Class: ")

###################################################
############### PLOT MAPS AND FIGURES #############

# PLOT PRODUCER'S AND USER'S ACCURACY
pdf("accuracy.pdf", paper="USr", width=15)
barplot(rbind(users,producers),col=c("lightgreen","lightyellow"), 
        names.arg=classnames,  beside = TRUE, ylab= "accuracy (%)")
dev.off()

# PLOT INPUT IMAGE
pdf("inimage.pdf")
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")
dev.off()

# PLOT SUPERVISED CLASSIFICATION
# Retrieve the names of the different categories and 
# the pixel values assigned to each one of them
catnames=slot(supervised$map@data, name="attributes")[[1]] 
catnames
# This defines the colors to assign to each catname
# The colors are assigned to each class are in the same order as they 
# appear in catnames. Make sure the colors are intuitive
mapcol=c("pink", "green", "lightblue", "yellow", "blue", "gray", "red")

pdf('superclass.pdf')
plot(supervised$map, col=mapcol, axes=FALSE, legend=FALSE, box=FALSE)

# use catnames$category below if you are using a PC
legend("topright", legend = catnames$value[2:length(catnames$value)],
       fill = mapcol, cex=0.7, bg="white")

# Add north arrow, scalebar.
prettymapr::addnortharrow(pos="bottomright", scale = 0.6, padin=c(0.5,0.1),
                          text.col = 'black', cols = c('black', 'black'))
prettymapr::addscalebar(pos="bottomleft", plotunit = 'm', widthhint = 0.25, lwd = 1, 
                        padin = c(0.5, 0.1), label.cex = 0.9)
dev.off()

# To change the name of the classes in the raster (Just if needed)
levels(supervised$map)[[1]][,2]=c("sand", "forest", "lake", "pasture", 
                                  "river", "second", "urban")

# PLOT UNSUPERVISED CLASSIFICATION
unsupercol=distinctColorPalette(k = 15) # These are the colors to be used for mapping the unsupervised map
pdf('unsuperclass.pdf')
plot(unsupervised$map, col=unsupercol, axes=FALSE, legend=T, box=FALSE)
# Add north arrow, scalebar.
prettymapr::addnortharrow(pos="bottomright", scale = 0.6, padin=c(0.5,0.1),
                          text.col = 'black', cols = c('black', 'black'))
prettymapr::addscalebar(pos="bottomleft", plotunit = 'm', widthhint = 0.25, lwd = 1, 
                        padin = c(0.5, 0.1), label.cex = 0.9)
dev.off()

# To save data
save(calibdata, file="calibdata.RData") # save the polygons as an R object
save(supervised, file="supervised.RData")
save(unsupervised, file="unsupervised.RData")

# To export spatial files in a format compatible with other applications
writeOGR(calibdata, dsn=getwd(), layer="calibdata", # save polygons as shape file
         driver="ESRI Shapefile")
writeRaster(supervised$map, filename="supervised.tif", 
            filetype="GTiff")
writeRaster(unsupervised$map, filename="unsupervised.tif", 
            filetype="GTiff")
writeRaster(inimage, filename="inimage.tif", 
            filetype="GTiff")

#################### TO RELOAD FILES ##############################
# To load data into the environment
load("calibdata.RData")
load("supervised.RData")
load("unsupervised.RData")
inimage=stack("inimage.tif")

################################################################################
############  BONUS ############################################################
# Reclassify unsupervised classification map with land cover types and values
# equal to the ones assigned for the supervised classification

# Select an extent in the input image. 
# Then zoom into it to interpret the names of each unsupervised class by
# comparing the input RGB image with the classes assigned to different pixels

par(mfrow=c(1,1))
plotRGB(inimage, r=5, g=4, b=3, stretch="lin")
e=drawExtent()
par(mfrow=c(2,1))
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)
plotRGB(inimage, r=5, g=4, b=3, stretch="lin", ext=e)
plot(unsupervised$map, ext=e, axes=FALSE, col=unsupercol, legend=FALSE, 
     box=FALSE, add=T, alpha=1) #
click(unsupervised$map, n=5, id=T) # n indicates the number of times you can click in the map

# Below you should create groups of three values. The third value is the class number that will be assigned  
# in the reclassified map to all pixels that are classified with a value laying with the range
# defined by the first two value.
# In the example below, all pixels that are classified as 7 and 8 in the original map, 
# will be classified as 2 in the new map.

m=c(0.5,1.5,7, 1.5,3.5,2, 3.5,4.5,4, 
    4.5,5.5,2, 5.5,6.5,6, 6.5,8.5,2,
    8.5,9.5,6, 9.5,10.5,4, 10.5,11.5,3,
    11.5,12.5,5, 
    12.5,13.5,4, 13.5,14.5,4, 14.5,15.5,1)
reclassmat=matrix(m, ncol=3, byrow=TRUE)

# This reclassifies the unsupervised map
unsupreclass=reclassify(unsupervised$map, reclassmat)

# Plot the reclassified unsupervised classification map
unsupreclass
catnames # check the names for the categories in the supervised classification
classcol=sort(unique(values(unsupreclass))) #check the classes in the unsupervised map

pdf('unsup_reclass.pdf')
plot(unsupreclass, col=mapcol, axes=FALSE, legend=FALSE, box=FALSE)

# use catnames$category if you are using a PC
legend("topright", legend = catnames$value[2:length(catnames$value)],
       fill = mapcol, cex=0.7, bg="white")
prettymapr::addnortharrow(pos="bottomright", scale = 0.6, padin=c(0.5,0.1),
                          text.col = 'black', cols = c('black', 'black'))
prettymapr::addscalebar(pos="bottomleft", plotunit = 'm', widthhint = 0.25, lwd = 1, 
                        padin = c(0.5, 0.1), label.cex = 0.9)
dev.off()

#################### SAVE RECLASSIFIED MAP ##############################
save(unsupreclass, file="unsupereclass.RData")
writeRaster(unsupereclass$map, filename="unsupreclass.tif", 
            filetype="GTiff").
