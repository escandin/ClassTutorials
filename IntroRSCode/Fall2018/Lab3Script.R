setwd("/Users/Victor/Documents/Courses/IntroRemoteSensing/2018Fall/Class3/Test")#Lab3Materials")

library(raster)
#library(rasterVis)

# Unzip, open and stack data from the image files
unzip("MOD09Q1.A2017209.h12v04.006.2017218045157.zip")
mod=stack("MOD09Q1.A2017209.h12v04.006.2017218045157_B1.tif",
          "MOD09Q1.A2017209.h12v04.006.2017218045157_B2.tif")
mod

# Decompress, open and stack Landsat image
untar("LC08_L1TP_014032_20170612_20170628_01_T1.tar")
#untar("LC08_L1TP_014032_20170730_20170811_01_T1.tar")

L8=stackMeta("LC08_L1TP_014032_20170612_20170628_01_T1_B2.TIF", 
         "LC08_L1TP_014032_20170612_20170628_01_T1_B3.TIF",
         "LC08_L1TP_014032_20170612_20170628_01_T1_B4.TIF")
L8

unzip("m_4007431_ne_18_1_20170826.zip")

naip=stack("m_4007431_ne_18_1_20170826.tif") # Explain that a Tiff file can have one or several bands

naip

# Plot RGB maps from each map
plotRGB(mod, r = 1, g = 2, b = 2, axes = TRUE, stretch = "lin",
        main = "MODIS False Color Composite")

plotRGB(L8, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin",
        main = "Landsat true Color Composite" )

plotRGB(naip, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin",
        main = "naip true Color Composite" )

# Compare the coordinate systems of the three data layers
crs(mod)
crs(L8)
crs(naip)

# The projections are different so L8 and naip with be projected to the one for MODIS

# Explore the documentation from the projectRaster function to identify arguments
?projectRaster

L8prj=projectRaster(L8, crs=crs(mod))
rm(L8) # remove unused objects to release memory

naipprj=projectRaster(naip, crs=crs(mod))
rm(naipprj) # remove unused objects to release memory

# Plot MODIS and Landsat for an area covering the extent of the NAIP image.
# This is achieved through the use of the ext argument in plotRGB

plotRGB(mod, r = 1, g = 2, b = 2, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "MODIS False Color Composite")
plotRGB(L8prj, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "Landsat True Color Composite" )
plotRGB(naipprj, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "naip True Color Composite" )

# Save plots as pdfs in the working folder
pdf("RGBs.pdf")
plotRGB(mod, r = 1, g = 2, b = 2, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "MODIS False Color Composite")
plotRGB(L8prj, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "Landsat True Color Composite" )
plotRGB(naipprj, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", ext=extent(naipprj),
        main = "naip True Color Composite" )
dev.off()


##### THIS IS TO ADD NEXT YEAR
# Plot MODIS and overlay it with Landsat
plotRGB(mod, r = 1, g = 2, b = 2, axes = TRUE, stretch = "lin",
        main = "MODIS False Color Composite")

plotRGB(L8prj, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", add=TRUE,
        main = "Landsat true Color Composite")

plotRGB(naipprj, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", add=TRUE,
        main = "naip true Color Composite" )




# Explore spatial information
nlayers(mod) # shows the number of layers
crs(mod) # shows the reference system
xres(mod) # shows the spatial resolution in the x axis
yres(mod) # shows the spatial resolution in the y axis
res(mod) # shows the spatial resolution in the x,y axis
ncell(mod) # shows the number of pixels in the image
dim(mod) # rows, columns, bands



#plotRGB(L8, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin",
#        main = "Landsat False Color Composite" )





# They differ in their coordinate systems. The analysis requires to 
# reproject other images to the coordinate system.
# What system to select? what are the pros and cons of the different coordinate systems?
# Explain why I recommend MODIS

crsmod=crs(mod)
crsl8=crs(L8)
crsnaip=crs(naip)

#system.time(projectRaster(mod, crs=crsnaip))
#system.time(projectRaster(L8, crs=crsmod))
#system.time(projectRaster(L8, crs=crsnaip))
#system.time(projectRaster(naip, crs=crsmod))



# Verify that all three files have the same projection
crs(mod)
crs(L8prj)
crs(naipprj)



# First check the extent of the NAIP image
extent(naipprj)


# Plot each dataset for the extent covered by the NAIP image.
# Answer the questions in point 3 of the answer sheet
# based on your best interpretation of each dataset






# Project everything using Landsat as the reference
modprj=projectRaster(naip, crs=crsl8)
naipprj=projectRaster(naip, crs=crsl8)




# THIS IS FOR THE HOMEWORK
# Decompress, open and stack NAIP image
unzip("m_4007564_se_18_1_20170824.zip")
naip=stack("m_4007564_se_18_1_20170824.tif")
plotRGB(naip, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", #add=TRUE, 
        main = "naip False Color Composite" )

