# Convert the output GeoTIFF to KMZ

library(raster)

InputFile = "../data/pixel-based/predictions/randomforest-median-threestep-walltowall.tif"
OutputFile = "../output/global-lc-fraction-map.kmz"

TIFFFile = brick(InputFile)
TIFFFile = TIFFFile[[-1]]
KML(TIFFFile, OutputFile, maxpixels = ncell(TIFFFile), blur=1, col=gray.colors(101, 0, 1), zlim=c(0, 100), overwrite=TRUE)
