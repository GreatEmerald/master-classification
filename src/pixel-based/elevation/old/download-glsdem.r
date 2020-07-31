# Script to download GLSDEM and calculate terrestrial metrics

library(raster)
library(gdalUtils)
source("pixel-based/utils/load-sampling-data.r")

# GLSDEM files are 1x1 degree. Downloading it is already implemented for the tile processing chain.
# For pixels, we need to figure out which tiles we need.
# Tiles are in LYYYLXXX, where XXX and YYY are the bottom(!)-left coordinates.

OutputDEMDir = "../../userdata/master-classification/dem/glsdem/" # GLSDEM_n055e020.tif
OutputCSVDir = "../data/pixel-based/covariates/"

ReferenceData = LoadGlobalValidationData()

# Figure out the tiles we need
XIDs = floor(ReferenceData$x) # Floor due to this being the lower-left corner
YIDs = floor(ReferenceData$y)

Tiler = function(i)
{
    # Ignore everything that is not Africa right now
    if (XIDs[i] < -19 || XIDs[i] > 51 || YIDs[i] > 37 || YIDs[i] < -35)
        return(NA)
    
    XLetter = ifelse(XIDs[i] >= 0, "e", "w")
    YLetter = ifelse(YIDs[i] >= 0, "n", "s")
    return(sprintf("%s%03d%s%03d", YLetter, abs(YIDs[i]), XLetter, abs(XIDs[i])))
}

TileIDs = sapply(1:length(XIDs), Tiler)
UniqueTiles = na.omit(unique(TileIDs))

GLSDEMDownloader = function(TileID)
{
    DEMBaseFilename = paste0("GLSDEM_", TileID)
    TileID_Y = substr(TileID, 1, 4)
    url = paste0("ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/", TileID_Y, "/",DEMBaseFilename, "/", DEMBaseFilename, ".tif.gz")
    outfile = paste0(OutputDEMDir, "/", DEMBaseFilename, ".tif")
    outfilegz = paste0(outfile, ".gz")
    if(!file.exists(outfilegz) && !file.exists(outfile))
        try(download.file(url, outfilegz, "wget"))
    if(file.exists(outfilegz) && file.size(outfilegz) > 0)
        R.utils::gunzip(outfilegz, remove=TRUE)
    if(file.exists(outfile) && file.size(outfile) > 0)
        return(outfile)
    return(NA)
}

DownloadedTiles = sapply(UniqueTiles, GLSDEMDownloader)

# Write the list to a file (too long to fit on a command line)
write(DownloadedTiles, file.path(OutputDEMDir, "AfricaElevation.txt"), sep="\n")
# The DEM has no NA values, because there are simply no NAs (water is 0, after all).
# Some tiles are missing because they are sea; this implies that they should be set to 0.
gdalbuildvrt(input_file_list=file.path(OutputDEMDir, "AfricaElevation.txt"), output.vrt=file.path(OutputDEMDir, "AfricaElevation.vrt"), verbose=TRUE)
CreationOptions = c("COMPRESS=DEFLATE", "BIGTIFF=YES", "ZLEVEL=9", "NUM_THREADS=ALL_CPUS")
gdaldem("slope", file.path(OutputDEMDir, "AfricaElevation.vrt"), file.path(OutputDEMDir, "AfricaSlope.tif"), s=111120, co=CreationOptions)
# Aspect is set to NA if Slope = 0. We replace it with 0 down the line for RF.
gdaldem("aspect", file.path(OutputDEMDir, "AfricaElevation.vrt"), file.path(OutputDEMDir, "AfricaAspect.tif"), co=CreationOptions)
gdaldem("TPI", file.path(OutputDEMDir, "AfricaElevation.vrt"), file.path(OutputDEMDir, "AfricaTPI.tif"), co=CreationOptions)
gdaldem("TRI", file.path(OutputDEMDir, "AfricaElevation.vrt"), file.path(OutputDEMDir, "AfricaTRI.tif"), co=CreationOptions)
gdaldem("roughness", file.path(OutputDEMDir, "AfricaElevation.vrt"), file.path(OutputDEMDir, "AfricaRoughness.tif"), co=CreationOptions)


Elevation = raster(file.path(OutputDEMDir, "AfricaElevation.vrt"))
Slope = raster(file.path(OutputDEMDir, "AfricaSlope.tif"))
Aspect = raster(file.path(OutputDEMDir, "AfricaAspect.tif"))
TPI = raster(file.path(OutputDEMDir, "AfricaTPI.tif"))
TRI = raster(file.path(OutputDEMDir, "AfricaTRI.tif"))
Roughness = raster(file.path(OutputDEMDir, "AfricaRoughness.tif"))
TerrainIndices = stack(Elevation, Slope, Aspect, TPI, TRI, Roughness)
names(TerrainIndices) = c("elevation", "slope", "aspect", "tpi", "tri", "roughness")
TerrainMatrix = extract(TerrainIndices, ReferenceData)
rownames(TerrainMatrix) = ReferenceData$location_id
TerrainMatrix[,"aspect"][is.na(TerrainMatrix[,"aspect"])] = 0 # Replace NA aspect with 0, otherwise can't use RF
write.csv(TerrainMatrix, file.path(OutputCSVDir, "terrain-validation.csv"))
