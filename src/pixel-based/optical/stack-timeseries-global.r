# stack-timeseries.r (extracting time series from Proba-V imagery) optimised for the globe.
# First, mosaic all tiles into a VRT. This makes sure we always have the same number of layers in the time series.
# Then, stack all mosaics into VRTs per VI.
# Lastly, do a mega extract.

library(gdalUtils)
library(raster)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/ProbaVDataDirs.r")

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
VRTDir = "../../userdata/master-classification/pixel-based/vrt/raster/"
OutputDir = "../data/pixel-based/raster"
VIs = c("SM", "RADIOMETRY", "NDVI")

SamplePoints = LoadGlobalRasterPoints()
TileList = levels(SamplePoints$Tile) # Tiles to process

DataDirCache = paste0(OutputDir, "DataDirs.csv")
if (!file.exists(DataDirCache))
{
    DataDirs = ProbaVDataDirs()
    DataDirs = data.frame(DataDirs, date=basename(DataDirs))
    write.csv(DataDirs, DataDirCache, row.names = FALSE)
} else DataDirs = read.csv(DataDirCache, stringsAsFactors = FALSE)

# First, mosaic all tiles into a VRT. This makes sure we always have the same number of layers in the time series.
for (dir in DataDirs$DataDirs)
{
    for (VI in VIs)
    {
        VIVRTDir = file.path(VRTDir, VI)
        OutFile = file.path(VIVRTDir, paste0(basename(dir), ".vrt"))
        if (!file.exists(OutFile))
        {
            if (!dir.exists(VIVRTDir))
                dir.create(VIVRTDir, recursive = TRUE)
            
            GlobalVI = list.files(dir, glob2rx(paste0("PROBAV_S5_TOC_*", VI, ".tif")), recursive = TRUE, full.names = TRUE)
            gdalbuildvrt(GlobalVI, OutFile)
        }
    }
}

# Then, stack all mosaics into VRTs per VI.
for (VI in VIs)
{
    VIVRTDir = file.path(VRTDir, VI)
    VIFiles = list.files(VIVRTDir, glob2rx("*.vrt"), full.names = TRUE)
    if (VI == "RADIOMETRY") # 4 bands, so can't stack this
    {
        for (Band in 1:4)
        {
            VRTFile = paste0(VRTDir, "/", VI, "-", Band, ".vrt")
            if (!file.exists(VRTFile))
            {
                gdalbuildvrt(VIFiles, VRTFile, separate = TRUE, b = Band)
                if (Band != 1)
                    system(paste0('sed -i "s|<SourceBand>1</SourceBand>|<SourceBand>', Band ,'</SourceBand>|" ', VRTFile))
            }
        }
    } else {
        VRTFile = paste0(VRTDir, "/", VI, ".vrt")
        if (!file.exists(VRTFile))
            gdalbuildvrt(VIFiles, VRTFile, separate = TRUE)
    }
}

## Done offscreen: sed the VRT files to make sure the block sizes align for faster extract.
# So that's 256x256 for Radiometry and 10080x256 for NDVI and SM.
# sed -i 's/BlockXSize="128" BlockYSize="128"/BlockXSize="256" BlockYSize="256"/g' RADIOMETRY-?.vrt
# sed -i 's/BlockXSize="128" BlockYSize="128"/BlockXSize="10080" BlockYSize="256"/g' SM.vrt NDVI.vrt

# Lastly, do a mega extract.
VI="RADIOMETRY-1"
VIRaster = brick(file.path(VRTDir, paste0(VI, ".vrt")))
# Extracting all at once eats all memory and dies. So let's use our knowledge about tiles and extract everything tile by tile.
library(foreach)
library(doParallel)
library(iterators)
registerDoParallel(cores=2)
# Make the dir
if (!dir.exists(OutputDir))
    dir.create(OutputDir)
MyMegaExtract = foreach(Tile = iter(levels(SamplePoints$Tile)), .packages="raster", .inorder=FALSE, .combine=rbind, .verbose=TRUE) %dopar%
{
    OutFile = paste0(OutputDir, "/", VI, "-", Tile, ".csv")
    if (!file.exists(OutFile))
    {
        TileExtract = extract(VIRaster, SamplePoints[SamplePoints$Tile == Tile,])
        colnames(TileExtract) = paste0("X", as.character(DataDirs$date)) # Add date to names
        TileExtract = cbind(as.data.frame(SamplePoints[SamplePoints$Tile == Tile,])[c("x", "y")], TileExtract) # add x/y
        write.csv(TileExtract, OutFile, row.names=FALSE)
    } else TileExtract = read.csv(OutFile)
    print(Sys.time())
    TileExtract
}
#NDVIs = extract(VIRaster, SamplePoints[1:2,])
#colnames(NDVIs) = as.character(DataDirs$date)
#write.csv(cbind(as.data.frame(SamplePoints[1:2,])[c("x", "y")], NDVIs), paste0(OutputDir, "/NDVI-test.csv"), row.names=FALSE)
#read.csv(paste0(OutputDir, "/NDVI-test.csv"))
#plot(as.POSIXct(DataDirs$date, format="%Y%m%d"), t(NDVIs)[,2], type="l")
