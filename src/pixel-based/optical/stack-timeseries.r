# Generates VRTs for all the Proba-V imagery to use as a stack.
library(gdalUtils)
library(raster)
source("pixel-based/utils/getProbaVInfo.r")
source("pixel-based/utils/load-sampling-data.r")
source("utils/GetProbaVQCMask.r")
source("pixel-based/utils/ProbaVDataDirs.r")

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
OutputDir = "../../userdata/master-classification/pixel-based/vrt/sr/"

TileList = expand.grid(sprintf("X%02d", 15:23), sprintf("Y%02d", 3:10), stringsAsFactors = FALSE)
TileList = paste0(TileList[,1], TileList[,2])
# There's just sea here
ExcludeTiles = expand.grid(sprintf("X%02d", 15:18), sprintf("Y%02d", 8:10), stringsAsFactors = FALSE)
ExcludeTiles = paste0(ExcludeTiles[,1], ExcludeTiles[,2])
ExcludeTiles = c(ExcludeTiles, "X15Y07", "X16Y07", "X23Y07", "X23Y10")
any(as.character(SamplePoints$Tile) %in% ExcludeTiles) # Make sure we don't discard something we need
TileList = TileList[!TileList %in% ExcludeTiles]

Tile = "X16Y06" # Test tile

# Generate a list of directories to read from.
# This list is semi-static: we don't read in more files when they arrive. Else the lengths would differ.
DataDirFile = "../data/DataDirs.csv"
if (!file.exists(DataDirFile))
{
    DataDirs = ProbaVValidDirs(RequiredTiles = TileList)#levels(SamplePoints$Tile))
    DataDirsDates = data.frame(dir=DataDirs, date=as.Date(basename(dirname(DataDirs)), format="%Y%m%d"))
    write.csv(DataDirsDates, DataDirFile, row.names=FALSE)
} else {
    DataDirsDates = read.csv(DataDirFile, stringsAsFactors=FALSE)
    DataDirs = DataDirsDates$dir
}

BuildProbaVTileVRT = function(DataDirs, VI, Tile, Band=1)
{
    TileFiles = list.files(DataDirs, pattern=glob2rx(paste0("PROBAV_S5_TOC_", Tile, "*", VI, ".tif")), full.names=TRUE)
    Dates = getProbaVinfo(TileFiles)$date
    VRTFile = file.path(OutputDir, paste0(Tile, "-", VI, "-", Band, ".vrt"))
    if (!file.exists(VRTFile))
    {
        gdalbuildvrt(TileFiles, VRTFile, separate=TRUE, b=Band)
        # Workaround for -b not working again
        if (Band != 1)
            system(paste0('sed -i "s|<SourceBand>1</SourceBand>|<SourceBand>', Band ,'</SourceBand>|" ', VRTFile))
    }
    Result = brick(VRTFile)
    Result = setZ(Result, Dates)
    return(Result)
}

SamplePoints = LoadGlobalTrainingData()
PointsInTile = SamplePoints[SamplePoints$Tile == Tile,]

# Extract QC values
QCStack = BuildProbaVTileVRT(DataDirs, "SM", Tile)
any(duplicated(getZ(QCStack)))
system.time(QCInTile <- extract(QCStack, PointsInTile))

# Extract NDVI values
TileStack = BuildProbaVTileVRT(DataDirs, "NDVI", Tile)
all(getZ(QCStack) == getZ(TileStack))

# Takes about 8 minutes per tile
system.time(NDVIInTile <- extract(TileStack, PointsInTile))
colnames(NDVIInTile) = paste0("NDVI-", c(getZ(TileStack)))

# Mask with QC
FilterQC = GetProbaVQCMask(bluegood=TRUE, redgood=TRUE, nirgood=TRUE, swirgood=TRUE,
                           ice=FALSE, cloud=FALSE, shadow=FALSE)
NDVIInTile[!QCInTile %in% FilterQC] = NA

visdat::vis_miss(as.data.frame(NDVIInTile))
write.csv(NDVIInTile, file.path("..", "data", "pixel-based", paste0(Tile, "-NDVI.csv")), row.names=FALSE)
NDVIInTile = as.matrix(read.csv(file.path("..", "data", "pixel-based", paste0(Tile, "-NDVI.csv"))))
colnames(NDVIInTile) = paste0("NDVI-", c(getZ(TileStack)))

# Could merge into one if needed
cbind(PointsInTile, NDVIInTile)

