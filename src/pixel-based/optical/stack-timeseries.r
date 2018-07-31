# Generates VRTs for all the Proba-V imagery to use as a stack.
library(gdalUtils)
library(raster)
library(foreach)
library(doParallel)
source("pixel-based/utils/getProbaVInfo.r")
source("pixel-based/utils/load-sampling-data.r")
source("utils/GetProbaVQCMask.r")
source("pixel-based/utils/ProbaVDataDirs.r")

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
OutputDir = "../data/pixel-based"

SamplePoints = LoadGlobalTrainingData()

# Tiles to process
TileList = expand.grid(sprintf("X%02d", 15:23), sprintf("Y%02d", 3:10), stringsAsFactors = FALSE)
TileList = paste0(TileList[,1], TileList[,2])
# Keep only the tiles that are in the reference data
TileList = TileList[TileList %in% levels(SamplePoints$Tile)]

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
    DataDirsDates$date = as.Date(DataDirsDates$date)
    DataDirs = DataDirsDates$dir
}

BuildProbaVTileVRT = function(DataDirs, VI, Tile, Band=1, OutputDir="../../userdata/master-classification/pixel-based/vrt/sr/")
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

# Extract time series data from the Proba-V tiles and mask them with the quality control layer.
# Returns a matrix of extracted, masked values.
# FilterQC values are based on GetProbaVQCMask(bluegood=TRUE, redgood=TRUE, nirgood=TRUE, swirgood=TRUE, ice=FALSE, cloud=FALSE, shadow=FALSE)
ExtractPixelData = function(ExtractLocations, DataDirs, VI, Tile, Band, QCMatrix, FilterQC=c(240, 248), OutputDir="../data/pixel-based")
{
    OutputCSVFile = file.path(OutputDir, paste(Tile, VI, Band, ".csv", sep="-"))
    if (file.exists(OutputCSVFile))
        return(read.csv(OutputCSVFile))
        
    BandStack = BuildProbaVTileVRT(DataDirs, VI, Tile, Band)
    BandValueMatrix = extract(BandStack, ExtractLocations)
    BandValueMatrix[!QCMatrix %in% FilterQC] = NA
    colnames(BandValueMatrix) = paste0(Band, "-", c(getZ(BandStack)))
    write.csv(BandValueMatrix, OutputCSVFile, row.names=FALSE)
    return(BandValueMatrix)
}

registerDoParallel(cores=4)
foreach(Tile=iter(TileList), .inorder=FALSE, .multicombine=TRUE) %dopar%
{
    PointsInTile = SamplePoints[SamplePoints$Tile == Tile,]
    if (nrow(PointsInTile) < 1)
        print(paste("Skipping tile", Tile, "because there are no samples here"))
    else
    {
        print(paste("Processing tile", Tile))
        
        # Extract QC values
        QCStack = BuildProbaVTileVRT(DataDirs, "SM", Tile)
        print(any(duplicated(getZ(QCStack))))
        system.time(QCInTile <- extract(QCStack, PointsInTile))
        
        # Extract NDVI values
        ExtractPixelData(PointsInTile, DataDirs, "NDVI", Tile, 1, QCInTile)
        print("NDVI extraction complete, moving on to radiometry")
        
        # Radiometry: do it for 4 bands (Red, NIR, Blue, SWIR)
        for (Band in 1:4)
            ExtractPixelData(PointsInTile, DataDirs, "RADIOMETRY", Tile, Band, QCInTile)
    }
}

# Could merge into one if needed
#cbind(PointsInTile, NDVIInTile)

