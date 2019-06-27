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
OutputDir = "../data/pixel-based/raster"

SamplePoints = LoadGlobalRasterPoints()

# Tiles to process
TileList = levels(SamplePoints$Tile)#GetTileList(SamplePoints)
TileList = TileList[!TileList %in% c("X00Y08", "X00Y09", "X00Y11", "X01Y05", "X02Y07", "X02Y08", "X02Y09", "X03Y08", "X03Y09", "X04Y08", "X06Y05", "X08Y07", "X10Y10", "X10Y13", "X11Y13", "X13Y01", "X14Y03", "X14Y09", "X14Y12", "X15Y13", "X23Y07", "X23Y08", "X23Y09", "X23Y12", "X24Y12", "X25Y12", "X29Y07", "X31Y06", "X32Y06", "X33Y06", "X34Y08", "X35Y07")] # Remove incomplete tiles

# Generate a list of directories to read from.
# This list is semi-static: we don't read in more files when they arrive. Else the lengths would differ.
DataDirsDates = LoadRawDataDirs(RequiredTiles = TileList)
DataDirs = DataDirsDates$dir

BuildProbaVTileVRT = function(TileFiles, VI, Tile, Band=1, OutputDir="../../userdata/master-classification/pixel-based/vrt/raster/")
{
    Dates = getProbaVinfo(TileFiles)$date
    if (!dir.exists(OutputDir))
        dir.create(OutputDir)
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
    OutputCSVFile = file.path(OutputDir, paste0(paste(Tile, VI, Band, sep="-"), ".csv"))
    if (file.exists(OutputCSVFile))
        return(read.csv(OutputCSVFile))
        
    TileFiles = list.files(DataDirs, pattern=glob2rx(paste0("PROBAV_S5_TOC_", Tile, "*", VI, ".tif")), full.names=TRUE)
    BandStack = BuildProbaVTileVRT(TileFiles, VI, Tile, Band)
    BandValueMatrix = extract(BandStack, ExtractLocations)
    BandValueMatrix[!QCMatrix %in% FilterQC] = NA
    colnames(BandValueMatrix) = paste0(Band, "-", c(getZ(BandStack)))
    rownames(BandValueMatrix) = ExtractLocations$location_id
    if (!dir.exists(OutputDir))
        dir.create(OutputDir)
    write.csv(BandValueMatrix, OutputCSVFile)
    return(BandValueMatrix)
}

registerDoParallel(cores=4)
foreach(Tile=iter(TileList), .inorder=FALSE, .multicombine=TRUE, .verbose=TRUE) %dopar%
{
    PointsInTile = SamplePoints[SamplePoints$Tile == Tile,]
    TileFiles = list.files(DataDirs, pattern=glob2rx(paste0("PROBAV_S5_TOC_", Tile, "*", "SM", ".tif")), full.names=TRUE)
    if (nrow(PointsInTile) < 1) {
        print(paste("Skipping tile", Tile, "because there are no samples here"))
    } else if (length(TileFiles) == 0) {
        print(paste("Skipping tile", Tile, "because there are no files at", DataDirs))
    } else {
        print(paste("Processing tile", Tile))
        
        # Extract QC values
        QCStack = BuildProbaVTileVRT(TileFiles, "SM", Tile)
        print(any(duplicated(getZ(QCStack))))
        print(system.time(
            QCInTile <- extract(QCStack, PointsInTile)
            ))
        
        # Extract NDVI values
        ExtractPixelData(PointsInTile, DataDirs, "NDVI", Tile, 1, QCInTile, OutputDir=OutputDir)
        print("NDVI extraction complete, moving on to radiometry")
        
        # Radiometry: do it for 4 bands (Red, NIR, Blue, SWIR)
        for (Band in 1:4)
            ExtractPixelData(PointsInTile, DataDirs, "RADIOMETRY", Tile, Band, QCInTile, OutputDir=OutputDir)
    }
}

# Could merge into one if needed
#cbind(PointsInTile, NDVIInTile)

