# Loading the validation dataset as `sf` objects
library(sf)

source("pixel-based/utils/ProbaVTileID.r")

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="~/shared/training_data_100m_16042018_V2.csv")
{
    # Read data
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    
    # Set correct data types
    SamplePoints$rowid = as.numeric(SamplePoints$rowid)
    SamplePoints$location_id = as.numeric(SamplePoints$location_id)
    SamplePoints$bare = as.numeric(SamplePoints$bare)
    SamplePoints$burnt = as.numeric(SamplePoints$burnt)
    SamplePoints$crops = as.numeric(SamplePoints$crops)
    SamplePoints$fallow_shifting_cultivation = as.numeric(SamplePoints$fallow_shifting_cultivation)
    SamplePoints$grassland = as.numeric(SamplePoints$grassland)
    SamplePoints$lichen_and_moss = as.numeric(SamplePoints$lichen_and_moss)
    SamplePoints$shrub = as.numeric(SamplePoints$shrub)
    SamplePoints$snow_and_ice = as.numeric(SamplePoints$snow_and_ice)
    SamplePoints$tree = as.numeric(SamplePoints$tree)
    SamplePoints$urban_built_up = as.numeric(SamplePoints$urban_built_up)
    SamplePoints$wetland_herbaceous = as.numeric(SamplePoints$wetland_herbaceous)
    SamplePoints$not_sure = as.numeric(SamplePoints$not_sure)
    SamplePoints$water = as.numeric(SamplePoints$water)
    SamplePoints$dominant_lc = as.factor(SamplePoints$dominant_lc)
    SamplePoints$lc = as.factor(SamplePoints$lc)
    SamplePoints$forest_type = as.factor(SamplePoints$forest_type)
    
    # Set CRS to WGS84
    st_crs(SamplePoints) = 4326
    
    # Add additional fields: Proba-V tile number
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    
    # Note: this takes around 40 MiB in RAM

    return(SamplePoints)
}

LoadVIMatrix = function(Tile, VI, Band, DataDir="../data/pixel-based")
{
    CSVFile = file.path(DataDir, paste0(paste(Tile, VI, Band, sep="-"), ".csv"))
    if (!file.exists(CSVFile))
    {
        warning(paste("Requested to load VI matrix from a non-existing file:", CSVFile))
        return()
    }
    Result = as.matrix(read.csv(CSVFile, row.names=1))
    try(colnames(Result) <- as.character(LoadRawDataDirs()$date)) # May fail if we don't have the cache file
    return(Result)
}

LoadRawDataDirs = function(CacheFile = "../data/DataDirs.csv", ...)
{
    
    if (!file.exists(CacheFile))
    {
        DataDirs = ProbaVValidDirs(...)
        DataDirsDates = data.frame(dir=DataDirs, date=as.Date(basename(dirname(DataDirs)), format="%Y%m%d"))
        write.csv(DataDirsDates, CacheFile, row.names=FALSE)
    } else {
        print(paste("Reusing existing list of data directories from", CacheFile))
        DataDirsDates = read.csv(CacheFile, stringsAsFactors=FALSE)
        DataDirsDates$date = as.Date(DataDirsDates$date)
    }
    return(DataDirsDates)
}

# Get a list of relevant tiles (tiles with observations over Africa at the moment)
GetTileList = function(SamplePoints = NULL)
{
    if (is.null(SamplePoints))
        SamplePoints = LoadGlobalTrainingData()
    
    TileList = expand.grid(sprintf("X%02d", 15:23), sprintf("Y%02d", 3:10), stringsAsFactors = FALSE)
    TileList = paste0(TileList[,1], TileList[,2])
    
    # Keep only the tiles that are in the reference data
    TileList = TileList[TileList %in% levels(SamplePoints$Tile)]
    return(TileList)
}
