# Loading the validation dataset as `sf` objects
library(sf)

source("pixel-based/utils/ProbaVTileID.r")
source("pixel-based/utils/covariate-names.r")

CorrectSFDataTypes = function(df)
{
    CorrectionMap = c(rowid="integer", location_id="integer", bare="numeric", burnt="numeric", crops="numeric",
                      fallow_shifting_cultivation = "numeric", grassland="numeric", lichen_and_moss="numeric",
                      shrub="numeric", snow_and_ice="numeric", tree="numeric", urban_built_up="numeric",
                      wetland_herbaceous="numeric", water="numeric", not_sure="numeric", dominant_lc="factor",
                      lc="factor", forest_type="factor", Tile="factor", min="numeric", max="numeric",
                      intercept="numeric", co="numeric", si="numeric", co2="numeric", si2="numeric",
                      trend="numeric", phase1="numeric", amplitude1="numeric", phase2="numeric",
                      amplitude2="numeric", mean.ndvi="numeric", ndvi.25="numeric", ndvi.75="numeric",
                      ndvi.iqr="numeric", red="numeric", nir="numeric", blue="numeric", swir="numeric",
                      ndvi="numeric", ndmi="numeric", osavi="numeric", evi="numeric", elevation="integer",
                      slope="numeric", aspect="numeric", tpi="numeric", tri="numeric", roughness="integer")
    
    for (i in 1:length(CorrectionMap))
    {
        if (names(CorrectionMap[i]) %in% names(df))
            df[[names(CorrectionMap[i])]] = switch(CorrectionMap[i],
               integer=as.integer(df[[names(CorrectionMap[i])]]),
               numeric=as.numeric(df[[names(CorrectionMap[i])]]),
               factor=as.factor(df[[names(CorrectionMap[i])]]))
    }
    
    return(df)
}

# Updates the dominant_lc column based on the classes desired
UpdateDominantLC = function(df, classes = GetIIASAClassNames())
{
    ClassProportions = df[,classes]
    DominantClasses = apply(ClassProportions, 1, which.max)
    df$dominant_lc = factor(classes[DominantClasses])
    return(df)
}

# Remove rows with NAs and drop covariates with too few observations
TidyData = function(df, classes = GetIIASAClassNames())
{
    Before = nrow(df)
    DropRows = apply(df, 1, function(x){any(is.na(x))})
    df = df[!DropRows,]
    After = nrow(df)
    print(paste("Dropped NAs, data frame size reduced from", Before, "to", After))
    Before = After
    stopifnot(all(apply(df, 2, function(x){sum(is.na(x))}) / nrow(df) * 100 == 0))

    # Recalculate dominant classes based on what we want
    df = UpdateDominantLC(df, classes)
    
    # Drop samples that are too few to be its own class. Also drop pixels dominated by "not sure"
    RemainingClasses = GetLargeClassNames(df)
    RemainingClasses = RemainingClasses[RemainingClasses != "not_sure"]
    #df = df[df$dominant_lc %in% RemainingClasses,]
    
    # Drop all points that contain any of the classes we don't care for
    DroppedClasses = classes[!classes %in% RemainingClasses]
    RowsToDrop = apply(df[,DroppedClasses], 1, function(x){any(x > 0)})
    df = df[!RowsToDrop,]
    
    After = nrow(df)
    print(paste("Dropped small classes, data frame size reduced from", Before, "to", After))
    
    # Also drop the level, otherwise sampling would try to sample from 0 points
    df = UpdateDominantLC(df, RemainingClasses)
    return(df)
}

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="~/shared/training_data_100m_16042018_V2.csv")
{
    # Read data
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    
    # Set correct data types
    SamplePoints = CorrectSFDataTypes(SamplePoints)
    
    # Set CRS to WGS84
    st_crs(SamplePoints) = 4326
    
    # Add additional fields: Proba-V tile number
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    
    # Note: this takes around 40 MiB in RAM

    return(SamplePoints)
}

LoadTrainingAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates/all.csv")
{
    AllData = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    st_crs(AllData) = 4326
    AllData$field_1 = NULL # Remove duplicate ID
    AllData = suppressWarnings(CorrectSFDataTypes(AllData))
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

LoadVIMatrix = function(Tile, VI, Band, DataDir="../data/pixel-based/vegetation-indices")
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

# Append binary "lack of class x" columns to a data frame
AddZeroValueColumns = function(df)
{
    Classes = GetIIASAClassNames()
    for (Class in Classes)
    {
        ColumnName = paste("no", Class, sep=".")
        df[,ColumnName] = df[,Class]==0
    }
    return(df)
}
