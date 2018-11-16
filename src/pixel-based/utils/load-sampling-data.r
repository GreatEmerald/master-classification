# Loading the validation dataset as `sf` objects
library(sf)

source("pixel-based/utils/ProbaVTileID.r")
source("pixel-based/utils/covariate-names.r")

CorrectSFDataTypes = function(df)
{
    CorrectionMap = c(rowid="integer", location_id="integer", bare="numeric", burnt="integer", crops="numeric",
                      fallow_shifting_cultivation = "numeric", grassland="numeric", lichen_and_moss="numeric",
                      shrub="numeric", snow_and_ice="integer", tree="numeric", urban_built_up="numeric",
                      wetland_herbaceous="integer", water="numeric", not_sure="numeric", dominant_lc="factor",
                      lc="factor", forest_type="factor", Tile="factor", min="numeric", max="numeric",
                      intercept="numeric", co="numeric", si="numeric", co2="numeric", si2="numeric",
                      trend="numeric", phase1="numeric", amplitude1="numeric", phase2="numeric",
                      amplitude2="numeric", mean.ndvi="numeric", ndvi.25="numeric", ndvi.75="numeric",
                      ndvi.iqr="numeric", red="numeric", nir="numeric", blue="numeric", swir="numeric",
                      ndvi="numeric", ndmi="numeric", osavi="numeric", evi="numeric", elevation="integer",
                      slope="numeric", aspect="numeric", tpi="numeric", tri="numeric", roughness="integer",
                      validation_id="integer", confidence="integer", sample_id="integer", userid="integer",
                      timestamp="POSIXct", used_google_maps="factor", update4submissionid="integer",
                      count="integer", trees="numeric", grass="numeric", urban="numeric", wetland="numeric",
                      tree_type="factor", class_s30="factor", class_s_plus5="factor", class_s_minus5="factor",
                      map_tree="integer", map_shrubs="integer", map_grass="integer", map_bare="integer",
                      strata="factor", rc4="factor", res_bare="numeric", res_tree="numeric", res_shrubs="numeric",
                      res_grass="numeric", des_weight="numeric", incl.p="numeric")
    
    for (i in 1:length(CorrectionMap))
    {
        if (names(CorrectionMap[i]) %in% names(df))
            df[[names(CorrectionMap[i])]] = switch(CorrectionMap[i],
               integer=as.integer(df[[names(CorrectionMap[i])]]),
               numeric=as.numeric(df[[names(CorrectionMap[i])]]),
               factor=as.factor(df[[names(CorrectionMap[i])]]),
               POSIXct=as.POSIXct(df[[names(CorrectionMap[i])]]))
    }
    
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

# Load the validation dataset (WUR)
LoadGlobalValidationData = function(filename="../data/data_dainius.csv")
{
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=sample_x", "Y_POSSIBLE_NAMES=sample_y"), stringsAsFactors = FALSE)
    st_crs(SamplePoints) = 4326
    names(SamplePoints)[names(SamplePoints) == "sample_x"] = "x"
    names(SamplePoints)[names(SamplePoints) == "sample_y"] = "y"
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    return(SamplePoints)
}

LoadTrainingAndCovariates = function(zerovalues=FALSE, excludenotsure=TRUE, filename="../data/pixel-based/covariates/all.csv")
{
    AllData = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    st_crs(AllData) = 4326
    AllData$field_1 = NULL # Remove duplicate ID
    AllData = suppressWarnings(CorrectSFDataTypes(AllData))
    
    # Redo the dominant_lc column to exclude "not sure"
    if (excludenotsure)
    {
        Classes = GetIIASAClassNames()
        ClassProportions = AllData
        class(ClassProportions) = "data.frame"
        ClassProportions = ClassProportions[,Classes]
        DominantClasses = apply(ClassProportions, 1, which.max)
        AllData$dominant_lc = factor(Classes[DominantClasses])
    }
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

LoadValidationAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates/all-validation.csv")
{
    AllData = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    st_crs(AllData) = 4326
    AllData$field_1 = NULL # Remove duplicate ID
    AllData = suppressWarnings(CorrectSFDataTypes(AllData))
    
    # Unify class names
    names(AllData)[names(AllData) == "trees"] = "tree"
    names(AllData)[names(AllData) == "grass"] = "grassland"
    names(AllData)[names(AllData) == "urban"] = "urban_built_up"
    names(AllData)[names(AllData) == "wetland"] = "wetland_herbaceous"
    
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
    # There are cases where elements of the time series are missing (perhaps it's over sea etc.)
    # So we need to make new columns filled with NAs
    Dates = as.character(LoadRawDataDirs()$date)
    if (ncol(Result) < length(Dates))
    {
        # Convert names to dates
        ResultNames = colnames(Result)
        ResultNames = gsub(".", "-", ResultNames, fixed=TRUE)
        ResultNames = substr(ResultNames, 4, 13)
        colnames(Result) = ResultNames
        AdditionalColAmount = sum(!Dates %in% ResultNames)
        AdditionalNAs = matrix(NA, nrow(Result), AdditionalColAmount)
        colnames(AdditionalNAs) = Dates[!Dates %in% ResultNames]
        FilledResult = cbind(Result, AdditionalNAs)
        Result = FilledResult[,order(colnames(FilledResult))]
    } else {
        try(colnames(Result) <- Dates) # May fail if we don't have the cache file
    }
    # Sometimes we have only one point, in which case it gets read as a vector rather
    # than a matrix. Fix those cases.
    if (length(Result) > 0 && is.null(dim(Result)))
        Result = t(as.matrix(Result))
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
