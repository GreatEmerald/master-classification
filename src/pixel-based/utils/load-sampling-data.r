# Loading the validation dataset as `sf` objects
library(sf)

source("pixel-based/utils/ProbaVTileID.r")
source("pixel-based/utils/covariate-names.r")

CorrectSFDataTypes = function(df)
{
    CorrectionMap = c(yabs="numeric",rowid="integer", location_id="integer", bare="numeric", burnt="numeric", crops="numeric",
                      fallow_shifting_cultivation = "numeric", grassland="numeric", lichen_and_moss="numeric",
                      shrub="numeric", snow_and_ice="numeric", tree="numeric", urban_built_up="numeric",
                      wetland_herbaceous="numeric", water="numeric", not_sure="numeric", dominant_lc="factor",
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
    ClimCovars = GetCovarNames("climate")
    CCCorrection = rep("numeric", length(ClimCovars))
    names(CCCorrection) = ClimCovars
    SoilCovars = c(GetLandGISCovars(), GetSoilGridsCovars(), GetSoilGridsClasses())
    SCCorrection = rep("numeric", length(SoilCovars))
    names(SCCorrection) = SoilCovars
    CorrectionMap = c(CorrectionMap, CCCorrection, SCCorrection)
    
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

# Updates the dominant_lc column based on the classes desired
UpdateDominantLC = function(df, classes = GetCommonClassNames())
{
    ClassProportions = df[,classes[classes %in% names(df)]]
    DominantClasses = apply(ClassProportions, 1, which.max)
    df$dominant_lc = factor(classes[DominantClasses])
    return(df)
}

# Remove rows with NAs and drop covariates with too few observations
TidyData = function(df, classes = GetCommonClassNames(), drop.cols=GetUncorrelatedPixelCovars())
{
    # Remove rows that have NA in some key columns.
    # NA in elevation means that the point is not actually in Africa. Remove those.
    # NA in amplitude1 means that we have no time series over the area, so remove these (though likely it's bare soil)
    # NA in evi means that we didn't have an image from the summer of year 2016. That's a lot of points to remove; but the alternative is dropping those covars altogether.
    # NA in soil or climate covars means it's over water.

    if (is.character(drop.cols))
    {
        Before = nrow(df)
        DropRows = apply(df[,drop.cols], 1, function(x){any(!is.finite(x))})
        #DropRows = apply(df[,GetAllPixelCovars()], 1, function(x){any(is.na(x))})
        df = df[!DropRows,]
        After = nrow(df)
        print(paste("Dropped NAs, data frame size reduced from", Before, "to", After))
        Before = After
        
        stopifnot(all(apply(df[,drop.cols], 2, function(x){sum(is.na(x))}) / nrow(df) * 100 == 0))
    }

    # Recalculate dominant classes based on all classes
    df = UpdateDominantLC(df, GetIIASAClassNames(FALSE))
    # Drop those dominated by "not_sure" and "snow_and_ice"
    df = df[df$dominant_lc != "not_sure" & df$dominant_lc != "snow_and_ice",]
    
    # Reclassify rare classes to common ones
    df = ReclassifyAndScale(df)
    
    After = nrow(df)
    print(paste("Reclassified and rescaled small classes, data frame size reduced from", Before, "to", After))
    
    # Also drop the level, otherwise sampling would try to sample from 0 points
    df = UpdateDominantLC(df, classes)
    
    return(df)
}

ReclassifyAndScale = function(df, output.classes=GetCommonClassNames())
{
    # Some classes map to other classes. Put the values there
    ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", wetland_herbaceous="grassland", lichen_and_moss="grassland")
    
    for (class in 1:length(ClassMap))
    {
        if (names(ClassMap[class]) %in% names(df))
            df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
    }
    
    # Scale relevant classes to 100%; that way we get rid of influences from not_sure and snow_and_ice
    RelevantClasses = df[,output.classes]
    df[,output.classes] = RelevantClasses / (rowSums(RelevantClasses) / 100)
    stopifnot(all(round(rowSums(df[,output.classes])) == 100))
    return(df)
}

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="../data/pixel-based/raw/training_data_100m_16042018_V2.csv")
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
LoadGlobalValidationData = function(filename="../data/pixel-based/raw/data_dainius.csv")
{
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=sample_x", "Y_POSSIBLE_NAMES=sample_y"), stringsAsFactors = FALSE)
    st_crs(SamplePoints) = 4326
    names(SamplePoints)[names(SamplePoints) == "sample_x"] = "x"
    names(SamplePoints)[names(SamplePoints) == "sample_y"] = "y"
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    return(SamplePoints)
}

LoadTrainingAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates/all.csv")
{
    AllData = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    st_crs(AllData) = 4326
    AllData$field_1 = NULL # Remove duplicate ID
    AllData = suppressWarnings(CorrectSFDataTypes(AllData))
    AllData = PostprocessCovars(AllData) # Outlier removal
    
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
    AllData = PostprocessCovars(AllData) # Outlier removal
    
    # Unify class names
    names(AllData)[names(AllData) == "trees"] = "tree"
    names(AllData)[names(AllData) == "grass"] = "grassland"
    names(AllData)[names(AllData) == "urban"] = "urban_built_up"
    names(AllData)[names(AllData) == "wetland"] = "wetland_herbaceous"
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

PostprocessCovars = function(df)
{
    df$amplitude1 = log(df$amplitude1+0.01)
    df$amplitude2 = log(df$amplitude2+0.001)
    # Outlier sine/cosine values; very conservative
    df$co[df$co > 1000 | df$co < -1000] = NA
    df$si[df$si > 1000 | df$si < -1000] = NA
    df$co2[df$co2 > 1000 | df$co2 < -1000] = NA
    df$si2[df$si2 > 1000 | df$si2 < -1000] = NA
    # All bioclimatic ones from 12
    df$bio12 = log(df$bio12+1)
    df$bio13 = log(df$bio13+1)
    df$bio14 = log(df$bio14+1)
    df$bio16 = log(df$bio16+1)
    df$bio17 = log(df$bio17+1)
    df$bio18 = log(df$bio18+1)
    df$bio19 = log(df$bio19+1)
    df$ORCDRC.M.sl7 = log(df$ORCDRC.M.sl7+1)
    df$ORCDRC.M.sl6 = log(df$ORCDRC.M.sl6+1)
    df$ORCDRC.M.sl5 = log(df$ORCDRC.M.sl5+1)
    df$ORCDRC.M.sl4 = log(df$ORCDRC.M.sl4+1)
    df$ORCDRC.M.sl3 = log(df$ORCDRC.M.sl3+1)
    df$ORCDRC.M.sl2 = log(df$ORCDRC.M.sl2+1)
    df$ORCDRC.M.sl1 = log(df$ORCDRC.M.sl1+1)
    df$OCSTHA.M.sd1 = log(df$OCSTHA.M.sd1+1)
    df$OCSTHA.M.sd2 = log(df$OCSTHA.M.sd2+1)
    df$OCSTHA.M.sd3 = log(df$OCSTHA.M.sd3+1)
    df$OCSTHA.M.sd4 = log(df$OCSTHA.M.sd4+1)
    df$OCSTHA.M.sd5 = log(df$OCSTHA.M.sd5+1)
    df$OCSTHA.M.sd6 = log(df$OCSTHA.M.sd6+1)
    df$tri = log(df$tri+0.1)
    df$roughness = log(df$roughness+1)
    df$slope = log(df$slope+0.1)
    
    return(df)
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
