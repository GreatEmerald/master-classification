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

    Before = nrow(df)
    if (is.character(drop.cols))
    {
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
    # Drop those dominated by "not_sure"
    df = df[df$dominant_lc != "not_sure",]
    
    # Reclassify rare classes to common ones
    df = ReclassifyAndScale(df)
    
    After = nrow(df)
    print(paste("Reclassified and rescaled small classes, data frame size reduced from", Before, "to", After))
    
    # Also drop the level, otherwise sampling would try to sample from 0 points
    df = UpdateDominantLC(df, classes)
    
    return(df)
}

# Replace NAs with means
NAToMean = function(df, cols=names(df))
{
    #NumData = as.matrix(df[,cols])
    CM = colMeans(df[,cols], na.rm=TRUE)
    for (col in cols)
    {
        ColWithNA = df[[col]]
        ColWithNA[is.na(ColWithNA)] = CM[col]
        df[[col]] = ColWithNA
    }
    return(df)
}

# Rescale a data.frame based on the distribution from another
RescaleBasedOn = function(what, template, cols=intersect(names(what), names(template)))
{
    for (col in cols) {
        what[,col] = (what[,col] - mean(template[,col], na.rm=TRUE)) / sd(template[,col], na.rm=TRUE)
    }
    return(what)
}

ReclassifyAndScale = function(df, output.classes=GetCommonClassNames())
{
    # Some classes map to other classes. Put the values there
    ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
    
    for (class in 1:length(ClassMap))
    {
        if (names(ClassMap[class]) %in% names(df))
            df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
    }
    
    # Scale relevant classes to 100%; that way we get rid of influences from not_sure and snow_and_ice
    RelevantClasses = df[, output.classes]
    ClassSums = rowSums(RelevantClasses)
    ZeroRows = ClassSums == 0
    if (any(ZeroRows))
    {
        print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
        RelevantClasses = RelevantClasses[!ZeroRows,]
        df = df[!ZeroRows,]
        ClassSums = ClassSums[!ZeroRows]
    }
    
    df[,output.classes] = RelevantClasses / (ClassSums / 100)
    stopifnot(all(round(rowSums(df[,output.classes])) == 100))
    return(df)
}

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="../data/pixel-based/raw-points/training_data_2015_100m_20190402_V4.csv")
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
LoadGlobalValidationData = function(filename="../data/pixel-based/raw-points/refdata_world_africa_included_locations_data20190709.csv")
{
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=sample_x,subpix_mean_x", "Y_POSSIBLE_NAMES=sample_y,subpix_mean_y"), stringsAsFactors = FALSE)
    st_crs(SamplePoints) = 4326
    names(SamplePoints)[names(SamplePoints) == "sample_x" | names(SamplePoints) == "subpix_mean_x"] = "x"
    names(SamplePoints)[names(SamplePoints) == "sample_y" | names(SamplePoints) == "subpix_mean_y"] = "y"
    SamplePoints = CorrectSFDataTypes(SamplePoints)
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    return(SamplePoints)
}

# Load points for a wall-to-wall global map
LoadGlobalRasterPoints = function(filename="../data/pixel-based/raw-points/global-point-grid-02deg.csv")
{
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=X", "Y_POSSIBLE_NAMES=Y"))
    st_crs(SamplePoints) = 4326
    names(SamplePoints)[names(SamplePoints) == "X"] = "x"
    names(SamplePoints)[names(SamplePoints) == "Y"] = "y"
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    return(SamplePoints)
}

LoadTrainingAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates.gpkg")
{
    TrainPoints = LoadGlobalTrainingData()
    AllData = st_read(filename)
    
    AllData = merge(TrainPoints, as.data.frame(AllData)[names(AllData) != "geom"], by.x=c("x", "y"), by.y=c("X", "Y"))
    AllData$yabs = abs(AllData$y)
    AllData = AddClusterColumns(AllData)
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

LoadValidationAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates.gpkg")
{
    ValidationPoints = LoadGlobalValidationData()
    AllData = st_read(filename)
    
    # Unify class names
    names(ValidationPoints)[names(ValidationPoints) == "trees"] = "tree"
    names(ValidationPoints)[names(ValidationPoints) == "grass"] = "grassland"
    names(ValidationPoints)[names(ValidationPoints) == "urban"] = "urban_built_up"
    names(ValidationPoints)[names(ValidationPoints) == "wetland"] = "wetland_herbaceous"
    
    AllData = merge(ValidationPoints, as.data.frame(AllData)[names(AllData) != "geom"], by.x=c("x", "y"), by.y=c("X", "Y"))
    AllData$yabs = abs(AllData$y)
    AllData = AddClusterColumns(AllData)
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

LoadPredictionAndCovariates = function(zerovalues=FALSE, filename="../data/pixel-based/covariates.gpkg")
{
    PredPoints = LoadGlobalRasterPoints()
    AllData = st_read(filename)
    
    AllData = merge(PredPoints, as.data.frame(AllData)[names(AllData) != "geom"], by.x=c("x", "y"), by.y=c("X", "Y"))
    AllData$yabs = abs(AllData$y)
    AllData = AddClusterColumns(AllData)
    
    if (zerovalues)
        AllData = AddZeroValueColumns(AllData)
    return(AllData)
}

# No longer doing that on the fly, see merge-databases.r
# PostprocessCovars = function(df)
# {
#     df$amplitude1 = log(df$amplitude1+0.01)
#     df$amplitude2 = log(df$amplitude2+0.001)
#     # Outlier sine/cosine values; very conservative
#     df$co[df$co > 1000 | df$co < -1000] = NA
#     df$si[df$si > 1000 | df$si < -1000] = NA
#     df$co2[df$co2 > 1000 | df$co2 < -1000] = NA
#     df$si2[df$si2 > 1000 | df$si2 < -1000] = NA
#     # All bioclimatic ones from 12
#     df$bio12 = log(df$bio12+1)
#     df$bio13 = log(df$bio13+1)
#     df$bio14 = log(df$bio14+1)
#     df$bio16 = log(df$bio16+1)
#     df$bio17 = log(df$bio17+1)
#     df$bio18 = log(df$bio18+1)
#     df$bio19 = log(df$bio19+1)
#     df$ORCDRC.M.sl7 = log(df$ORCDRC.M.sl7+1)
#     df$ORCDRC.M.sl6 = log(df$ORCDRC.M.sl6+1)
#     df$ORCDRC.M.sl5 = log(df$ORCDRC.M.sl5+1)
#     df$ORCDRC.M.sl4 = log(df$ORCDRC.M.sl4+1)
#     df$ORCDRC.M.sl3 = log(df$ORCDRC.M.sl3+1)
#     df$ORCDRC.M.sl2 = log(df$ORCDRC.M.sl2+1)
#     df$ORCDRC.M.sl1 = log(df$ORCDRC.M.sl1+1)
#     df$OCSTHA.M.sd1 = log(df$OCSTHA.M.sd1+1)
#     df$OCSTHA.M.sd2 = log(df$OCSTHA.M.sd2+1)
#     df$OCSTHA.M.sd3 = log(df$OCSTHA.M.sd3+1)
#     df$OCSTHA.M.sd4 = log(df$OCSTHA.M.sd4+1)
#     df$OCSTHA.M.sd5 = log(df$OCSTHA.M.sd5+1)
#     df$OCSTHA.M.sd6 = log(df$OCSTHA.M.sd6+1)
#     df$tri = log(df$tri+0.1)
#     df$roughness = log(df$roughness+1)
#     df$slope = log(df$slope+0.1)
#     
#     return(df)
# }

LoadVIMatrix = function(Tile = "%", VI, Band = NULL, DBFile="../data/pixel-based/timeseries/timeseries.gpkg")
{
    # Band is deprecated, part of VI now
    # Tile is optional, just to reduce the number of points in memory
    if (!file.exists(DBFile))
    {
        warning(paste("Requested to load VI matrix from a non-existing file:", DBFile))
        return()
    }
    Result = as.matrix(st_read(DBFile, VI, query=paste0("SELECT * FROM ", VI, " WHERE Tile LIKE '", Tile, "'")))
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

AddClusterColumns = function(df, filename="../data/pixel-based/biomes/ProbaV_UTM_LC100_biome_clusters_V3_global.gpkg")
{
    EClusters = st_read(filename)
    EClusters = EClusters[,"bc_id"]
    Result = st_join(df, EClusters)
    NoCluster = is.na(Result$bc_id)
    ClosestZones = st_nearest_feature(Result[NoCluster,], EClusters)
    Result[NoCluster,]$bc_id = EClusters$bc_id[ClosestZones]
    return(Result)
}

# Get a list of neighbouring clusters for each cluster.
# This was made by buffering the ecozones by 0.2 degrees (due to missing topology for st_touches() in QGIS.
# The result is a list where each element has a name of a zone and the entries are names of the neighbours.
ClusterNeighbours = function(filename="../data/pixel-based/biomes/ProbaV_UTM_LC100_biome_clusters_buffered.gpkg")
{
    EClusters = st_read(filename, quiet=TRUE)
    ECNeighbours = st_intersects(EClusters)
    names(ECNeighbours) = EClusters[["bc_id"]]
    ECNCodes = lapply(ECNeighbours, function(x)as.character(EClusters[["bc_id"]][x]))
    # Reorder so that the zone itself is always first
    ECNCOrdered = lapply(1:length(ECNCodes), function(x) {
        idx = ECNCodes[[x]]==names(ECNCodes)[x]
        c(ECNCodes[[x]][idx], ECNCodes[[x]][!idx])
    })
    names(ECNCOrdered) = names(ECNCodes)
    return(ECNCOrdered)
}
