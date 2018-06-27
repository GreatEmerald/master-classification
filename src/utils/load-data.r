# Utility for loading data from rasters and CSVs in a spatial manner
library(raster)

# TrainingFiles and TrainingNames need to be kept in sync; use this file for reference!

# Returns a vector of covariate names, with ability to exclude some
GetTrainingNames = function(uncorelated = FALSE, exclude=c())
{
    TrainingNames = c("red", "nir", "blue", "swir", "osavi", "lswi", "is.water",
        "height", "slope", "aspect", "tpi",
        "mean.ndvi", "phase1", "amplitude1", "phase2", "amplitude2")
    
    if (uncorelated)
        exclude = c("osavi", "amplitude2")
    if (length(exclude) > 0)
        for (i in 1:length(exclude))
            if (exclude[i] %in% TrainingNames)
                TrainingNames = TrainingNames[-which(TrainingNames == exclude[i])]
            else
                warning("Asked to exclude a non-existent covariate")
    
    return(TrainingNames)
}

# Returns a stack of covariate rasters, with the ability to exclude some
LoadTrainingRasters = function(uncorelated = FALSE, exclude=c(), extent=NULL, basedir="../../userdata/indices-no-na")
{
    TrainingFiles = c(file.path(basedir, "composite.tif"),
        file.path(basedir, "OSAVI.tif"),
        file.path(basedir, "LSWI.tif"),
        file.path(basedir, "watermask.tif"),
        file.path(basedir, "pv-height.tif"),
        file.path(basedir, "pv-slope.tif"),
        file.path(basedir, "pv-aspect-2.tif"),
        file.path(basedir, "pv-tpi-2.tif"),
        file.path(basedir, "phase-amplitude-6.tif"))
    
    # First load everything, then drop what we don't need
    rasters = stack(TrainingFiles)
    if (!is.null(extent))
        rasters = crop(rasters, extent)
    names(rasters) = GetTrainingNames()
    
    if (uncorelated)
        exclude = c("osavi", "amplitude2")
    if (length(exclude) > 0)
        for (i in 1:length(exclude))
            if (exclude[i] %in% names(rasters))
                rasters = dropLayer(rasters, which(names(rasters) == exclude[i]))
            else
                warning("Asked to exclude a non-existent covariate")
    
    return(rasters)
}

LoadTrainingPixels = function(tiffile = "../../userdata/indices-no-na/pixels.tif",
    rfile="../../userdata/indices-no-na/pixels.rds", exclude=c())
{
    if (!file.exists(rfile))
    {
        rasters = brick(tiffile)
        names(rasters) = GetTrainingNames()
        pixels = as(rasters, "SpatialPixelsDataFrame")
        saveRDS(pixels, compress = "bzip2", file = rfile)
    }
    else
    {
        pixels = readRDS(rfile)
    }
    
    if (length(exclude) > 0)
        for (i in 1:length(exclude))
            if (exclude[i] %in% names(pixels))
                pixels = pixels[-which(names(pixels) == exclude[i])]
            else
                warning("Asked to exclude a non-existent covariate")
            
    return(pixels)
}

GetValidationNames = function()
{
    return(c("cropland", "dec.trees", "evgr.trees", "shrubland", "grassland",
        "bare.soil", "wetland", "urban", "water"))
}

LoadClassificationData = function(filename = "../data/variables.csv", uncorelated = FALSE)
{
    data = read.csv(filename)
    coordinates(data) = ~X+Y
    projection(data) = "+proj=longlat +datum=WGS84 +no_defs"
    data$X.1 = NULL
    data$optional = NULL
    if (uncorelated)
    {
        data$osavi = NULL
        data$amplitude2 = NULL
    }
    return(data)
}

LoadValidationData = function(...)
{
    alldata = LoadClassificationData(...)
    return(alldata[1:9])
}

LoadTrainingData = function(...)
{
    alldata = LoadClassificationData(...)
    return(alldata[13:length(names(alldata))])
}
