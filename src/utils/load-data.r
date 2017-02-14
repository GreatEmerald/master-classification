# Utility for loading data from a CSV file in a spatial manner
library(raster)

# These need to be kept in sync; use this file for reference
GetTrainingNames = function(uncorelated = FALSE, exclude=c())
{
    TrainingNames = c("red", "nir", "blue", "swir", "osavi", "lswi", "is.water",
        "height", "slope", "aspect", "tpi",
        "mean.ndvi", "phase1", "amplitude1", "phase2", "amplitude2")
    
    if (uncorelated)
        exclude = c("osavi", "amplitude2")
    for (i in length(exclude))
        TrainingNames = TrainingNames[-which(TrainingNames == exclude[i])]
    
    return(TrainingNames)
}

GetTrainingFiles = function(uncorelated = FALSE, exclude=c())
{
    TrainingFiles = c("../../userdata/indices-no-na/composite.tif",
        "../../userdata/indices-no-na/OSAVI.tif",
        "../../userdata/indices-no-na/LSWI.tif",
        "../../userdata/indices/watermask.tif",
        "../../userdata/indices-no-na/pv-height.tif",
        "../../userdata/indices-no-na/pv-slope.tif",
        "../../userdata/indices-no-na/pv-aspect-2.tif",
        "../../userdata/indices-no-na/pv-tpi-2.tif",
        "../../userdata/indices-no-na/phase-amplitude-6.tif")
    
    if (uncorelated)
        exclude = c("osavi", "amplitude2")
    for (i in length(exclude))
        TrainingFiles = TrainingFiles[-which(TrainingNames == exclude[i])]
}
ValidationNames = c("cropland", "dec.trees", "evgr.trees", "shrubland", "grassland",
    "bare.soil", "wetland", "urban", "water")

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

LoadTrainingRasters = function(uncorelated = FALSE)
{
    rasters = stack(TrainingFiles)
    names(rasters) = TrainingNames
    if (uncorelated)
    {
        rasters = dropLayer(rasters, which(names(rasters) == "osavi"))
        rasters = dropLayer(rasters, which(names(rasters) == "amplitude2"))
    }
    return(rasters)
}

LoadTrainingPixels = function(tiffile = "../../userdata/indices-no-na/pixels.tif",
    rfile="../../userdata/indices-no-na/pixels.rds")
{
    if (!file.exists(rfile))
    {
        rasters = brick(tiffile)
        names(rasters) = TrainingNames
        pixels = as(rasters, "SpatialPixelsDataFrame")
        saveRDS(pixels, compress = "bzip2", file = rfile)
    }
    else
    {
        pixels = readRDS(rfile)
    }
    return(pixels)
}
