# Utility for loading data from a CSV file in a spatial manner
library(raster)

# These need to be kept in sync; use this file for reference
TrainingNames = c("red", "nir", "blue", "swir", "ndvi", "osavi", "lswi", "is.water",
        "height", "slope", "aspect", "tpi")
TrainingFiles = c("../../userdata/composite/radiometry/composite.tif",
        "../../userdata/composite/indices.tif",
        "../../userdata/composite/watermask.tif",
        "../../userdata/dem/merged/pv-height.tif",
        "../../userdata/dem/merged/pv-slope.tif",
        "../../userdata/dem/merged/pv-aspect.tif",
        "../../userdata/dem/merged/pv-tpi.tif")
ValidationNames = c("cropland", "dec.trees", "evgr.trees", "shrubland", "grassland",
    "bare.soil", "wetland", "urban", "water")

LoadClassificationData = function(filename = "../data/variables.csv")
{
    data = read.csv(filename)
    coordinates(data) = ~X+Y
    projection(data) = "+proj=longlat +datum=WGS84 +no_defs"
    data$X.1 = NULL
    data$optional = NULL
    data$ndvi = NULL # Too similar to OSAVI
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
    return(alldata[13:22])
}

LoadTrainingRasters = function()
{
    rasters = stack(TrainingFiles)
    names(rasters) = TrainingNames
    return(rasters)
}

LoadTrainingPixels = function(tiffile = "../../userdata/pixels.tif", rfile="../../userdata/pixels.rds")
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
