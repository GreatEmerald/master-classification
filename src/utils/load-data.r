# Utility for loading data from a CSV file in a spatial manner
library(raster)

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
    rasterfiles = c("../../userdata/composite/radiometry/composite.tif",
    "../../userdata/composite/indices.tif",
    "../../userdata/dem/merged/pv-height.tif",
    "../../userdata/dem/merged/pv-slope.tif",
    "../../userdata/dem/merged/pv-aspect.tif",
    "../../userdata/dem/merged/pv-tpi.tif")
    rasters = stack(rasterfiles)
    names(rasters) = c("red", "nir", "blue", "swir", "ndvi", "osavi", "lswi",
        "height", "slope", "aspect", "tpi")
    return(rasters)
}

LoadTrainingPixels = function(tiffile = "../../userdata/pixels.tif", rfile="../../userdata/pixels.rds")
{
    if (!file.exists(rfile))
    {
        rasters = brick(tiffile)
        names(rasters) = c("red", "nir", "blue", "swir", "ndvi", "osavi", "lswi",
            "height", "slope", "aspect", "tpi")
        pixels = as(rasters, "SpatialPixelsDataFrame")
        saveRDS(pixels, compress = "bzip2", file = rfile)
    }
    else
    {
        pixels = readRDS(rfile)
    }
    return(pixels)
}
