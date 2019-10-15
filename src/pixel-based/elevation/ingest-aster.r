# Ingest terrain data into a GeoPackage
# Run this after calc-covars.sh

library(raster)

source("pixel-based/utils/load-sampling-data.r")

InDir = "../../userdata/master-classification/dem/aster-covars/"
OutDir = "../data/pixel-based/terrain"

if (!dir.exists(OutDir))
    dir.create(OutDir)

# Points to extract
TrainingPoints = LoadGlobalTrainingData()
ValidationPoints = LoadGlobalValidationData()
PredictionPoints = LoadGlobalRasterPoints()
AllPoints = rbind(TrainingPoints[,c("x", "y")], ValidationPoints[,c("x", "y")], PredictionPoints[,c("x", "y")])

# Put all data sources into a list
MetricNames = c("elevation", "slope", "aspect", "tpi", "roughness")
MetricStack = stack(c(
    file.path(InDir, "elevation.vrt"),
    file.path(InDir, "slope.tif"),
    file.path(InDir, "aspect.tif"),
    file.path(InDir, "tpi.tif"),
    file.path(InDir, "roughness.tif")))
names(MetricStack) = MetricNames

# Write to database in chunks
ChunkSize = 5000
Iterator = seq(1, nrow(AllPoints), ChunkSize)
pb = txtProgressBar(min = 1, max = nrow(AllPoints), style = 3)
for (i in Iterator)
{
    ChunkEnd = min(i+ChunkSize-1, nrow(AllPoints))
    DataChunk = AllPoints[i:ChunkEnd,]
    
    DSValues = st_as_sf(extract(MetricStack, DataChunk, df=TRUE, sp=TRUE))
    st_write(DSValues, file.path(OutDir, "terrain.gpkg"))
    rm(DSValues)
    rm(DataChunk)
    gc()
    
    setTxtProgressBar(pb, ChunkEnd)
}

close(pb)
