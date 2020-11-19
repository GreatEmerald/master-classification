# Extract values from reference products
library(terra)
source("pixel-based/utils/load-sampling-data.r")

InDir = "../data/pixel-based/reference-products"
OutDir = "../data/pixel-based/reference-products"

if (!dir.exists(OutDir))
    dir.create(OutDir)

# Points to extract
#TrainingPoints = LoadGlobalTrainingData()
ValidationPoints = LoadGlobalValidationData()
#PredictionPoints = LoadGlobalRasterPoints()
AllPoints = ValidationPoints[,c("x", "y")]#rbind(TrainingPoints[,c("x", "y")], ValidationPoints[,c("x", "y")], PredictionPoints[,c("x", "y")])

# Put all data sources into a list
MetricNames = c("treecover", "gsw", "ghsl", "impervious")
MetricFiles = c(
    file.path(InDir, "MeasuresTreecover.vrt"),
    file.path(InDir, "GSW2015.vrt"),
    file.path(InDir, "GHSLBuiltup2014.vrt"),
    file.path(InDir, "QinghuaImpervious2015.vrt"))
names(MetricFiles) = MetricNames

for (f in 1:length(MetricFiles))
{
    MetricRast = rast(MetricFiles[f])
    
    ChunkSize = 5000
    Iterator = seq(1, nrow(AllPoints), ChunkSize)
    pb = txtProgressBar(min = 1, max = nrow(AllPoints), style = 3)
    for (i in Iterator)
    {
        ChunkEnd = min(i+ChunkSize-1, nrow(AllPoints))
        DataChunk = AllPoints[i:ChunkEnd,]
        
        DSValues = cbind(extract(MetricRast, vect(DataChunk)), DataChunk)
        names(DSValues)[1] = MetricNames[f]
        st_write(DSValues, file.path(OutDir, paste0(MetricNames[f], ".gpkg")), append=TRUE)
        rm(DSValues)
        rm(DataChunk)
        gc()
        
        setTxtProgressBar(pb, ChunkEnd)
    }
    
    close(pb)
}
