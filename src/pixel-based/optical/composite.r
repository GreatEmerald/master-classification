# Script to composite surface reflectance into a cloud-free "map" and calculate vegetation indices

library(foreach)
library(doParallel)
source("pixel-based/utils/load-sampling-data.r")

# Load data and dates
SamplingPoints = LoadGlobalTrainingData()
TileList = GetTileList(SamplingPoints)
Dates = LoadRawDataDirs()$date
# Subset data to summer 2016
StartIndex = which(Dates == as.Date("2016-06-01"))
EndIndex = which(Dates == as.Date("2016-08-26"))

# There must be a nicer way to do this, but I can't think of any
Subsetter = function(Tile, VI, Band)
{
    VIMatrix = LoadVIMatrix(Tile, VI, Band) # Load the VI matrix from file
    VIMatrix = VIMatrix[,StartIndex:EndIndex] # Subset to summer 2016
    
    stopifnot(nrow(VIMatrix) == length(MaxNDVIIdx))
    
    # Get the value at the highest NDVI time
    Result = c()
    for (i in 1:nrow(VIMatrix))
        Result = c(Result, VIMatrix[i, MaxNDVIIdx[i]])
    names(Result) = rownames(VIMatrix)
    return(Result)
}

ScaleNDVI = function(NDVI){return((NDVI - 20) / 250)}
OSAVI = function(RED, NIR, BLUE, SWIR){return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))}
NDMI = function(RED, NIR, BLUE, SWIR){return((NIR-SWIR)/(NIR+SWIR))}
EVI = function(RED, NIR, BLUE, SWIR){return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE)+1)))}

# Should be fast enough to not need parallelisation, but hey, why not
registerDoParallel(cores=4)
Covariates = foreach(Tile=iter(TileList), .combine=rbind, .inorder=FALSE, .multicombine=TRUE) %do%
{
    NDVIMatrix = LoadVIMatrix(Tile, "NDVI", 1)
    NDVIMatrix = ScaleNDVI(NDVIMatrix)
    
    # Get mean NDVI over the whole time series
    MeanNDVI = unlist(apply(NDVIMatrix, 1, mean, na.rm=TRUE))
    NDVILow = unlist(apply(NDVIMatrix, 1, quantile, na.rm=TRUE, probs=0.25))
    NDVIHigh = unlist(apply(NDVIMatrix, 1, quantile, na.rm=TRUE, probs=0.75))
    NDVIIQR = unlist(apply(NDVIMatrix, 1, IQR, na.rm=TRUE))
    
    # Subset to summer 2016
    NDVIMatrix = NDVIMatrix[,StartIndex:EndIndex]
    
    # Get date of highest NDVI, that will be the one that we use for the other bands too
    # The function is which.max that on all NAs returns NA rather than integer(0)
    MaxNDVIIdx = unlist(apply(NDVIMatrix, 1, function(x){a=which.max(x); return(ifelse(length(a)<1, NA, a))}))
    
    Band1Matrix = Subsetter(Tile, "RADIOMETRY", 1) / 2000
    Band2Matrix = Subsetter(Tile, "RADIOMETRY", 2) / 2000
    Band3Matrix = Subsetter(Tile, "RADIOMETRY", 3) / 2000
    Band4Matrix = Subsetter(Tile, "RADIOMETRY", 4) / 2000
    # The provided NDVI matches NDVI we get from calculating, except for loss of precision
    NDVIMatrix = ScaleNDVI(Subsetter(Tile, "NDVI", 1))
    NDMIMatrix = NDMI(Band1Matrix, Band2Matrix, Band3Matrix, Band4Matrix)
    OSAVIMatrix = OSAVI(Band1Matrix, Band2Matrix, Band3Matrix, Band4Matrix)
    EVIMatrix = EVI(Band1Matrix, Band2Matrix, Band3Matrix, Band4Matrix)
    
    # Scale and merge all into a data frame
    Result = data.frame(mean.ndvi=MeanNDVI,
                        ndvi.25=NDVILow,
                        ndvi.75=NDVIHigh,
                        ndvi.iqr=NDVIIQR,
                        red=Band1Matrix,
                        nir=Band2Matrix,
                        blue=Band3Matrix,
                        swir=Band4Matrix,
                        ndvi=NDVIMatrix,
                        ndmi=NDMIMatrix,
                        osavi=OSAVIMatrix,
                        evi=EVIMatrix)
}

write.csv(Covariates, "../data/pixel-based/covariates/spectral.csv")

# Subset the entire dataset to only the points that we have processed (= all points in Africa).
# This should be done at the very end, after all processing is complete, possibly in a separate file.
#AfricanSamples = subset(SamplePoints, subset=location_id %in% rownames(Covariates))
#Covariates$location_id = rownames(Covariates)
#AfricanSamples = merge(AfricanSamples, Covariates, by="location_id")

