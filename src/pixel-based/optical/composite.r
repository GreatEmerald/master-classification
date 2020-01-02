# Script to composite surface reflectance into a cloud-free "map" and calculate vegetation indices

library(sf)
library(lubridate)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/ProbaVDataDirs.r")
source("pixel-based/utils/db-io.r")

# Load data and dates
InputDB = "../data/pixel-based/timeseries/timeseries-cloudfree.gpkg"
TempDB = "../data/pixel-based/timeseries/composites-intermediate.gpkg"
VIDB = "../data/pixel-based/timeseries/VI-TS.gpkg"
OutputDB = "../data/pixel-based/timeseries/composites.gpkg"

Dates = LoadRawDataDirs()$date
ColDates = paste0("X", gsub("-", "", Dates))
VIs = c("NDMI", "OSAVI", "EVI", "NIRv")
seasons = c("year", "spring", "summer", "autumn", "winter")
XYs = st_read(InputDB, "RADIOMETRY-1", query="SELECT X, Y, geom FROM 'RADIOMETRY-1'")

# There must be a nicer way to do this, but I can't think of any
Subsetter = function(VI, Band)
{
    VIMatrix = LoadVIMatrix(Tile, VI, Band, DataDir=DataDir) # Load the VI matrix from file
    VIMatrix = VIMatrix[,StartIndex:EndIndex] # Subset to summer 2016
    if (length(VIMatrix) > 0 && is.null(dim(VIMatrix)))
        VIMatrix = t(as.matrix(VIMatrix))
    
    stopifnot(nrow(VIMatrix) == length(MaxNDVIIdx))
    
    # Get the value at the highest NDVI time
    Result = c()
    for (i in 1:nrow(VIMatrix))
    {
        ResultIndex = MaxNDVIIdx[i]
        Result = c(Result, ifelse(is.na(ResultIndex), NA, VIMatrix[i, ResultIndex]))
    }
    names(Result) = rownames(VIMatrix)
    return(Result)
}

ScaleNDVI = function(NDVI){return((NDVI - 20) / 250)}
OSAVI = function(RED, NIR, BLUE=NULL, SWIR=NULL){return(1.16 * ((NIR-RED)/(NIR+RED+0.16)))}
NDMI = function(RED=NULL, NIR, BLUE=NULL, SWIR){return((NIR-SWIR)/(NIR+SWIR))}
EVI = function(RED, NIR, BLUE, SWIR=NULL){return(2.5*((NIR-RED)/((NIR + 6*RED - 7.5*BLUE)+1)))}
NIRv = function(RED, NIR, BLUE=NULL, SWIR=NULL){((NIR-RED) / (NIR+RED))*NIR}

# Subset input to the specified year/season smartly
SeasonMask = function(dfrow, type=c("year", "spring", "summer", "autumn", "winter"), dates=Dates)
{
    stopifnot(length(dfrow) == length(dates))
    
    type = match.arg(type)
    DOY = yday(dates)
    KeepIdx = switch(type, year=rep(TRUE, length(dates)),
        spring=DOY >= 60 & DOY <= 151,
        summer=DOY > 151 & DOY <= 243,
        autumn=DOY > 243 & DOY <= 334,
        winter=DOY < 60  | DOY > 334)
    KeepIdxThreeYears = KeepIdx & year(dates) < 2017
    
    
    MaskedRow = dfrow
    MaskedRow[!KeepIdxThreeYears] = NA
    if (all(is.na(MaskedRow))) # Do we have at least one observation? If not, use whole time series
    {
        MaskedRow = dfrow
        MaskedRow[!KeepIdx] = NA
    }
    
    return(MaskedRow)
}

# Utility iterator function around SeasonMask()
SMMeanMedian = function(x, ...)
{
    i = SeasonMask(x, ...)
    return(c(Mean=mean(i, na.rm=TRUE), Median=median(i, na.rm=TRUE)))
}

SMVIStats = function(x, ...)
{
    i = SeasonMask(x, ...)
    return(c(mean=mean(i, na.rm=TRUE),
        median=median(i, na.rm=TRUE),
        p80=quantile(i, na.rm=TRUE, probs=0.80),
        p20=quantile(i, na.rm=TRUE, probs=0.20),
        IQR=IQR(i, na.rm=TRUE)
        ))
}

# Should be fast enough to not need parallelisation, but hey, why not
#registerDoParallel(cores=4)
#Covariates = foreach(Tile=iter(TileList), .combine=rbind, .inorder=FALSE, .multicombine=TRUE) %do%
#{
    NDVIMatrix = st_read(InputDB, "NDVI")
    NDVIMatrix = RunSFFunction(NDVIMatrix, ScaleNDVI, select=ColDates)
    
    # Get mean NDVI over the whole time series
    MeanNDVI = ApplySF(NDVIMatrix, mean, na.rm=TRUE, select=ColDates)
    NDVILow = ApplySF(NDVIMatrix, quantile, na.rm=TRUE, probs=0.25, select=ColDates)
    NDVIHigh = ApplySF(NDVIMatrix, quantile, na.rm=TRUE, probs=0.75, select=ColDates)
    NDVIIQR = ApplySF(NDVIMatrix, IQR, na.rm=TRUE, select=ColDates)
    
    # Get date of highest NDVI, that will be the one that we use for the other bands too
    # The function is which.max that on all NAs returns NA rather than integer(0)
    #MaxNDVIIdx = ApplySF(NDVIMatrix, function(x){a=which.max(x); return(ifelse(length(a)<1, NA, a))}, select=ColDates)
    
    RadiometryMatrices = list(Red=st_read(InputDB, "RADIOMETRY-1"),
        NIR = st_read(InputDB, "RADIOMETRY-2"),
        Blue = st_read(InputDB, "RADIOMETRY-3"),
        SWIR = st_read(InputDB, "RADIOMETRY-4"))
    
    
    for (season in seasons)
    {
        # Composite all four bands, mean and median of all three years
        RadiometryYear = lapply(RadiometryMatrices, ApplySF, SMMeanMedian, select=ColDates, type=season)
        gc(TRUE)
        # Combine into an sf again
        RadYearSF = Reduce(cbind, RadiometryYear)
        RadYearSF = RadYearSF / 2000 # Convert to real units
        colnames(RadYearSF) = paste(rep(names(RadiometryYear), each=2), colnames(RadYearSF), sep=".")
        RadYearSF = cbind(RadiometryMatrices$Red[,c("X", "Y")], RadYearSF)
        # Write the output
        st_write(RadYearSF, TempDB, season)
        rm(RadiometryYear, RadYearSF)
        gc(TRUE)
    }
    
    rm(RadiometryMatrices)
    gc(TRUE)
    XYs = st_read(InputDB, "RADIOMETRY-1", query="SELECT X, Y, geom FROM 'RADIOMETRY-1'")
    # Calculate VIs
    for (season in seasons)
    {
        SeasonSF = st_read(TempDB, season)
        for (statistic in c("Mean", "Median"))
        {
            RadiometryInputs = list(RED=SeasonSF[[paste0("Red.", statistic)]],
                    NIR=SeasonSF[[paste0("NIR.", statistic)]],
                    BLUE=SeasonSF[[paste0("Blue.", statistic)]],
                    SWIR=SeasonSF[[paste0("SWIR.", statistic)]])
            for (VI in c("NDMI", "OSAVI", "EVI", "NIRv"))
            {
                VI_stat = do.call(VI, RadiometryInputs)
                SeasonSF = cbind(SeasonSF, VI_stat)
                names(SeasonSF)[length(SeasonSF)-1] = paste(VI, statistic, sep=".")
            }
        }
        st_write(SeasonSF, VIDB, season, delete_layer=TRUE)
    }
    
# Calculate VIs first
RadiometryMatrices = list(RED=as.matrix(as.data.frame(st_read(InputDB, "RADIOMETRY-1"))[,ColDates]),
                        NIR = as.matrix(as.data.frame(st_read(InputDB, "RADIOMETRY-2"))[,ColDates]),
                        BLUE = as.matrix(as.data.frame(st_read(InputDB, "RADIOMETRY-3"))[,ColDates]),
                        SWIR = as.matrix(as.data.frame(st_read(InputDB, "RADIOMETRY-4"))[,ColDates]))

for (VI in VIs)
{
    VI_stat = do.call(VI, RadiometryMatrices)
    st_write(cbind(XYs, VI_stat), VIDB, VI)
}

rm(VI_stat)
gc(TRUE)

# Run covar calculation.

for (VI in VIs)
{
    VITSMatrix = as.matrix(as.data.frame(st_read(VIDB, VI))[,ColDates])
    for (season in seasons)
    {
        VICovars = t(pbapply(VITSMatrix, 1, SMVIStats, type=season))
        st_write(cbind(XYs, VICovars), TempDB, paste(VI, season, sep="."), delete_layer=TRUE)
    }
}
rm(VITSMatrix, VICovars)
gc(TRUE)

# Merge it all into one big table.

CompositesSF = NULL
for (VI in VIs)
{
    for (season in seasons)
    {
        ID = paste(VI, season, sep=".")
        VISF = st_read(TempDB, ID)
        VIDF = as.matrix(as.data.frame(VISF)[,!names(VISF) %in% c("X", "Y", "geom")])
        colnames(VIDF) = paste(ID, colnames(VIDF), sep=".")
        if (is.null(CompositesSF)) {
            CompositesSF = cbind(VISF[,c("X", "Y")], VIDF)
        } else {
            CompositesSF = cbind(CompositesSF, VIDF)
        }
    }
}
st_write(CompositesSF, OutputDB)

# Input: matrices of each band representing time series
# Output: 5 versions of each VI: median, summer median, winter median, autumn median, spring median
# First calculate all the VIs
# Then take max NDVI over each of the subsets
