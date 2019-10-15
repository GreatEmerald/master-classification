# Download and extract WorldClim data
library(raster)
library(dismo)
library(sf)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/db-io.r")

# Temporary directory for storing the data before extraction
TempDir = "../../userdata/master-classification/climate/"
OutDir = "../data/pixel-based/climate/"

# Generate bioclimatic variables manually; for raster-based processing better to download,
# but in this case it's more efficient to generate
DSNames = c("tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr")

DownloadDir = file.path(TempDir, "raw")
if (!file.exists(DownloadDir))
    dir.create(DownloadDir)
if (!file.exists(OutDir))
    dir.create(OutDir)

URLs = paste0("http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_", DSNames, ".zip")
Filenames = paste0(DownloadDir, "/", DSNames, ".zip")
Filenames = Filenames[!file.exists(file.path(DownloadDir, basename(Filenames)))] # Filter already downloaded
if (length(Filenames) > 0)
    download.file(URLs, Filenames)

DSDirs = file.path(TempDir, DSNames)
DSStacks = list()
for (i in 1:length(DSNames))
{
    RasterFiles = list.files(DSDirs[i], pattern=glob2rx("*.tif"), full.names = TRUE)
    if (length(RasterFiles) <= 0)
    {
        unzip(Filenames[i], exdir=DSDirs[i])
        RasterFiles = list.files(DSDirs[i], pattern=glob2rx("*.tif"), full.names = TRUE)
    }
    DSStacks[[i]] = stack(RasterFiles)
}
names(DSStacks) = DSNames

# Extract our values
TrainingPoints = LoadGlobalTrainingData()
ValidationPoints = LoadGlobalValidationData()
PredictionPoints = LoadGlobalRasterPoints()
AllPoints = rbind(TrainingPoints[,c("x", "y")], ValidationPoints[,c("x", "y")], PredictionPoints[,c("x", "y")])

DSValues = list()
for (i in 1:length(DSNames))
{
    DSValues[[i]] = extract(DSStacks[[i]], AllPoints)
}
names(DSValues) = DSNames
BioValues = dismo::biovars(DSValues$prec, DSValues$tmin, DSValues$tmax)

BaseClimateData = do.call("cbind", DSValues)
AllClimateData = cbind.data.frame(X=AllPoints$x, Y=AllPoints$y, BaseClimateData, BioValues)

SpatialClimate = DFtoSF(AllClimateData)
rm(AllClimateData)
st_write(SpatialClimate, file.path(OutDir, "climate.gpkg"))
