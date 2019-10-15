# Process ASTER DEM (replacement for GLSDEM since that is no longer available)
# The ASTER files have been downloaded from NASA Earthdata Search
# Input: zip files at 30 m resolution
# Output: ASTER DEM resampled to Proba-V grid
# Run the bash file to get global mosaic in the Proba-V grid of elevation, slope, aspect, roughness, TPI

library(gdalUtils)
library(raster)
library(foreach)
library(doSNOW)

source("pixel-based/utils/ProbaVTileID.r")

InputDir = "/data/mep_cg1/ancillary/DEM/aster/geotiff/dem/"
OutputDir = "/userdata2/master-classification/dem/aster-covars/"
ProjectionReference = "../work/globalmosaic.vrt"
SampleDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/2019/20190101/PROBAV_S5_TOC_20190101_100M_V101/"

# Get a projection sample: a mosaic of Proba-V imagery
if (!file.exists(ProjectionReference))
{
    SampleFiles = list.files(SampleDir, pattern=glob2rx("*NDVI.tif"), full.names = TRUE)
    gdalbuildvrt(SampleFiles, ProjectionReference)
}
ReferenceRaster = raster(ProjectionReference)

if (!dir.exists(OutputDir))
    dir.create(OutputDir)

AsterTifs = list.files(InputDir, glob2rx("*.tif"), full.names = TRUE)

cl <- makeCluster(2, outfile="")
registerDoSNOW(cl)
pb = txtProgressBar(max = length(AsterTifs), style = 3)

# Iterate over all zip files
foreach (AsterTif=AsterTifs, .packages=c("raster"),
         .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar%
{
    #AsterTif = sub(".zip", "_dem.tif", AsterZip)
    OutFile = file.path(OutputDir, basename(AsterTif))
    if (file.exists(OutFile))
    {
        print(paste(OutFile, "exists, not processing"))
        return(NULL)
    }
    
    # Unzip just the file we need
    #unzip(AsterZip, files = basename(AsterTif), exdir=dirname(AsterZip))
    #if (!file.exists(AsterTif))
    #    stop(paste("Could not extract file from", AsterZip))
    #print(paste("Reading from", AsterTif))
    AsterDem = raster(AsterTif)
    AsterDem = readAll(AsterDem)
    # Resample to 100 m
    # The result is always the size of the reference, so crop refrence first
    ReferenceRegion = tryCatch(
        crop(ReferenceRaster, AsterDem, snap="out"),
        error=function(e){
                if (e$message == "Failure during raster IO\n")
                {
                    ADE = extent(AsterDem)
                    TID = ProbaVTileID(data.frame(x=mean(ADE@xmin, ADE@xmax),y=mean(ADE@ymin, ADE@ymax)))
                    RefTile = raster(list.files(SampleDir, pattern=glob2rx(paste0("*", TID, "*NDVI.tif")), full.names = TRUE))
                    crop(RefTile, AsterDem, snap="out")
                } else stop(e)
            }
        )
    #print(paste("Writing to", OutFile))
    raster::resample(AsterDem, ReferenceRegion, datatype="INT2S", filename=OutFile,
                     options=c("COMPRESS=DEFLATE", "ZLEVEL=9"))
    rm(ReferenceRegion)
    # Delete original
    #file.remove(AsterTif) #, AsterZip - no permission
}

close(pb)
stopCluster(cl) 
