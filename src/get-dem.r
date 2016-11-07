# Download and preprocess GLSDEM

# URLs: ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/n060/GLSDEM_n060e025/GLSDEM_n060e025.tif.gz
library(R.utils)
library(landsat)

TilesN = 55:65
TilesE = 20:30

dems = list()
for(n in TilesN)
{
    for(e in TilesE)
    {
        filename = paste0("GLSDEM_n0", n, "e0", e, ".tif")
        url = paste0("ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/n0", n,
            "/GLSDEM_n0", n, "e0", e, "/", filename, ".gz")
        outfile = paste0("../data/dem/", filename)
        outfilegz = paste0(outfile, ".gz")
        if(!file.exists(outfilegz) && !file.exists(outfile))
            download.file(url, outfilegz, "wget")
        if(file.exists(outfilegz) && file.size(outfilegz) > 0)
            gunzip(paste0(outfile, ".gz"))
        if(file.exists(outfile) && file.size(outfile) > 0)
            dems = list(dems, raster(outfile))
    }
}
dems = unlist(dems)

# Mosaic; raster is silly and does not accept lists, so use a wrapper
setMethod('mosaic', signature(x='list', y='missing'), 
function(x, y, fun, tolerance=0.05, filename="", overwrite=FALSE){
    stopifnot(missing(y))
    args <- x
    if (!missing(fun)) args$fun <- fun
    if (!missing(tolerance)) args$tolerance<- tolerance
    if (!missing(filename)) args$filename<- filename
    if (!missing(overwrite)) args$overwrite<- overwrite
    do.call(mosaic, args)
})

DEMMosaic = mosaic(dems, fun=mean, filename="../data/dem/mosaic.grd", overwrite=TRUE, tolerance=0.5)
# USGS doesn't provide rasters for pure sea since it's 0 by definition, so fill it
#DEMMosaic[is.na(DEMMosaic)] = 0
DEMMosaic = reclassify(DEMMosaic, cbind(NA, 0))
writeRaster(DEMMosaic, filename="../data/dem/mosaic.tif", overwrite=TRUE)
DEMMosaic = writeRaster(DEMMosaic, filename="../data/dem/mosaic.grd", overwrite=TRUE)

# Resample the DEM to the PROBA-V pixels (the DEM is slightly more detailed)
PVExample = raster("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/20160711/PROBAV_S5_TOC_20160711_100M_V001/PROBAV_S5_TOC_X20Y01_20160711_100M_V001_NDVI.tif")
DEMAligned = raster::resample(DEMMosaic, PVExample, method="bilinear", filename="../data/dem/dem.grd")
writeRaster(DEMAligned, datatype="INT2S", filename="../data/dem/dem.tif")

# Calculate aspect and slope
DEMStats = terrain(DEMAligned, c("slope", "aspect", "TRI"), neighbors=4, filename="../data/dem/slopeaspect.grd")
writeRaster(DEMStats, filename="../data/dem/slopeaspect.tif")

# R3241-R5414
# L2333-K4421-K5442
# K-R 2-5
# Filelist obtained via bash script:
# rsync -arPv rsync://tiedostot.kartat.kapsi.fi/mml/korkeusmalli/hila_10m/etrs-tm35fin-n2000 | grep \.tif | awk '{print $5}' | grep -P "[K-R]" > filelist.txt

