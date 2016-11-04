# Download and preprocess GLSDEM

# URLs: ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/n060/GLSDEM_n060e025/GLSDEM_n060e025.tif.gz
library(R.utils)

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
writeRaster(DEMMosaic, filename="../data/dem/mosaic.grd", overwrite=TRUE)
