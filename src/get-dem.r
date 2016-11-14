# Download and preprocess GLSDEM

# URLs: ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/n060/GLSDEM_n060e025/GLSDEM_n060e025.tif.gz
library(R.utils)
library(landsat)
source("utils/raster-utils.r")
source("utils/dem-statistics.r")

GLSDir = "../../userdata/dem/glsdem"
TilesN = 55:61
TilesE = 28:29

dems = list()
for(n in TilesN)
{
    for(e in TilesE)
    {
        filename = paste0("GLSDEM_n0", n, "e0", e, ".tif")
        url = paste0("ftp://ftp.glcf.umd.edu/glcf/GLSDEM/Degree_tiles/n0", n,
            "/GLSDEM_n0", n, "e0", e, "/", filename, ".gz")
        outfile = paste0(GLSDir, "/", filename)
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

GLSMosaic = mosaic(dems, fun=mean, filename=paste0(GLSDir, "/mosaic.grd"), overwrite=TRUE, tolerance=0.5)
# USGS doesn't provide rasters for pure sea since it's 0 by definition, so fill it if needed
#DEMMosaic[is.na(DEMMosaic)] = 0
#GLSMosaic = reclassify(GLSMosaic, cbind(NA, 0))
#writeRaster(GLSMosaic, filename=paste0(GLSDir,"/mosaic.tif"), overwrite=TRUE)
PVExample = raster("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/20160711/PROBAV_S5_TOC_20160711_100M_V001/PROBAV_S5_TOC_X20Y01_20160711_100M_V001_NDVI.tif")
CalculateDEMStatistics(GLSMosaic, GLSDir, PVExample)

# R3241-R5414
# L2333-K4421-K5442
# K-R 2-5
# Filelist obtained via bash script:
# rsync -arPv rsync://tiedostot.kartat.kapsi.fi/mml/korkeusmalli/hila_10m/etrs-tm35fin-n2000 | grep \.tif | awk '{print $5}' | grep -P "[K-R]" > filelist.txt

