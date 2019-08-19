#!/bin/bash
# Compute DEM covariates out of elevation
indir="/userdata2/master-classification/dem/aster-covars"
outdir=$indir

# Mosaic all
gdalbuildvrt ${outdir}/elevation.vrt ${indir}/*.tif || exit 1

# Calc DEM params
gdaldem slope ${outdir}/elevation.vrt ${outdir}/slope.tif -s 111120 \
    -co COMPRESS=DEFLATE -co BIGTIFF=YES -co ZLEVEL=9 -co NUM_THREADS=3 || exit 1

gdaldem roughness ${outdir}/elevation.vrt ${outdir}/roughness.tif \
    -co COMPRESS=DEFLATE -co BIGTIFF=YES -co ZLEVEL=9 -co NUM_THREADS=3 || exit 1

gdaldem TPI ${outdir}/elevation.vrt ${outdir}/tpi.tif \
    -co COMPRESS=DEFLATE -co BIGTIFF=YES -co ZLEVEL=9 -co NUM_THREADS=3 || exit 1

gdaldem aspect ${outdir}/elevation.vrt ${outdir}/aspect.tif \
    -co COMPRESS=DEFLATE -co BIGTIFF=YES -co ZLEVEL=9 -co NUM_THREADS=3 || exit 1
