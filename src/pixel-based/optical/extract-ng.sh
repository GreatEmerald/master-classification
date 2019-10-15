#!/bin/bash
# Run gdallocationinfo -xml to get all points per tile
export GDAL_CACHEMAX=4000
csv_dir=../work/extract-from/
vrt_tiles=../work/raw-vrts/*.vrt
outdir=../../userdata/master-classification/work/extracted-points/

mkdir -p $outdir

# Match raster filenames with tiles
for file in $vrt_tiles
do
    # Figure out which tile this is
    tile=$(echo $file | awk -F'/' '{print $NF}' | awk -F"-" '{print $1}')
    outname=$(basename $file .vrt)
    # Does it exist already?
    outpath=$outdir$outname.xml
    if [[ -f "$outpath" ]]; then
        echo File $outpath already exists, skipping
        continue
    fi
    echo gdallocationinfo -wgs84 -xml $file '<' $csv_dir$tile.csv  '>' $outpath
    gdallocationinfo -wgs84 -xml $file < $csv_dir$tile.csv  > $outpath
done
