library(raster)
library(rgdal)
# Merging CCI types:
# Crops: 10 (Cropland rainfed), 11 (Cropland rainfed - Herbaceous cover), 12 (Cropland rainfed - Tree or shrub cover),
#        20 (Cropland irrigated or post-flooding),
#        30 (Mosaic cropland (>50%) / natural vegetation (tree/shrub/herbaceous cover) (<50%))
# Deciduous trees: 60 (Tree cover  broadleaved  deciduous  closed to open (>15%)),
#                  61 (Tree cover  broadleaved  deciduous  closed (>40%)),
#                  62 (Tree cover  broadleaved  deciduous  open (15-40%)),
#                  80 (Tree cover  needleleaved  deciduous  closed to open (>15%)),
#                  81 (Tree cover  needleleaved  deciduous  closed (>40%)),
#                  82 (Tree cover  needleleaved  deciduous  open (15-40%)),
#                  90 (Tree cover  mixed leaf type (broadleaved and needleleaved))
# Evergreen trees: 50 (Tree cover broadleaved evergreen closed to open (>15%)),
#                  70 (Tree cover  needleleaved  evergreen  closed to open (>15%)),
#                  71 (Tree cover  needleleaved  evergreen  closed (>40%))
#                  72 (Tree cover  needleleaved  evergreen  open (15-40%))
# Shrublands: 100 (Mosaic tree and shrub (>50%) / herbaceous cover (<50%)), 120 (Shrubland), 121 (Shrubland evergreen),
#             122 (Shrubland deciduous)
# Grasslands: 130 (Grassland), 140 (Lichens and mosses), 110 (Mosaic herbaceous cover (>50%) / tree and shrub (<50%)),
#             40 (Mosaic natural vegetation (tree/shrub/herbaceous cover) (>50%) / cropland (<50%))
# Bare soil: 150 (Sparse vegetation (tree/shrub/herbaceous cover) (<15%)), 152 (Sparse shrub (<15%)),
#            153 (Sparse herbaceous cover (<15%)), 200 (Bare areas), 201 (Consolidated bare areas),
#            202 (Unconsolidated bare areas)
# Wetlands: 160 (Tree cover flooded fresh or brakish water), 170 (Tree cover flooded saline water),
#           180 (Shrub or herbaceous cover flooded fresh/saline/brakish water)
# Urban: 190 (Urban areas)
# Water: 210 (Water bodies)
# Permanent snow or ice: 220 (Permanent snow and ice)

# LC-CCI map, freely available
CCI = raster("../data/ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif")
# Boundaries of the area of interest (tile X20Y01)
AOI = readOGR("../data/aoi.shp", "aoi")
CCI.crop = crop(CCI, AOI)
# Vectors of 3: range min, range max, overwritten by what
reclassifier = c(
    10, 30, 10,
    60, 62, 60, 80, 90, 60,
    70, 72, 70, 50, 50, 70,
    120, 122, 120, 100, 100, 120,
    130, 140, 130, 40, 40, 130, 110, 110, 130,
    200, 202, 200, 150, 153, 200,
    160, 180, 160)
CCI.simple = reclassify(CCI.crop, reclassifier, right=NA)
writeRaster(CCI.simple, "../data/cci-simplified.tif")
# Grid params: ~20-~30 x, ~55-~65 y; 10/10080 steps
# One grid area is exactly 12196.1127540375
# Create stratified random sample of 1000 points (9 classes since there is no permafrost)
Samples = sampleStratified(CCI.simple, 112, sp=TRUE)
writeOGR(Samples, "samples.gml", "samples", driver="GML")
