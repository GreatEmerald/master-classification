# Script for optimising the extraction time of global data
# Depends on the VRTs created in stack-timeseries-global.r

## Test how long it takes to extract a single pixel, from a VRT vs locally
library(microbenchmark)
library(raster)
library(gdalUtils)
library(velox)
source("pixel-based/utils/ProbaVTileID.r")
#source("pixel-based/utils/ProbaVDataDirs.r")

TestPoints = data.frame(x=c(-8.397321, -7.969742, -7.378472, -6.831845, -6.609623),
                        y=c(12.15724, 12.06994, 11.71379, 12.96081, 13.15427))
RegularPoints = expand.grid(x=seq(-10, 0, 0.2), y=seq(5, 15, 0.2))
ProbaVTileID(TestPoints) # All X17Y06

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
BenchmarkDir = "../../userdata/master-classification/pixel-based/benchmark"
VRTDir = "../../userdata/master-classification/pixel-based/vrt/raster/"
DataDirCache = "../data/DataDirs.csv"
DataDirs = read.csv(DataDirCache, stringsAsFactors = FALSE)

dir.create(BenchmarkDir)
VIRaster = brick(file.path(VRTDir, "NDVI.vrt"))
TileRaster = brick(file.path(VRTDir, "NDVI.vrt"))

# Extract from a megaVRT
system.time(extract(VIRaster, TestPoints[1,])) # 11.7 1.3 61.1
system.time(extract(VIRaster, TestPoints))
# velox
MegaVelox = velox(file.path(VRTDir, "NDVI.vrt")) # fails because it tries to load 200 Gib into RAM

# Extract from a webFS VRT of a single tile
# Find all timesteps of the given tile
X17Y06Files = list.files(DataDirs$dir, glob2rx("PROBAV_S5_TOC_X17Y06_????????_100M_V???_NDVI.tif"), full.names = TRUE)

# Stack and extract
X17Y06Stack = stack(X17Y06Files)
system.time(extract(X17Y06Stack, TestPoints[1,])) # 70.158   1.834 127.800

# VRT, brick and extract
gdalbuildvrt(X17Y06Files, file.path(BenchmarkDir, "X17Y06.vrt"), separate = TRUE)
X17Y06Brick = brick(file.path(BenchmarkDir, "X17Y06.vrt"))
system.time(extract(X17Y06Brick, TestPoints[1,])) # 7.647   1.282  29.469
system.time(extract(X17Y06Brick, TestPoints)) # 34.922   4.249 156.662, exactly 5 times longer
# This is already with the correct block side in the VRT, but gdalinfo shows it as 128x128

# Download, stack and extract
file.copy(X17Y06Files, BenchmarkDir)
X17Y06Local = list.files(BenchmarkDir, glob2rx("*.tif"), full.names = TRUE)
X17Y06LocalS = stack(X17Y06Local)
system.time(extract(X17Y06LocalS, TestPoints[1,])) # 96.752   0.590 109.207

gdalbuildvrt(X17Y06Local, file.path(BenchmarkDir, "X17Y06Local.vrt"), separate = TRUE)
X17Y06LocalB = brick(file.path(BenchmarkDir, "X17Y06Local.vrt"))
system.time(extract(X17Y06LocalB, TestPoints[1,])) # 7.953   1.474  32.225

# velox
TileVelox = velox(file.path(BenchmarkDir, "X17Y06.vrt")) # still fails

# Brick to tmp and extract
rasterOptions(tmpdir=BenchmarkDir)
X17Y06Brick = brick(X17Y06Stack) # Fails
system.time(extract(X17Y06Brick, TestPoints[1,]))

# VRT a chunk of 10080x256, which means 40 chunks in total
gdalbuildvrt(X17Y06Files, file.path(BenchmarkDir, "X17Y06Chunk.vrt"), separate = TRUE,
             te=c(extent(X17Y06Brick)@xmin, extent(X17Y06Brick)@ymin+7, extent(X17Y06Brick)@xmax, extent(X17Y06Brick)@ymin+7.25))
X17Y06Chunk = brick(file.path(BenchmarkDir, "X17Y06Chunk.vrt"))
system.time(extract(X17Y06Chunk, TestPoints[1,])) # 7.241   1.311  31.638
system.time(extract(X17Y06Chunk, TestPoints)) # 13.492   1.432  28.359
system.time(extract(X17Y06Chunk, RegularPoints)) # 7.405   1.000  27.659; this extracts 50 points at once!

# How about two chunks
gdalbuildvrt(X17Y06Files, file.path(BenchmarkDir, "X17Y06Chunk2.vrt"), separate = TRUE,
             te=c(extent(X17Y06Brick)@xmin, extent(X17Y06Brick)@ymin+7, extent(X17Y06Brick)@xmax, extent(X17Y06Brick)@ymin+7.5))
X17Y06Chunk2 = brick(file.path(BenchmarkDir, "X17Y06Chunk2.vrt"))
system.time(extract(X17Y06Chunk2, RegularPoints)) # 15.167   2.981 766.843 - makes no sense
