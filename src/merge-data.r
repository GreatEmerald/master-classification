# Merge the original validation data with extracted parameters for model input
library(raster); source("utils/set-temp-path.r")
source("utils/load-data.r")

# Original data
samples = read.csv("../data/samples.csv")
coordinates(samples) = ~X+Y
row.names(samples) = samples$pkuid
samples$pkuid = NULL
projection(samples) = "+proj=longlat +datum=WGS84 +no_defs"
str(samples)

# Add "hardened" column: dominant land cover class
samples$dominant = factor(apply(samples@data, 1, which.max), labels = colnames(samples@data)[1:9], ordered = FALSE)
# Add a column that identifies endmembers
samples$pure = apply(samples@data[,1:9], 1, max) >= 95

# Extract data from rasters
rasters = stack(TrainingFiles)
rasters$composite.1 = rasters$composite.1 / 2000
rasters$composite.2 = rasters$composite.2 / 2000
rasters$composite.3 = rasters$composite.3 / 2000
rasters$composite.4 = rasters$composite.4 / 2000
data = extract(rasters, samples, method="simple", cellnumbers=TRUE, sp=TRUE)

# Give nice names and units
names(data) = c(names(samples), "cell.no", TrainingNames)

# Save it
write.csv(data, "../data/variables.csv")

# Get a reduced raster for SpatialPixelsDataFrame
pixels = raster::mask(rasters, samples, filename="../../userdata/pixels.tif", overwrite=TRUE)

# Data exploration
#cor(data@data[names(data) != "dominant"])
# Correlations are logical. OSAVI and NDVI are too correlated, and red and blue are quite close too.
#hist(data@data[,"water"]) # Base data is often zero-inflated
#hist(data@data[,"cell.no"]) # Not biased
#hist(data@data[,"red"]) # Lognormal
#hist(data@data[,"nir"]) # Bimodal normal
#hist(data@data[,"swir"]) # Bimodal normal
#hist(data@data[,"ndvi"]) # Reversed lognormal
#hist(data@data[,"lswi"]) # Normal
#hist(data@data[,"height"]) # Zero-inflated normal
#hist(data@data[,"slope"]) # Lognormal
#hist(data@data[,"aspect"]) # Pi-inflated uniform
#hist(data@data[,"tpi"]) # Normal
