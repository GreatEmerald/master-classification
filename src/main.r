## Main file of the processing chain
## Mostly for documentation purposes; it takes way too long to actually run the whole thing.
## Instead, run the files mentioned manually. Many of them run in the terminal autonomously.

## Preprocessing ##

## Optical: preprocess PROBA-V data into a variety of indices

# Mask out clouds using built-in QC layer
source("optical/clean-builtin.r")
# Mask out clouds even more using the time series method
source("optical/clean-timeseries.r")

# Get a summer 2016 composite via maximum NDVI
source("optical/composite-probav.r")
# Calculate LSWI and OSAVI out of it, add a water mask, and merge all together
source("optical/calc-indices.r")

# Calculate harmonic parameters of NDVI
source("optical/get-harmonics.r")

## DEM: preprocess GLSDEM and EUDEM into a variety of indices

# Download and preprocess GLSDEM
source("elevation/get-dem.r")
# Preprocess EUDEM
source("elevation/process-eudem.r")
# Merge the two together
source("elevation/dem-merge.r")

## Validation data: preprocess and merge into a coherent file with everything

# Preprocess CCI land cover (for reference when gathering data only!)
source("validation/simplify-cci.r")
# Read validation data CSV and convert into SpatialPointsDataFrame
source("validation/process-csv.r")
# Merge everything together
source("validation/merge-data.r")

## Classification: make use of coherent files to train and predict classification models

# Fuzzy c-means
source("classification/classify-cmeans.r")
# Neural networks
source("classification/classify-nn.r")
# Gradient boosting
source("classification/classify-mgb.r")
# Random forest
source("classification/classify-rf.r")
