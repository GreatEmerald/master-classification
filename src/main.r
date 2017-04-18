## Main file of the processing chain
## Mostly for documentation purposes; it takes way too long to actually run the whole thing.
## Instead, run the files mentioned manually. Many of them run in the terminal autonomously.

## Preprocessing ##

## Optical: preprocess PROBA-V data into a variety of indices

# Mask out clouds using built-in QC layer
source("optical/clean-builtin.r")

# Mask out clouds from NDVI even more using the time series method: produce a mask file
source("optical/clean-timeseries.r")
# Apply the mask file to get a directory of cleaned NDVI layers
source("optical/apply-timeseries-mask.r")

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

# Remove NAs because the algorithms cannot handle them
source("validation/remove-nas.r")
# Preprocess CCI land cover (for reference when gathering data only!)
source("validation/simplify-cci.r")
# Check consistency of validation data CSV and convert into SpatialPointsDataFrame
source("validation/process-csv.r")
# Merge everything together into a RasterStack/Brick to get the training/validation dataset with all covars
source("validation/merge-data.r")

## Classification: make use of coherent files to train and predict classification models

# Calculate error statistics and perform model optimisations:
# Fuzzy c-means
source("classification/classify-cmeans.r")
# Neural networks
source("classification/classify-nn.r")
# Gradient boosting
source("classification/classify-mgb.r")
# Random forest
source("classification/classify-rf.r")

# Perform full-tile predictions using optimised models:
# Control
source("classification/predict-dummy.r")
# Fuzzy c-means
source("classification/predict-cmeans.r")
# Neural networks
source("classification/predict-nn.r")
# Gradient boosting
source("classification/predict-mgb.r")
# Random forest
source("classification/predict-rf.r")
