This directory contains scripts that are optimised for running computations on training/validation pixels only. Instead of processing entire tiles, we process individual pixels out of tiles. The other scripts are for processing entire tiles.

The scripts should be run in a particular order, according to the processing chain. First, all data needs to be acquired, in any order but from start to finish for each type:

* `optical`:
  1. `make-timeseries-vrts.r` to create VRTs for PROBA-V data
  1. `extract-ng.sh` to extract time series over points in XML format
  1. `stack-timeseries-grep.r` to parse the XMLs and produce geopackages
  1. `mask-clouds.r` to apply the status mask and filter out all cloudy pixels
  1. `temporal-filter.r` to perform additional temporal cleaning
  1. `composite.r` to generate VIs and their seasonal information
  1. `get-harmonics.r` to generate temporal features
* `elevation`:
  1. `process-aster.r` to resample ASTER data to PROBA-V resolution
  1. `calc-covars.sh` to calculate terrain parameters out of elevation
  1. `ingest-aster.r` to extract the data at required points from the raster
* `soil`:
  1. `get-soilgrids.r` to download data at point locations via SoilGrids API and save to geopackage
  1. `choose-horizon.r` is an optional exploratory step to choose the soil horizon to retain in variable selection
* `climate`:
  1. `get-worldclim.r` to download WorldClim data and extract to geopackage
  1. `add-biovars.r` to add additional computed bioclimatic parameters to the geopackage

Next, run `pre-classification/merge-databases.r` to perform variable selection and merge all the covariates into one final GeoPackage. Then, run the models in any order from the `classification` directory.

Finally, `post-classification` has the code to make visualisations of the data:

* `predict-visualisation.r` for making the wall-to-wall global fraction map and graphs for model comparison
* `spatial-errors.r` for spatial uncertainty maps of the best model using the validation data
* `variable-importance.r` for heatmaps showing variable importance

Files in subdirectories named `old` are previous code that was not used in the end, but may still be useful for reference. All code expects the working directory to be set to `src`, not this directory.
