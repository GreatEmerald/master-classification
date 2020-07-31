This directory contains scripts that are optimised for running computations on training/validation pixels only. Instead of processing entire tiles, we process individual pixels out of tiles. The other scripts are for processing entire tiles.

The scripts should be run in a particular order, according to the processing chain. First, all data needs to be acquired:

* `optical`:
  * `make-timeseries-vrts.r` to create VRTs for PROBA-V data
  * `extract-ng.sh` to extract time series over points in XML format
  * `stack-timeseries-grep.r` to parse the XMLs and produce geodatabases
  * `mask-clouds.r` to apply the status mask and filter out all cloudy pixels
  * `temporal-filter.r` to perform additional temporal cleaning
  * `composite.r` to generate VIs and their seasonal information
  * `get-harmonics.r` to generate temporal features
* `elevation`:
  * `process-aster.r` to resample ASTER data to PROBA-V resolution
  * `calc-covars.sh` to calculate terrain parameters out of elevation
  * `ingest-aster.r` to extract the data at required points from the raster
* `soil`:
  * 
