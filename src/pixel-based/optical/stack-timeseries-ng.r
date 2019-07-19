# Next-gen setup for data retrieval:
# 1) Run get-point-coords.r to generate a space-delimited CSV with all coordinates of points
# 2) Run make-timeseries-vrts.r to generate VRTs per tile per band
# 3) Run extract-ng.sh to get an XML of time series values per band per tile.
#    This runs on *all* points regardless of source for the sake of efficiency (reading tiles only once)
# 4) Run this script to ingest the information into one large GeoPackage.
# 5) Proceed with the analysis. The dataset needs to be split into training, validation and prediction.
