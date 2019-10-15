#!/bin/bash
# Build VRTs for each tile, taking datadirs into account

# Handle datadirs: parse to a variable, it becomes space-separated
datadirs=$(awk -F\" 'NR>1 { print $2 }' ../data/DataDirs.csv)
echo $datadirs | xargs -I % echo find % -iname "*.tif"
