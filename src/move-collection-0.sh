#!/bin/bash
# Script for moving collection 0 files that have been reprocessed out to another directory
# First argument is search term including VXXX, like V101.tif or V101_NDVI_sm.tif
for file in *$1; do
    mv $(sed -n "/$1/s/$1//p" <<<$file)V00* collection-0
done
