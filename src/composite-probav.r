# PROBA-V images are often cloudy, to the point that no one 5-day composite is ever cloud-free for a single tile.
# Thus we need to composite images manually, starting with the 1-day composites.
# Generate composites dynamically by merging images one by one. That allows inspecting intermediate results.
# Start with the composite in the middle of summer (July 15), then go +1 day, -2 days, +3 days etc. until we get 100% coverage or hit the end of summer.
# First, bad quality pixels need to be discarded. There should be no ice in summer (even in the middle of Finland), so any such "ice" is actually a cloud.
# Water is a class, keep it. It may be useful as an additional variable.
# Thus discard anything cloudy/shadowed/ice (assert b0-2 == 000), and bad quality (assert b4-7 == 1)
# In dec the good pixels are 240 (water) and 248 (land)
# Some pixels may be covered by not all bands. If there is land cover change between dates, that might result in weird results, so also discard partial pixels like that.
# And the cloud detection algorithm used by PROBA-V is not optimal.
# Thus either get the highest NDVI, or also remove outliers from a LOESS function, or calculate distance from nearest cloud.
# Dealing with bit flags is not something humans should do. So instead have a function that converts a data frame with readable names to bit flags internally.

# Get PROBA-V processing package by JE
if (!("probaV" %in% installed.packages()[,"Package"]))
{
    if (!("devtools" %in% installed.packages()[,"Package"]))
    {
        install.packages("devtools")
    }
    library(devtools)
    options(unzip = 'internal')
    if (!("lubridate" %in% installed.packages()[,"Package"]))
    {
        install.packages("lubridate")
    }
    install_github("johanez/probaV")
}

library(probaV)

getProbaVinfo("/home/dainius/Documents/master-classification/data/", pattern=glob2rx("PROBAV*.tif"))
