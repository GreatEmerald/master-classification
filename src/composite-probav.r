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
    install_github("johanez/probaV", quick=TRUE)
}

library(probaV)

TileOfInterest = "X20Y01"
getProbaVinfo("/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M/20160711/PROBAV_S5_TOC_20160711_100M_V001/", pattern=glob2rx("PROBAV*X20Y01*.tif"))
head(getProbaVQClist()$all_bits)

# True means must have, False means must not have, NA means don't care
GetProbaVQCMask = function(bluegood = NA, redgood = NA, nirgood = NA, swirgood = NA, land = NA, ice = NA, cloud = NA, shadow = NA)
{
  bluegoodbit = bitwShiftL(1, 7)
  redgoodbit = bitwShiftL(1, 6)
  nirgoodbit = bitwShiftL(1, 5)
  swirgoodbit = bitwShiftL(1, 4)
  landbit = bitwShiftL(1, 3)
  icebit = bitwShiftL(1, 2)
  cloudbit = bitwShiftL(1, 1)
  shadowbit = bitwShiftL(1, 0)
  
  # If True, add the bit, if False don't, if NA keep original and also add it
  BitIterator = function(input, state, bit)
  {
    if(is.na(state))
      input = c(input, input + bit)
    else if(state)
      input = input + bit
    return(input)
  }
  
  result = 0L
  result = BitIterator(result, bluegood, bluegoodbit)
  result = BitIterator(result, redgood, redgoodbit)
  result = BitIterator(result, nirgood, nirgoodbit)
  result = BitIterator(result, swirgood, swirgoodbit)
  result = BitIterator(result, land, landbit)
  result = BitIterator(result, ice, icebit)
  result = BitIterator(result, cloud, cloudbit)
  result = BitIterator(result, shadow, shadowbit)
  
  return(result)
}

QC.vals = GetProbaVQCMask(bluegood=TRUE, redgood=TRUE, nirgood=TRUE, swirgood=TRUE, ice=FALSE, cloud=FALSE, shadow=FALSE)

cleanProbaV(f_data = , filename=filename, QC_val = QC.vals, fill=255, datatype="FLT4S", as.is = F, overwrite = T)
