# Clean images using the time series method
library(probaV)
source("utils/set-temp-path.r")
source("../../cloud_filter.r") # JD's extra utility script

IntermediaryDir = "../../userdata/composite/radiometry/"
OutputDir = "../../userdata/composite/tscleaned/"
TileOfInterest = "X20Y01"

# Subset for testing
xmin <- 27
xmax <- 28
ymin <- 58
ymax <- 59

# select bands and create virtual name output
bands_select <- '(BLUE)' 
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")
vrt_name <- file.path(paste0(OutputDir,"/",TileOfInterest, "_",paste0(bands_select, collapse = "_"), ".vrt"))

# Select all dates of the blue band
b_vrt <- timeVrtProbaV(IntermediaryDir, pattern = bands_sel, vrt_name = vrt_name, tile = TileOfInterest,
    return_raster = T,
    #start_date = "2015-10-21", end_date = "2016-03-01",
    te = c(xmin, ymin, xmax, ymax))

# create output folder and name
out_name <- paste0(OutputDir, 'cloud_filter.envi')
logfile <- paste0(OutputDir, 'cloud_filter.log')

bands_select <- '(BLUE)' 
bands_sel <- paste(bands_select,'_sm.tif$', sep = "")

cloud_filter(x = b_vrt, probav_sm_dir = IntermediaryDir, pattern = bands_sel,
                          tiles = TileOfInterest, minrows = 15, mc.cores = 16,
                          logfile=logfile, overwrite=TRUE, span=0.3, 
                          cf_bands = c(1), thresholds=c(-80, Inf), 
                          filename = out_name)

blue_c_filter <- brick(out_name)

# select date
blue_c_filter <- subset(blue_c_filter ,1)
