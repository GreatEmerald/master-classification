# Remove NA values from all rasters, using different techniques:
# For the summer composite, use extra pixels from the Collection 0 composite.
# For all other rasters, apply a weighted average filter

library(raster); source("utils/set-temp-path.r")
library(foreach)
library(doParallel)
library(iterators)
library(tools)

OutputDir = "../../userdata/indices-no-na/"
RasterOptions = c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4")

NewCompositeFile = "../../userdata/indices/composite.tif"
OldCompositeFile = "../../userdata/composite/composite.tif"
NewerCompositeFile = "../../userdata/indices/composite-merged.tif"

NewComposite = brick(NewCompositeFile)
OldComposite = brick(OldCompositeFile)

#cellStats(is.na(OldComposite[[1]]), "sum")
if (!file.exists(NewerCompositeFile))
    NewerComposite = merge(NewComposite, OldComposite,
        progress="text", filename=NewerCompositeFile, options=RasterOptions)

# Now fill out NAs using a focal filter
# The weights are approximate inverse distances
Weights = matrix(c(1/124, 1/55, 1/124, 1/111, 0, 1/111),3,3)
Weights = Weights / sum(Weights)

fill.na = function(x, i=5)
{
    if (is.na(x)[i])
        return(weighted.mean(x, Weights, na.rm=TRUE))
    return(x[i])
}

FillRasterNAs = function(InputFile, OutputFile, RasterOptions = c("COMPRESS=DEFLATE", "ZLEVEL=9", "NUMTHREADS=4"))
{
    if (file.exists(OutputFile))
    {
        message(paste("File", OutputFile, "already exists, skipping."))
        return()
    }
    InputRaster = brick(InputFile)
    InputType = dataType(InputRaster)
    
    NumLayers = nlayers(InputRaster)
    registerDoParallel(cores = NumLayers)
    psnice(value = min(NumLayers - 1, 19))
    OutputStack = foreach(i=iter(names(InputRaster)), .packages="raster", .verbose=TRUE, .inorder=TRUE,
        .combine=stack) %dopar%
    {
        Output = focal(InputRaster[[i]], w=matrix(1,3,3), fun=fill.na, pad=TRUE, na.rm=FALSE,
            datatype=InputType, progress="text")
        
        message(paste("File", InputFile, "layer", i, "processed, remaining NAs:",
            cellStats(is.na(Output), "sum")))
        
        Output
    }
    writeRaster(OutputStack, filename=OutputFile, options=RasterOptions, datatype=InputType, progress="text")
    message(paste("Output written to", OutputFile))
}

FillRasterNAs(NewerCompositeFile, paste0(OutputDir, "composite.tif"))
FillRasterNAs("../../userdata/dem/merged/pv-height.tif", paste0(OutputDir, "pv-height.tif"))
FillRasterNAs("../../userdata/dem/merged/pv-slope.tif", paste0(OutputDir, "pv-slope.tif"))
FillRasterNAs("../../userdata/dem/merged/pv-aspect.tif", paste0(OutputDir, "pv-aspect.tif"))
FillRasterNAs("../../userdata/dem/merged/pv-tpi.tif", paste0(OutputDir, "pv-tpi.tif"))
FillRasterNAs("../../userdata/harmonics/phase-amplitude.tif", paste0(OutputDir, "phase-amplitude.tif"))

# DEM ones that use focal have edge effects, so have to process twice
FillRasterNAs(paste0(OutputDir, "pv-aspect.tif"), paste0(OutputDir, "pv-aspect-2.tif"))
FillRasterNAs(paste0(OutputDir, "pv-tpi.tif"), paste0(OutputDir, "pv-tpi-2.tif"))
# And harmonics require a lot of data, so also large spaces with NAs (in clumps)
FillRasterNAs(paste0(OutputDir, "phase-amplitude.tif"), paste0(OutputDir, "phase-amplitude-2.tif"))
FillRasterNAs(paste0(OutputDir, "phase-amplitude-2.tif"), paste0(OutputDir, "phase-amplitude-3.tif"))
FillRasterNAs(paste0(OutputDir, "phase-amplitude-3.tif"), paste0(OutputDir, "phase-amplitude-4.tif"))
FillRasterNAs(paste0(OutputDir, "phase-amplitude-4.tif"), paste0(OutputDir, "phase-amplitude-5.tif"))
FillRasterNAs(paste0(OutputDir, "phase-amplitude-5.tif"), paste0(OutputDir, "phase-amplitude-6.tif"))
