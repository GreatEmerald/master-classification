# Utility functions for the raster package. For sourcing, not calling.

# Mosaic; raster is silly and does not accept lists, so use a wrapper
setMethod('mosaic', signature(x='list', y='missing'), 
function(x, y, fun, tolerance=0.05, filename="", overwrite=FALSE){
    stopifnot(missing(y))
    args <- x
    if (!missing(fun)) args$fun <- fun
    if (!missing(tolerance)) args$tolerance<- tolerance
    if (!missing(filename)) args$filename<- filename
    if (!missing(overwrite)) args$overwrite<- overwrite
    do.call(mosaic, args)
})
