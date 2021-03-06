library(sf)
library(pbapply)

# Find whether a given point (df, with X and Y attributes) is already in the dataset (CurrentDataset)
FindSpatialDuplicates = function(df, CurrentDataset, threshold = 0) {
    if (is.null(CurrentDataset))
        return(0) # No data, we're sure it's not duplicated
    
    Distances = abs(CurrentDataset$X-df[["X"]])+abs(CurrentDataset$Y-df[["Y"]])
    Duplicates = sum(Distances<=threshold) # Adjust fuzz threshold if necessary
    
    if (is.null(Duplicates))
        stop(paste0("Unable to calculate distances from point ", X, ",", Y))
    if (!is.finite(Duplicates))
    {
        print(CurrentDataset[is.na(CurrentDataset$X) | is.na(CurrentDataset$Y), c("X", "Y")])
        stop(paste0("Number of duplicates is ", Duplicates, ", check whether input makes sense. Target:", X, ",", Y))
    }
    return(Duplicates)
}

# Get a GPKG-compatible SF object from a data.frame with X and Y attributes
DFtoSF = function(VIDF, coords=c("X", "Y"))
{
    if (!is.data.frame(VIDF))
        VIDF = as.data.frame(VIDF)
    VISpatial = st_as_sf(VIDF, coords=coords, dim="XY", remove=FALSE)
    rm(VIDF)
    names(VISpatial)[names(VISpatial) == "geometry"] = "geom"
    st_geometry(VISpatial) = "geom" # rename to match GPKG name
    st_crs(VISpatial) = 4326
    return(VISpatial)
}

# Run a specificed function on the time series of input SF and return a compatible SF
RunSFFunction = function(sfo, fx, subset=TRUE, keepcols=c("X", "Y"), ...)
{
    cbind(sfo[subset, keepcols], fx(as.matrix(subset(as.data.frame(sfo), subset=subset, ...))))
}

# Apply a function to each row of the sf and return the result as a matrix
ApplySF = function(sfo, fx, subset=TRUE, select=TRUE, cl=NULL, ...)
{
    t(pbapply(as.matrix(subset(as.data.frame(sfo), subset=subset, select=select)), 1, fx, ..., cl=cl))
}

