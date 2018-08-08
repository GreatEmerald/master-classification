# Utils for getting covariate names, based on their correlations

# List of names of pixel-based covariates that are known to not be correlated. Useful for random forest VarImp.
GetUncorrelatedPixelCovars = function()
{
    return(c("x", "y", "co", "co2", "si2", "trend", "phase1", "amplitude1", "phase2",
             "amplitude2", "mean.ndvi", "nir", "elevation", "slope", "aspect", "tpi"))
}

GetAllPixelCovars = function()
{
    return(c("x", "y", "min", "max", "intercept", "co", "si", "co2", "si2", "trend",
             "phase1", "amplitude1", "phase2", "amplitude2", "mean.ndvi", "ndvi.25", "ndvi.75", "ndvi.iqr",
             "red", "nir", "blue", "swir", "ndvi", "ndmi", "osavi", "evi",
             "elevation", "slope", "aspect", "tpi", "tri", "roughness"))
}

# Names of the classes in the original data collected by IIASA
GetIIASAClassNames = function()
{
    # There is also "not_sure", but it's pointless to predict that
    return(c("bare", "burnt", "crops", "fallow_shifting_cultivation", "grassland", "lichen_and_moss", "shrub",
             "snow_and_ice", "tree", "urban_built_up", "water", "wetland_herbaceous"))
}
