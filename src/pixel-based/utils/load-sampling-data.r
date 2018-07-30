# Loading the validation dataset as `sf` objects
library(sf)

source("pixel-based/utils/ProbaVTileID.r")

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="~/shared/training_data_100m_16042018_V2.csv")
{
    # Read data
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), stringsAsFactors = FALSE)
    
    # Set correct data types
    SamplePoints$rowid = as.numeric(SamplePoints$rowid)
    SamplePoints$location_id = as.numeric(SamplePoints$location_id)
    SamplePoints$bare = as.numeric(SamplePoints$bare)
    SamplePoints$burnt = as.numeric(SamplePoints$burnt)
    SamplePoints$crops = as.numeric(SamplePoints$crops)
    SamplePoints$fallow_shifting_cultivation = as.numeric(SamplePoints$fallow_shifting_cultivation)
    SamplePoints$grassland = as.numeric(SamplePoints$grassland)
    SamplePoints$lichen_and_moss = as.numeric(SamplePoints$lichen_and_moss)
    SamplePoints$shrub = as.numeric(SamplePoints$shrub)
    SamplePoints$snow_and_ice = as.numeric(SamplePoints$snow_and_ice)
    SamplePoints$tree = as.numeric(SamplePoints$tree)
    SamplePoints$urban_built_up = as.numeric(SamplePoints$urban_built_up)
    SamplePoints$wetland_herbaceous = as.numeric(SamplePoints$wetland_herbaceous)
    SamplePoints$not_sure = as.numeric(SamplePoints$not_sure)
    SamplePoints$water = as.numeric(SamplePoints$water)
    SamplePoints$dominant_lc = as.factor(SamplePoints$dominant_lc)
    SamplePoints$lc = as.factor(SamplePoints$lc)
    SamplePoints$forest_type = as.factor(SamplePoints$forest_type)
    
    # Set CRS to WGS84
    st_crs(SamplePoints) = 4326
    
    # Add additional fields: Proba-V tile number
    SamplePoints$Tile = as.factor(ProbaVTileID(SamplePoints))
    
    # Note: this takes around 40 MiB in RAM

    return(SamplePoints)
}
