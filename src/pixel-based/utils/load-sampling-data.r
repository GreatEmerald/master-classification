# Loading the validation dataset as `sf` objects
library(sf)

# Load the global model training dataset (IIASA).
LoadGlobalTrainingData = function(filename="~/shared/training_data_100m_16042018_V2.csv")
{
    # Read data
    SamplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"))
    # Set CRS to WGS84
    st_crs(SamplePoints) = 4326

    return(SamplePoints)
}
