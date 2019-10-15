# Clean all data and merge it all into a single database
# It should have either three tables or a column
# to determine whether it's training, validation, or prediction
# Based on merge-data.r

source("pixel-based/utils/load-sampling-data.r")

SoilDataT = st_read("../data/pixel-based/soil/soilgrids-raw.gpkg", "Training")
SoilDataT$Type = "Training"
SoilDataP = st_read("../data/pixel-based/soil/soilgrids-raw.gpkg", "Prediction")
SoilDataP$Type = "Prediction"
SoilDataV = st_read("../data/pixel-based/soil/soilgrids-raw.gpkg", "Validation")
SoilDataV$Type = "Validation"
# Merge all three layers
SoilData = rbind(SoilDataT, SoilDataP, SoilDataV)
# Write
st_write(SoilData, "../data/pixel-based/soil/soilgrids-merged.gpkg")
rm(SoilDataT, SoilDataP, SoilDataV)

# Drop non-numeric columns (all set to NA) and ones that have too many NAs to be useful
SampleSize = 100000
CovarNAs = apply(SoilData[sample(nrow(SoilData), SampleSize),], 2, function(x){sum(is.na(x)) / SampleSize})
sort(CovarNAs)

SoilData = SoilData[,-which(CovarNAs > 0.0376)] # Down to 104 vars, making sure not to drop organic carbon
gc(TRUE)

# Drop taxonomy
SoilData[,grep("TAX", names(SoilData))] = NULL
gc(TRUE)

# Drop 2^16-1
which(SoilData==2^16-1)

# Points that are not over soil are set to 0 for all columns. Replace with NA

