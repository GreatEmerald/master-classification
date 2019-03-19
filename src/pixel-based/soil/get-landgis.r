# Same as get-soilgrids except using the new LandGIS version
library(rjson)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/soil-apis.r")

DataDir = "../data/pixel-based"
#TrainingPoints = LoadGlobalTrainingData()
#ValidationPoints = LoadGlobalValidationData()
#AllPoints = rbind(TrainingPoints[,c("x", "y", "location_id")], ValidationPoints[,c("x", "y", "location_id")])
AllPoints = read.csv(file.path(DataDir, "covariates", "soil-merged.csv"))
nrow(AllPoints) # 35581 points

urls = LGURL(AllPoints)
MyResults = lapply(urls, GetSGMatrix, ulproperty="response")
MyMatrix = plyr::rbind.fill.matrix(MyResults)
write.csv(MyMatrix, "../data/pixel-based/soil/landgis-rerun-raw.csv")

# Replace the returned lat/lon with full precision x/y
#hist(abs(MyMatrix$lon - AllPoints$x))
#hist(abs(MyMatrix$lat - AllPoints$y))
MyDF = cbind(x=AllPoints$x, y=AllPoints$y, location_id=AllPoints$location_id, as.data.frame(MyMatrix))
MyDF$lon = NULL
MyDF$lat = NULL

# Get the names into a more readable shape
# sol_bulkdens.fineearth_usda.4a1h_m_250m_b0..0cm_1950..2017_v0.1.tif -> sol_bulkdens.0cm
SoilNames = names(MyDF)[4:ncol(MyDF)]
SoilComponents = strsplit(SoilNames, ".", TRUE)
names(MyDF)[4:ncol(MyDF)] = sapply(SoilComponents, function(x){return(paste0(x[1], ".", strsplit(x[5], "_")[[1]][1]))})

# 255 is a fill value. Replace with NA
all(unlist(MyDF[MyDF$sol_bulkdens.0cm == 255,-(1:3)]) == 255) # Proof that 255s are fill
MyDF[MyDF == 255] = NA

write.csv(MyDF, "../data/pixel-based/soil/landgis-rerun.csv", row.names = FALSE)