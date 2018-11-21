# Same as get-soilgrids except using the new LandGIS version
library(rjson)
source("pixel-based/utils/load-sampling-data.r")

TrainingPoints = LoadGlobalTrainingData()
ValidationPoints = LoadGlobalValidationData()
AllPoints = rbind(TrainingPoints[,c("x", "y", "location_id")], ValidationPoints[,c("x", "y", "location_id")])

LGURL = function(points)
{
    return(paste0("https://landgisapi.opengeohub.org/query/point?lon=", points$x, "&lat=", points$y, "&coll=predicted250m&regex=sol_[^t].*_m_250m_b"))
}

urls = LGURL(AllPoints)

GetSGMatrix = function(url)
{
    SGData = rjson::fromJSON(RCurl::getURL(url))
    QR = unlist(SGData$response[[1]])
    RRow = as.numeric(QR)
    return(matrix(RRow, ncol=length(RRow), dimnames=list(NULL, names(QR))))
}
MyResults = lapply(urls, GetSGMatrix)

MyMatrix = plyr::rbind.fill.matrix(MyResults)
write.csv(MyMatrix, "../data/pixel-based/soil/landgis.csv")

MyMatrix = read.csv("../data/pixel-based/soil/landgis.csv")
# Replace the returned lat/lon with full precision x/y
hist(abs(MyMatrix$lon - AllPoints$x))
hist(abs(MyMatrix$lat - AllPoints$y))
MyMatrix = cbind(x=AllPoints$x, y=AllPoints$y, location_id=AllPoints$location_id, MyMatrix)
MyMatrix$X = NULL
MyMatrix$lon = NULL
MyMatrix$lat = NULL

# Get the names into a more readable shape
# sol_bulkdens.fineearth_usda.4a1h_m_250m_b0..0cm_1950..2017_v0.1.tif -> sol_bulkdens.0cm
SoilNames = names(MyMatrix)[3:ncol(MyMatrix)]
SoilComponents = strsplit(SoilNames, ".", TRUE)
names(MyMatrix)[3:ncol(MyMatrix)] = sapply(SoilComponents, function(x){return(paste0(x[1], ".", strsplit(x[5], "_")[[1]][1]))})

# 255 is a fill value. Replace with NA
all(unlist(MyMatrix[MyMatrix$sol_bulkdens.0cm == 255,-(1:3)]) == 255) # Proof that 255s are fill
MyMatrix[MyMatrix == 255] = NA

write.csv(MyMatrix, "../data/pixel-based/covariates/landgis.csv", row.names = FALSE)
