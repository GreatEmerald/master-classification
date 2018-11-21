library(rjson)
source("pixel-based/utils/load-sampling-data.r")

Data.df = LoadTrainingAndCovariates()

TrainingPoints = LoadGlobalTrainingData()
ValidationPoints = LoadGlobalValidationData()
AllPoints = rbind(TrainingPoints[,c("x", "y", "location_id")], ValidationPoints[,c("x", "y", "location_id")])

MissingPoints = AllPoints[!AllPoints$location_id %in% Data.df$location_id,]


SGURL = function(points)
{
    return(paste0("https://rest.soilgrids.org/query?lon=", points$x, "&lat=", points$y))
}

urls = SGURL(Data.df)

GetSGMatrix = function(url)
{
    SGData = rjson::fromJSON(RCurl::getURL(url))
    QR = unlist(SGData$properties)
    RRow = as.numeric(QR)
    return(matrix(RRow, ncol=length(RRow), dimnames=list(NULL, names(QR))))
}
MyResults = lapply(urls, GetSGMatrix)

MyMatrix = plyr::rbind.fill.matrix(MyResults)

EmptyCovars = apply(MyMatrix, 2, function(x){all(is.na(x))}) # Drop empty (=non-numeric) columns
MyMatrix = MyMatrix[,!EmptyCovars] # 390 covars

# Remove covariates with no variance (all equal values)
Variances = apply(MyMatrix, 2, sd, na.rm=TRUE)
MyMatrix = MyMatrix[,Variances!=0]

# Add location ID
MyMatrix$location_id = Data.df[["location_id"]]

# Replace fill values with NAs
MyMatrix[MyMatrix == -2^15] = NA
ColsWith255Fill = c("BDRICM.M.BDRICM_M", "BDRLOG.M.BDRLOG_M", "CLYPPT.M.sl1", "CLYPPT.M.sl2", "CLYPPT.M.sl3", "CLYPPT.M.sl4", "CLYPPT.M.sl5", "CLYPPT.M.sl6", "CLYPPT.M.sl7", "CRFVOL.M.sl1", "CRFVOL.M.sl2", "CRFVOL.M.sl3", "CRFVOL.M.sl4", "CRFVOL.M.sl5", "CRFVOL.M.sl6", "CRFVOL.M.sl7", "PHIHOX.M.sl1", "PHIHOX.M.sl2", "PHIHOX.M.sl3", "PHIHOX.M.sl4", "PHIHOX.M.sl5", "PHIHOX.M.sl6", "PHIHOX.M.sl7", "PHIKCL.M.sl1", "PHIKCL.M.sl2", "PHIKCL.M.sl3", "PHIKCL.M.sl4", "PHIKCL.M.sl5", "PHIKCL.M.sl6", "PHIKCL.M.sl7", "SLTPPT.M.sl1", "SLTPPT.M.sl2", "SLTPPT.M.sl3", "SLTPPT.M.sl4", "SLTPPT.M.sl5", "SLTPPT.M.sl6", "SLTPPT.M.sl7", "SNDPPT.M.sl1", "SNDPPT.M.sl2", "SNDPPT.M.sl3", "SNDPPT.M.sl4", "SNDPPT.M.sl5", "SNDPPT.M.sl6", "SNDPPT.M.sl7")
MyMatrix[,ColsWith255Fill][MyMatrix[,ColsWith255Fill] == 255] = NA
write.csv(MyMatrix, "../data/pixel-based/soil/soilgrids-remainder.csv", row.names=FALSE)

