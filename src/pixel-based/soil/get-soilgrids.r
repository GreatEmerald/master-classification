library(rjson)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/soil-apis.r")
source("pixel-based/utils/db-io.r")

DataDir = "../data/pixel-based/soil"
OutFile = file.path(DataDir, "soilgrids-raw.gpkg")
PointsToExtract = LoadGlobalTrainingData()

if (!dir.exists(DataDir))
    dir.create(DataDir)

#ValidationPoints = LoadGlobalValidationData()
#AllPoints = rbind(TrainingPoints[,c("x", "y", "location_id")], ValidationPoints[,c("x", "y", "location_id")])
# The above is too many points, so rerun code only on the points that we know matter from the first run
#AllPoints = read.csv(file.path(DataDir, "covariates", "soil-merged.csv"))
#nrow(AllPoints) # 35581 points

CurrentDataset = tryCatch(st_read(OutFile, query=paste0("SELECT X, Y FROM 'soilgrids-raw'")), error=function(e)NULL)

# Input: a row of the input DF.
# Output: TRUE on a successful write to a database.
ReadWriteDB = function(df, outfile = OutFile, curdata = CurrentDataset)
{
    IsDuplicated = FindSpatialDuplicates(data.frame(X=df$x, Y=df$y), CurrentDataset)
    if (IsDuplicated > 0) # Skip, already exists
        return()
    
    PointData = GetSGMatrix(SGURL(df))
    SPD = DFtoSF(data.frame(X=df$x, Y=df$y, PointData))
    st_write(SPD, outfile)
    return(TRUE)
}

apply(PointsToExtract, 1, ReadWriteDB)

# urls = SGURL(PointsToExtract) # Get all URLs
# 
# MyResults = lapply(urls, GetSGMatrix) # Extract information out of each URL to get a list of matrices
# MyMatrix = plyr::rbind.fill.matrix(MyResults) # "unlist" the list into one large matrix, filling NA values for missing columns
# 
# write.csv(MyMatrix, "../data/pixel-based/soil/soilgrids-rerun-raw.csv", row.names=FALSE)
# 
# EmptyCovars = apply(MyMatrix, 2, function(x){all(is.na(x))}) # Drop empty (=non-numeric) columns
# MyMatrix = MyMatrix[,!EmptyCovars] # 390 covars
# 
# # Remove covariates with no variance (all equal values)
# Variances = apply(MyMatrix, 2, sd, na.rm=TRUE)
# MyMatrix = MyMatrix[,Variances!=0]
# 
# # Replace fill values with NAs
# MyMatrix[MyMatrix == -2^15] = NA
# MyMatrix[MyMatrix == 2^16-1] = NA
# ColsWith255Fill = c("BDRICM.M.BDRICM_M", "BDRLOG.M.BDRLOG_M", "CLYPPT.M.sl1", "CLYPPT.M.sl2", "CLYPPT.M.sl3", "CLYPPT.M.sl4", "CLYPPT.M.sl5", "CLYPPT.M.sl6", "CLYPPT.M.sl7", "CRFVOL.M.sl1", "CRFVOL.M.sl2", "CRFVOL.M.sl3", "CRFVOL.M.sl4", "CRFVOL.M.sl5", "CRFVOL.M.sl6", "CRFVOL.M.sl7", "PHIHOX.M.sl1", "PHIHOX.M.sl2", "PHIHOX.M.sl3", "PHIHOX.M.sl4", "PHIHOX.M.sl5", "PHIHOX.M.sl6", "PHIHOX.M.sl7", "PHIKCL.M.sl1", "PHIKCL.M.sl2", "PHIKCL.M.sl3", "PHIKCL.M.sl4", "PHIKCL.M.sl5", "PHIKCL.M.sl6", "PHIKCL.M.sl7", "SLTPPT.M.sl1", "SLTPPT.M.sl2", "SLTPPT.M.sl3", "SLTPPT.M.sl4", "SLTPPT.M.sl5", "SLTPPT.M.sl6", "SLTPPT.M.sl7", "SNDPPT.M.sl1", "SNDPPT.M.sl2", "SNDPPT.M.sl3", "SNDPPT.M.sl4", "SNDPPT.M.sl5", "SNDPPT.M.sl6", "SNDPPT.M.sl7")
# MyMatrix[,ColsWith255Fill][MyMatrix[,ColsWith255Fill] == 255] = NA
# 
# # Add location ID
# MyMatrix = as.data.frame(MyMatrix)
# MyMatrix$location_id = AllPoints[["location_id"]]
# 
# write.csv(MyMatrix, "../data/pixel-based/soil/soilgrids-rerun.csv", row.names=FALSE)

