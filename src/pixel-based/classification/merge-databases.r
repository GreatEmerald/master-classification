# Clean all data and merge it all into a single database
# It should have either three tables or a column
# to determine whether it's training, validation, or prediction
# Based on merge-data.r

source("pixel-based/utils/load-sampling-data.r")

## Harmonise soil ##

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
set.seed(0xfedbeef)
SampleSize = 10000
CovarNAs = apply(SoilData[sample(nrow(SoilData), SampleSize),], 2, function(x){sum(is.na(x)) / SampleSize})
sort(CovarNAs)

SoilData = SoilData[,-which(CovarNAs >= 0.0351)]
gc(TRUE)
ncol(SoilData) # Down to 214 vars

# Drop taxonomy
SoilData[,grep("TAX", names(SoilData))] = NULL
gc(TRUE)
ncol(SoilData) # Down to 153 vars

ColumnTypes = sapply(1:ncol(SoilData), function(x)class(SoilData[[x]])[1])
# Drop zero sd, drops depth codes
CovarSDs = apply(as.data.frame(SoilData)[sample(nrow(SoilData), SampleSize),ColumnTypes=="numeric"], 2, sd, na.rm=TRUE)
SoilData[,names(CovarSDs)[CovarSDs == 0]] = NULL
gc(TRUE)
ncol(SoilData) # Down to 140 vars

# Drop land cover-related covars: land cover itself and pH in KCL
SoilData[,c("GLC100m.M.GLC2010", paste0("PHIKCL.M.sl", 1:7))] = NULL
gc(TRUE)
ncol(SoilData) # Down to 133 vars

# Keep only sl2
RemDepth = c(1, 3:7)
for (i in RemDepth)
{
    SoilData[,grep(paste0(".sl", i), names(SoilData))] = NULL
}
gc(TRUE)
ncol(SoilData) # Down to 72 vars

# Remove MODIS 2001 data, no Proba-V back then
SoilData[,grep("2001", names(SoilData))] = NULL
gc(TRUE)
ncol(SoilData) # Down to 52 vars

# Remove precipitation data as we have that from Worldclim
SoilData[,grep("PREMRG.", names(SoilData))] = NULL
gc(TRUE)
ncol(SoilData) # Down to 42 vars

# Organic carbon storage is using "sd" instead of "sl"
RemDepth = c(1, 3:5)
for (i in RemDepth)
{
    SoilData[,grep(paste0("OCSTHA.M.sd", i), names(SoilData))] = NULL
}
gc(TRUE)
ncol(SoilData) # Down to 38 vars

# Drop MODIS data except for Feb and Aug (solstice + 2 months, not 1 because missing night values)
RemMonth = c("M", "S", "J", "O", "N", "D", "Ap")
for (i in RemMonth)
{
    SoilData[,grep(paste0("MOD_2011.M.", i), names(SoilData))] = NULL
}
gc(TRUE)
ncol(SoilData) # Down to 23 vars

# Drop AWCh1&3, the second distinguishes natural classes better
SoilData[,c("AWCh1.M.sl2", "AWCh3.M.sl2")] = NULL
gc(TRUE)
ncol(SoilData) # 21

# MODIS temperature is also something that should already be in worldclim, so save it on the side
ModTempNames = c("TMNMOD_2011.M.Aug", "TMNMOD_2011.M.Feb", "TMDMOD_2011.M.Aug", "TMDMOD_2011.M.Feb")
ModTemp = SoilData[,c("X", "Y", "Type", ModTempNames)]

hist(ModTemp[[ModTempNames[1]]]) # NA value is -2^15
for (i in ModTempNames)
    ModTemp[[i]][ModTemp[[i]] == -2^15] = NA
cor(as.data.frame(ModTemp)[,ModTempNames], use="pairwise.complete.obs") # Fairly low correlation, but over 0.9 for day and night in Feb
pairs(as.data.frame(ModTemp)[,ModTempNames])
summary(as.data.frame(ModTemp)[,ModTempNames]) # Night has a lot more NAs, so keep day
ModTemp[,c("TMNMOD_2011.M.Aug", "TMNMOD_2011.M.Feb")] = NULL
st_write(ModTemp, "../data/pixel-based/climate/modis-temperature.gpkg", delete_dsn=TRUE)

SoilData[,ModTempNames] = NULL
rm(ModTemp)
gc(TRUE)
ncol(SoilData) # 17

pairs(as.data.frame(SoilData)[,3:15])
cor(as.data.frame(SoilData)[,3:15], use="pairwise.complete.obs")
# -0.99 correlation between AWCtS.M.sl2 and BLDFIE.M.sl2, drop former;
# OCSTHA vs ORCDRC, drop former as it's just potential
# TEXMHT is actually categorical and correlates with CLYPPT
# We get a singularity if we include all SNDPPT, SLTPPT and CLYPPT
SoilData[,c("AWCtS.M.sl2", "OCSTHA.M.sd2", "TEXMHT.M.sl2")] = NULL
# 255 as NA value for CLYPPT, CRFVOL, SLTPPT, SNDPPT
NA225 = c("SNDPPT.M.sl2", "CLYPPT.M.sl2", "SLTPPT.M.sl2", "CRFVOL.M.sl2", "PHIHOX.M.sl2")
for (i in NA225)
    SoilData[[i]][SoilData[[i]]==255] = NA

scor = cor(as.data.frame(SoilData)[,3:12], use="pairwise.complete.obs")
pairs(as.data.frame(SoilData)[,3:12])

mean(abs(scor[,"CLYPPT.M.sl2"])) # Much less correlated
mean(abs(scor[,"SNDPPT.M.sl2"]))
mean(abs(scor[,"SLTPPT.M.sl2"])) # Most correlated, drop

SoilData[["SLTPPT.M.sl2"]] = NULL

# Check distributions
for (i in names(SoilData))
{
    if (i == "X" || i == "Y" || i == "geom" || i == "Type")
        next
    hist(SoilData[[i]], main=i)
}

# Transform CECSOL, CRFVOL, ORCDRC
hist(log(SoilData[["CECSOL.M.sl2"]]+1)) # Very good
hist(log(SoilData[["CRFVOL.M.sl2"]]+1)) # At least a bit better
hist(log(SoilData[["ORCDRC.M.sl2"]]+1)) # Very good
SoilData[["CECSOL.M.sl2"]] = log(SoilData[["CECSOL.M.sl2"]]+1)
SoilData[["CRFVOL.M.sl2"]] = log(SoilData[["CRFVOL.M.sl2"]]+1)
SoilData[["ORCDRC.M.sl2"]] = log(SoilData[["ORCDRC.M.sl2"]]+1)

cor(as.data.frame(SoilData)[,3:11], use="pairwise.complete.obs")
pairs(as.data.frame(SoilData)[,3:11]) # ORCDRC and BLDFIE are correlated, drop the transformed (ORCDRC)
SoilData[["ORCDRC.M.sl2"]] = NULL

cor(as.data.frame(SoilData)[,3:10], use="pairwise.complete.obs")
cor(as.data.frame(SoilData)[,3:10], method="spearman", use="pairwise.complete.obs") # Nobody is speared
pairs(as.data.frame(SoilData)[,3:10])

# Give pretty names
names(SoilData)[3:10] = c("soil.av.water", "soil.bulkdens", "soil.log.cation", "soil.clay.pct", "soil.log.coarfrag", "soil.ph", "soil.sand.pct", "soil.wilt.wat")

st_write(SoilData, "../data/pixel-based/soil/soilgrids.gpkg", delete_dsn=TRUE)
