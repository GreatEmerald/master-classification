# Clean all data and merge it all into a single database
# It should have either three tables or a column
# to determine whether it's training, validation, or prediction
# Based on merge-data.r

library(corrplot)
library(reshape2)
source("pixel-based/utils/load-sampling-data.r")

RawSoilDB = "../data/pixel-based/soil/soilgrids-raw.gpkg"
ProcessedSoilDB = "../data/pixel-based/soil/soilgrids.gpkg"
RawHarmonicsDB = "../data/pixel-based/timeseries/harmonics.gpkg"
ProcessedHarmonicsDB = "../data/pixel-based/timeseries/harmonics-decorr.gpkg"
RawCompositeDB = "../data/pixel-based/timeseries/composites.gpkg"
ProcessedCompositeDB = "../data/pixel-based/timeseries/composites-decorr.gpkg"
RawClimateDB = "../data/pixel-based/climate/climate-extended.gpkg"
ProcessedClimateDB = "../data/pixel-based/climate/climate-decorr.gpkg"
RawTerrainDB = "../data/pixel-based/terrain/terrain.gpkg"
ProcessedTerrainDB = "../data/pixel-based/terrain/terrain-decorr.gpkg"
RawTempDB = "../data/pixel-based/climate/modis-temperature.gpkg"
AllCovarDB = "../data/pixel-based/covariates.gpkg"

OutlierVis = function(covar, range)
{
    print(quantile(covar, c(0, 0.05, 0.95, 1), na.rm=TRUE))
    covarsub = covar
    covarsub[covarsub < min(range) | covarsub > max(range)] = NA
    print(c(sum(is.na(covar)), sum(is.na(covarsub))))
    hist(covarsub, breaks="Scott")
}

## Harmonise soil ##

SoilDataT = st_read(RawSoilDB, "Training")
SoilDataT$Type = "Training"
SoilDataP = st_read(RawSoilDB, "Prediction")
SoilDataP$Type = "Prediction"
SoilDataV = st_read(RawSoilDB, "Validation")
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

st_write(SoilData, ProcessedSoilDB, delete_dsn=TRUE)

## Harmonise harmonics ##

HarmonicsSF = st_read(RawHarmonicsDB)
HarmonicsNames = names(HarmonicsSF)[3:(ncol(HarmonicsSF)-1)]
HarmKeep = HarmonicsNames

# Remove outliers, these are calculation mistakes due to too few data leading to overfitting
CovarRange = c(-40000, 40000)
OutlierVis(HarmonicsSF$intercept, CovarRange)
HarmonicsSF$intercept[HarmonicsSF$intercept < min(CovarRange) | HarmonicsSF$intercept > max(CovarRange)] = NA # ~1000 more NAs

# These are all Laplace distributions, not normal
OutlierVis(HarmonicsSF$co, c(-200, 200)) # Consistently lose about 1000 points, likely the same ones
HarmonicsSF$co[HarmonicsSF$co < -200 | HarmonicsSF$co > 200] = NA

OutlierVis(HarmonicsSF$si, c(-150, 150))
HarmonicsSF$si[HarmonicsSF$si < -150 | HarmonicsSF$si > 150] = NA

OutlierVis(HarmonicsSF$co2, c(-75, 75))
HarmonicsSF$co2[HarmonicsSF$co2 < -75 | HarmonicsSF$co2 > 75] = NA

OutlierVis(HarmonicsSF$si2, c(-75, 75))
HarmonicsSF$si2[HarmonicsSF$si2 < -75 | HarmonicsSF$si2 > 75] = NA

CovarRange = c(-20, 20)
OutlierVis(HarmonicsSF$trend, CovarRange)
HarmonicsSF$trend[HarmonicsSF$trend < min(CovarRange) | HarmonicsSF$trend > max(CovarRange)] = NA

# Strictly positive, so bimodal exponential
CovarRange = c(-Inf, 250)
OutlierVis(HarmonicsSF$amplitude1, CovarRange)
HarmonicsSF$amplitude1[HarmonicsSF$amplitude1 < min(CovarRange) | HarmonicsSF$amplitude1 > max(CovarRange)] = NA

# Exponential
CovarRange = c(-Inf, 100)
OutlierVis(HarmonicsSF$amplitude2, CovarRange)
HarmonicsSF$amplitude2[HarmonicsSF$amplitude2 < min(CovarRange) | HarmonicsSF$amplitude2 > max(CovarRange)] = NA

scor = cor(as.data.frame(HarmonicsSF)[,HarmKeep], use="pairwise.complete.obs")
pairs(as.data.frame(HarmonicsSF)[sample(1:nrow(HarmonicsSF), 10000),HarmKeep])
which(abs(scor) > 0.9)
HarmKeep = HarmKeep[!HarmKeep %in% "intercept"] # Intercept and trend are almost the same, drop intercept

scor = cor(as.data.frame(HarmonicsSF)[,HarmKeep], use="pairwise.complete.obs")
pairs(as.data.frame(HarmonicsSF)[sample(1:nrow(HarmonicsSF), 10000),HarmKeep])
scor[which(abs(scor) > 0.9)] # Only the diagonal, so no correlations left

scor = cor(as.data.frame(HarmonicsSF)[,HarmKeep], use="pairwise.complete.obs", method="spearman")
scor[which(abs(scor) > 0.9)] # Only the diagonal as well

# Write result to temp db

st_write(HarmonicsSF[,c("X", "Y", HarmKeep)], ProcessedHarmonicsDB, delete_dsn=TRUE)

## Composites ##

CompositeSF = st_read(RawCompositeDB)
names(CompositeSF)
CompKeep = names(CompositeSF)[c(-1,-2,-103)]

# Some EVI values are -Inf, which makes things very slow. Replace all -Inf with NA
for (EVIname in grep("EVI", CompKeep))
    CompositeSF[[EVIname]][is.infinite(CompositeSF[[EVIname]])] = NA

scor = cor(as.data.frame(CompositeSF)[,c(-1,-2,-103)], use="pairwise.complete.obs")
corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")
mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
plot(EVI.year.mean~EVI.year.median, data=CompositeSF)
# correlation with outliers is artful but doesn't work, remove them
CovarRange = c(-10, 15)
OutlierVis(CompositeSF$EVI.year.mean, CovarRange)
#CompositeSF$EVI.year.mean[CompositeSF$EVI.year.mean < min(CovarRange) | CompositeSF$EVI.year.mean > max(CovarRange)] = NA

CovarRange = c(-2, 5)
OutlierVis(CompositeSF$EVI.year.median, CovarRange) # Very few losses
#CompositeSF$EVI.year.median[CompositeSF$EVI.year.median < min(CovarRange) | CompositeSF$EVI.year.median > max(CovarRange)] = NA

plot(EVI.year.mean~EVI.year.median, data=CompositeSF) # Not that correlated after all
plot(OSAVI.year.mean~OSAVI.year.median, data=CompositeSF) # OSAVI is fine
plot(NDMI.year.mean~NDMI.year.median, data=CompositeSF) # NDMI is fine
plot(NIRv.year.mean~NIRv.year.median, data=CompositeSF) # NIRv is fine
summary(CompositeSF[,CompKeep]) # Only EVI values are outlier-prone

CovarRange = c(-10, 15)
OutlierVis(CompositeSF$EVI.spring.mean, CovarRange)
#CompositeSF$EVI.spring.mean[CompositeSF$EVI.spring.mean < min(CovarRange) | CompositeSF$EVI.spring.mean > max(CovarRange)] = NA
CovarRange = c(-10, 15)
OutlierVis(CompositeSF$EVI.spring.median, CovarRange) # Very few losses
#CompositeSF$EVI.spring.median[CompositeSF$EVI.spring.median < min(CovarRange) | CompositeSF$EVI.spring.median > max(CovarRange)] = NA
# Cap everything to that range
CovarRange = c(-10, 15)
for (EVIname in grep("EVI", CompKeep, value=TRUE))
    CompositeSF[[EVIname]][CompositeSF[[EVIname]] < min(CovarRange) | CompositeSF[[EVIname]] > max(CovarRange)] = NA
summary(CompositeSF[,CompKeep]) # EVI values capped


scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs") # Very fast

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

# Medians and means are 0.99 correlated. Drop means because medians are robust against clouds and EVI -Infs
CompKeep = CompKeep[-grep("mean", CompKeep)]

scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

plot(OSAVI.winter.p80.80.~OSAVI.winter.median, data=CompositeSF) # Seasonal quantiles are highly correlated, remove

CompKeep = CompKeep[!(CompKeep %in% grep("year", grep("p80", CompKeep, value=TRUE), value=TRUE, invert=TRUE))]
CompKeep = CompKeep[!(CompKeep %in% grep("year", grep("p20", CompKeep, value=TRUE), value=TRUE, invert=TRUE))]

scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

plot(OSAVI.summer.median~OSAVI.year.p80.80., data=CompositeSF)
# Correlation between quantiles of the year and the seasonal medians. The seasonal medians are not portable across hemispheres so drop those

CompKeep = CompKeep[!(CompKeep %in% grep("year", grep("median", CompKeep, value=TRUE), value=TRUE, invert=TRUE))]

scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

plot(EVI.year.median~OSAVI.year.median, data=CompositeSF)
# EVI.year has issues with Infs and outliers, and correlates with both NIRv and OSAVI, so drop it

CompKeep = CompKeep[!(CompKeep %in% grep("EVI.year", CompKeep, value=TRUE))]

scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

plot(OSAVI.year.p80.80.~OSAVI.year.median, data=CompositeSF)
plot(NIRv.year.median~OSAVI.year.median, data=CompositeSF) # NIRv correlates with OSAVI; remove OSAVI as NIRv correlates with GPP better

CompKeep = CompKeep[!(CompKeep %in% grep("OSAVI.year", CompKeep, value=TRUE))]

scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "ellipse", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

plot(NDMI.autumn.IQR~NDMI.summer.IQR, data=CompositeSF)
pairs(as.data.frame(CompositeSF)[sample(1:nrow(CompositeSF), 10000),CompKeep]) # No more correlations, though EVI is still unusual and mostly IQR remains which may not be useful

# Check also Spearman
scor = cor(as.data.frame(CompositeSF)[,CompKeep], use="pairwise.complete.obs", method="spearman")
mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Medians too correlated to the other percentiles, so drop all percentiles

CompKeep = CompKeep[!(CompKeep %in% grep("p80.80", CompKeep, value=TRUE))]
CompKeep = CompKeep[!(CompKeep %in% grep("p20.20", CompKeep, value=TRUE))]

st_write(CompositeSF[,c("X", "Y", CompKeep)], ProcessedCompositeDB, delete_dsn=TRUE)

## Harmonise climate ##

ClimateSF = st_read(RawClimateDB)
names(ClimateSF)
ClimKeep = names(ClimateSF)[3:144]
ClimKeep = ClimKeep[!(ClimKeep %in% c("cold", "warm", "dry", "wet"))] # Exclude helper columns

summary(as.data.frame(ClimateSF)[,ClimKeep]) # Some have -Inf
for (Climname in ClimKeep)
    ClimateSF[[Climname]][is.infinite(ClimateSF[[Climname]])] = NA

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

# Exclude duplicates: bio5, bio6, bio13, bio14 are already calculated as additional biovars
ClimKeep = ClimKeep[!(ClimKeep %in% c("bio5", "bio6", "bio13", "bio14"))]


scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
plot(wc2.0_30s_vapr_02~wc2.0_30s_vapr_01, data=ClimateSF)
# There is a huge correlation between neighbouring months, so keep only 4 months
barplot(table(ClimateSF$wet)) # 07
barplot(table(ClimateSF$dry)) # 01
barplot(table(ClimateSF$cold)) # 01
barplot(table(ClimateSF$warm)) # 07
# Ergo, keep 01, 07, 04, 10

ClimKeep = ClimKeep[!(ClimKeep %in% grep("_02|_03|_05|_06|_08|_09|_11|_12", ClimKeep, value=TRUE))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
plot(wc2.0_30s_tavg_01~wc2.0_30s_tmin_01, data=ClimateSF) # There is not enough of a range in temperatures, so drop all tmin/tmax

ClimKeep = ClimKeep[!(ClimKeep %in% grep("tmin|tmax", ClimKeep, value=TRUE))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # tavg monthly still not enough to be distinct from bio*

ClimKeep = ClimKeep[!(ClimKeep %in% grep("wc2.0_30s_tavg_", ClimKeep, value=TRUE))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # bio8-11,16-19 are quarterly which is not distinct from monthly
ClimKeep = ClimKeep[!(ClimKeep %in% c("bio8", "bio10", "bio11", "bio9", "bio16", "bio17", "bio18", "bio19"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Also not enough distinction to keep vapour non-extreme months

ClimKeep = ClimKeep[!(ClimKeep %in% c("wc2.0_30s_vapr_04", "wc2.0_30s_vapr_10"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # It's cold because there is little solar radiation, and then there is little vapour
# Also bio4 and bio7 are both seasonality, keep range as easier to interpret

ClimKeep = ClimKeep[!(ClimKeep %in% c("cold.srad", "warm.srad", "cold.vapr", "warm.vapr", "bio4"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Not enough variation in wind to keep the extremes
# Also temperature average of the coldest/warmest months doesn't make sense given that we also had tmin and tmax

ClimKeep = ClimKeep[!(ClimKeep %in% c("min.wind", "max.wind", "cold.tavg", "warm.tavg"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Vapour range not enough
# Also monthly wind is not distinct enough

ClimKeep = ClimKeep[!(ClimKeep %in% c("min.vapr", "max.vapr", grep("wc2.0_30s_wind", ClimKeep, value=TRUE)))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Cold/dry wind and solar radiation non-extreme months are not distinct

ClimKeep = ClimKeep[!(ClimKeep %in% c("wc2.0_30s_srad_10", "wc2.0_30s_srad_04", "cold.wind", "warm.wind", "dry.wind", "wet.wind"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Dry/wet vapour and vapour of individual months not distinct
plot(mean.vapr~wc2.0_30s_vapr_01, data=ClimateSF)

ClimKeep = ClimKeep[!(ClimKeep %in% c("dry.vapr", "wet.vapr", grep("wc2.0_30s_vapr", ClimKeep, value=TRUE)))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # Wet/dry doesn't matter for temperature
ClimKeep = ClimKeep[!(ClimKeep %in% c("dry.tavg", "wet.tavg"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)

pairs(as.data.frame(ClimateSF)[sample(1:nrow(ClimateSF), 10000),ClimKeep])

hist(ClimateSF$cold.prec, breaks="Scott") # lognormal, so transform
hist(log(ClimateSF$cold.prec+1), breaks="Scott")

for (PrecName in grep("prec", ClimKeep, value=TRUE))
{
    PrecLogName = paste0(PrecName, ".log")
    ClimateSF[[PrecLogName]] = log(ClimateSF[[PrecName]]+1)
    ClimKeep[ClimKeep == PrecName] = PrecLogName
}

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
pairs(as.data.frame(ClimateSF)[sample(1:nrow(ClimateSF), 10000),ClimKeep])

# bio12+ are also precipitation
PrecName = "bio12"
PrecLogName = paste0(PrecName, ".log")
ClimateSF[[PrecLogName]] = log(ClimateSF[[PrecName]]+1)
ClimKeep[ClimKeep == PrecName] = PrecLogName

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
pairs(as.data.frame(ClimateSF)[sample(1:nrow(ClimateSF), 10000),ClimKeep])

# What about wind?
hist(ClimateSF$mean.wind, breaks="Scott") # Actually fairly normal
hist(log(ClimateSF$mean.wind), breaks="Scott") # Less normal

# Wet precipitation is too similar
ClimKeep = ClimKeep[!(ClimKeep %in% c("dry.prec.log", "wet.prec.log"))]

scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
pairs(as.data.frame(ClimateSF)[sample(1:nrow(ClimateSF), 10000),ClimKeep])

# All good, what about spearman
scor = cor(as.data.frame(ClimateSF)[,ClimKeep], use="pairwise.complete.obs", method="spearman")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
# Also fine

# Rename to human readable format
ProcessedClimateSF = ClimateSF[,ClimKeep]
RenameMap = data.frame(from=c("wc2.0_30s_prec_01.log", "wc2.0_30s_prec_04.log", "wc2.0_30s_prec_07.log", "wc2.0_30s_prec_10.log",
    "wc2.0_30s_srad_01", "wc2.0_30s_srad_07",
    "bio1", "bio2", "bio3", "bio7", "bio12.log", "bio15"),
    to=c("jan.prec.log", "apr.prec.log", "jul.prec.log", "oct.prec.log",
    "jan.srad", "jul.srad",
    "mean.tavg", "tavg.monthly.range", "isothermality", "tavg.annual.range", "annual.prec.log", "prec.seasonality"), stringsAsFactors=FALSE)
for (NameIdx in 1:nrow(RenameMap))
    names(ProcessedClimateSF)[names(ProcessedClimateSF) == RenameMap$from[NameIdx]] = RenameMap$to[NameIdx]

ProcessedClimateSF = cbind(ProcessedClimateSF, X=ClimateSF$X, Y=ClimateSF$Y)

st_write(ProcessedClimateSF, ProcessedClimateDB, delete_dsn=TRUE)

## Check against MODIS LSTM

LSTMSF = st_read(RawTempDB)
names(LSTMSF)
levels(LSTMSF$Type)
plot(TMDMOD_2011.M.Aug~TMDMOD_2011.M.Feb, data=LSTMSF)
nrow(ProcessedClimateSF) > nrow(LSTMSF)
summary(LSTMSF)

CombinedClimate = merge(ProcessedClimateSF, as.data.frame(LSTMSF)[,!(names(LSTMSF) %in% "geom")], by=c("X", "Y"))
nrow(CombinedClimate) # even larger

ClimKeep = names(CombinedClimate)[-c(1,2,24,27)]

scor = cor(as.data.frame(CombinedClimate)[,ClimKeep], use="pairwise.complete.obs")
plot(TMDMOD_2011.M.Aug~max.srad, data=CombinedClimate)

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor)
pairs(as.data.frame(CombinedClimate)[sample(1:nrow(CombinedClimate), 10000),ClimKeep])

plot(TMDMOD_2011.M.Aug~mean.tavg, data=CombinedClimate)
plot(TMDMOD_2011.M.Feb~mean.tavg, data=CombinedClimate)
# Feb is too correlated but Aug isn't, that's odd. Let's not use this

## Harmonise terrain

TerrainSF = st_read(RawTerrainDB)
TerrKeep = names(TerrainSF)[-c(1,2,8)]
pairs(as.data.frame(TerrainSF)[,TerrKeep]) # Artful, but outliers due to issues in ASTER

# Laplace distribution
OutlierVis(TerrainSF[["tpi"]], c(-50, 50))
TerrainSF[["tpi"]][TerrainSF[["tpi"]] < -50 | TerrainSF[["tpi"]] > 50] = NA
pairs(as.data.frame(TerrainSF)[,TerrKeep])

# Exponential distribution
OutlierVis(TerrainSF[["roughness"]], c(-Inf, Inf))
OutlierVis(log(TerrainSF[["roughness"]]+1), c(-Inf, Inf))
TerrainSF[["roughness"]] = log(TerrainSF[["roughness"]]+1)
names(TerrainSF)[names(TerrainSF)=="roughness"] = "roughness.log"
TerrKeep = names(TerrainSF)[-c(1,2,8)]
pairs(as.data.frame(TerrainSF)[,TerrKeep])

# Lognormal distribution
OutlierVis(TerrainSF[["slope"]], c(-Inf, Inf))
OutlierVis(log(TerrainSF[["slope"]]+1), c(-Inf, Inf))
TerrainSF[["slope"]] = log(TerrainSF[["slope"]]+1)
names(TerrainSF)[names(TerrainSF)=="slope"] = "slope.log"
TerrKeep = names(TerrainSF)[-c(1,2,8)]
pairs(as.data.frame(TerrainSF)[,TerrKeep])

cor(as.data.frame(TerrainSF)[,TerrKeep], use="pairwise.complete.obs") # Roughness and slope are too correlated, keep slope
TerrKeep = TerrKeep[!(TerrKeep %in% "roughness.log")]

cor(as.data.frame(TerrainSF)[,TerrKeep], use="pairwise.complete.obs")
cor(as.data.frame(TerrainSF)[,TerrKeep], use="pairwise.complete.obs", method="spearman")

st_write(TerrainSF[,c("x", "y", TerrKeep)], ProcessedTerrainDB, delete_dsn=TRUE)

## Merge them all into one big database
# We merge the details of the training and validation data separately later, because we also have a lot of points that have neither.

SoilSF = st_read(ProcessedSoilDB)
HarmonicsSF = st_read(ProcessedHarmonicsDB)
CompositeSF = st_read(ProcessedCompositeDB)
ClimateSF = st_read(ProcessedClimateDB)
TerrainSF = st_read(ProcessedTerrainDB)

nrow(SoilSF)
nrow(HarmonicsSF)
nrow(CompositeSF)
nrow(ClimateSF)
nrow(TerrainSF)
# All are slightly different, so let's try and merge them all

Mismatched = !(ClimateSF$X %in% HarmonicsSF$X)
sum(Mismatched) # 10 mismatched (in climate but not harmonics). Where?
plot(ClimateSF[Mismatched,"mean.tavg"]) # No apparent pattern...

Mismatched = !(ClimateSF$X %in% SoilSF$X)
sum(Mismatched) # 19 mismatched (in climate but not soil).
plot(ClimateSF[Mismatched,"mean.tavg"])

Mismatched = !(SoilSF$X %in% HarmonicsSF$X)
sum(Mismatched) # 10 again
plot(SoilSF[Mismatched,"Type"]) # Mostly validation. Also no clear pattern, maybe off the boundaries or so

# Do the merge
CovarSF = merge(SoilSF, as.data.frame(HarmonicsSF)[,names(HarmonicsSF) != "geom"], by=c("X", "Y"), all=TRUE)
nrow(CovarSF)
Mismatched = !(SoilSF$X %in% CovarSF$X)
stopifnot(sum(Mismatched) == 0) # 0, good

CovarSF = merge(CovarSF, as.data.frame(CompositeSF)[,names(CompositeSF) != "geom"], by=c("X", "Y"), all=TRUE)
CovarSF = merge(CovarSF, as.data.frame(ClimateSF)[,names(ClimateSF) != "geom"], by=c("X", "Y"), all=TRUE)
CovarSF = merge(CovarSF, as.data.frame(TerrainSF)[,names(TerrainSF) != "geom"], by.x=c("X", "Y"), by.y=c("x", "y"), all=TRUE)

# Now we have 19 empty geometries, so regenerate those
CovarSF = st_as_sf(as.data.frame(CovarSF)[,names(CovarSF) != "geometry"], dim="XY", coords=c("X", "Y"), remove=FALSE)
st_crs(CovarSF) = 4326

# Decorrelate the remainder
NumberCols = names(CovarSF)[!(names(CovarSF) %in% c("X", "Y", "Type", "geometry"))]
scor = cor(as.data.frame(CovarSF)[,NumberCols], use="pairwise.complete.obs")

corrplot(scor, diag = FALSE, order = "alphabet",
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper") # Some problems with max.srad
summary(CovarSF[["max.srad"]])
         
mscor = melt(scor)
mscor = mscor[mscor$Var1 != mscor$Var2,]
mscor = mscor[order(abs(mscor[["value"]]), decreasing=TRUE),]
head(mscor) # None above 0.9

# Regenerate Type column.
# If it's both Prediction and Training, treat as Training.
# If it's both Validation and Training, treat as Validation (we have fewer of those).
# So Validation > Training > Rest (assume Prediction).
CovarSF$Type = "Prediction"
TrainPoints = LoadGlobalTrainingData()
TrainIDs = paste0(TrainPoints$x, "Y", TrainPoints$y)
any(duplicated(TrainIDs))
CovarIDs = paste0(CovarSF$X, "Y", CovarSF$Y)
DuplicatedData = as.data.frame(CovarSF[duplicated(CovarIDs),]) # We have duplicates!
stopifnot(sum(!duplicated(DuplicatedData)) == length(unique(CovarIDs[duplicated(CovarIDs)])))
# All of these also have the same data. Good. We can just dedup.
CovarSF = CovarSF[!duplicated(CovarSF),]

CovarIDs = paste0(CovarSF$X, "Y", CovarSF$Y)
stopifnot(!any(duplicated(CovarIDs))) # No more duplicates

sum(CovarIDs %in% TrainIDs) == length(TrainIDs) # All training points accounted for
CovarSF$Type[CovarIDs %in% TrainIDs] = "Training"

ValidationPoints = LoadGlobalValidationData()
ValidationIDs = paste0(ValidationPoints$x, "Y", ValidationPoints$y)
sum(CovarIDs %in% ValidationIDs) == length(ValidationIDs) # One validation point is missing, no problem
CovarSF$Type[CovarIDs %in% ValidationIDs] = "Validation"

table(CovarSF$Type) # P: 289622 T: 150405 V: 21751

# Write
st_write(CovarSF, AllCovarDB, delete_dsn=TRUE)

# Add spatial index
library(gdalUtils)
ogrinfo(AllCovarDB, sql="CreateSpatialIndex('covariates', 'geom')")
