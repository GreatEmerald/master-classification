# Predict pixels using Random Forest

library(ranger)
source("pixel-based/utils/load-sampling-data.r")

Data = LoadTrainingAndCovariates()

# Explore data
library(corrplot)
oldClass(Data) = "data.frame"
DC = cor(Data[,c(1:17, 22:49)], use="complete.obs")
DP = cor.mtest(Data[,c(1:17, 22:49)])
corrplot(DC, p.mat=DP$p, method="ellipse", insig="blank")
# Trend and intercept are colinear, mean.ndvi and its quantiles (including min max) are highly correlated
# Then red with swir, osavi with ndvi, ndvi.mean with ndvi, evi with ndmi, blue with swir and ndvi, si with co
# TRI and roughness are also correlated with slope
# And NDMI, IQR are correleated with mean.ndvi
DC = cor(Data[,c(1:17, 25, 27:34, 39, 46:49)], use="complete.obs")
DP = cor.mtest(Data[,c(1:17, 25, 27:34, 39, 46:49)])
corrplot(DC, p.mat=DP$p, method="ellipse", insig="blank")
names(Data)[c(1:17, 25, 27:34, 39, 46:49)]

ClassNames = GetIIASAClassNames()
CovariateNames = c(names(Data)[c(1:4, 25, 27:34, 39, 46:49)])#which(names(Data)=="min"):(length(Data)-1)])
WaterFormula = formula(paste("water ~", paste(CovariateNames, collapse="+")))
RFFullModel = ranger(WaterFormula, data=Data)
plot(Data$water~predict(RFFullModel, Data)$prediction, col="blue")

# Impute values
apply(Data, 2, function(x){sum(is.na(x))}) / nrow(Data) * 100 # Most missing in slope, 13.5%
library(yaImpute)
ImputeFormula = formula(co+co2+si2+trend+phase1+amplitude1+phase2+amplitude2+mean.ndvi+nir+elevation+slope+aspect ~
                            x+y+location_id+rowid+tpi)
Imputer = yai(ImputeFormula, ImputeFormula, data=Data)
str(impute(Imputer))
library(Hmisc)
Data$co = impute(Data$co)
Data$co2 = impute(Data$co2)
Data$si2 = impute(Data$si2)
Data$trend = impute(Data$trend)
Data$phase1 = impute(Data$phase1)
Data$amplitude1 = impute(Data$amplitude1)
Data$phase2 = impute(Data$phase2)
Data$amplitude2 = impute(Data$amplitude2)
Data$mean.ndvi = impute(Data$mean.ndvi)
Data$nir = impute(Data$nir)
Data$elevation = impute(Data$elevation)
Data$slope = impute(Data$slope)
Data$tpi = impute(Data$tpi)

apply(Data[,5:17], 2, function(x){sum(x==0)}) / nrow(Data) * 100 # 84% of all data is 0
apply(Data[,5:17], 2, function(x){sum(x==100)}) / nrow(Data) * 100 # Only bare soil has a lot (18%) of 100

Data$no.water = Data$water == 0
Data$no.water = as.factor(Data$no.water)

WaterFormulaBin = formula(paste("no.water ~", paste(CovariateNames, collapse="+")))
RFFullModelbin = ranger(WaterFormulaBin, data=Data)
plot(Data$no.water~predict(RFFullModelbin, Data)$prediction)

RFIsWaterModel = ranger(WaterFormula, data=Data[Data$no.water==FALSE,])
plot(Data[Data$no.water==FALSE,"water"]~predict(RFIsWaterModel, Data[Data$no.water==FALSE,])$prediction)
abline(0,1)
CombinedPred = predict(RFFullModelbin, Data)$prediction
CombinedPred = as.numeric(!as.logical(CombinedPred))
CombinedPred[CombinedPred == 1] = predict(RFIsWaterModel, Data[Data$no.water==FALSE,])$prediction
points(Data$water~CombinedPred, col="green")

MAE.full = mean(abs(Data$water - predict(RFFullModel, Data)$prediction))
MAE.combined = mean(abs(Data$water - CombinedPred)) # Much better

