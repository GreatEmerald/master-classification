# Scratchpad for data exploration. Used to be part of classify-*.r scripts.
library(caret)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")

Data = LoadTrainingAndCovariates()
Data.df = Data
class(Data.df) = "data.frame"
rm(Data)
Data.df = AddZeroValueColumns(Data.df)

ClassNames = GetIIASAClassNames()
CovariateNames = GetUncorrelatedPixelCovars() #names(Data[which(names(Data)=="min"):(length(Data)-1)])

# Explore correlations
Covariates[findCorrelation(cor(Data.df[,CovariateNames]), cutoff = 0.75)]

library(corrplot)
DC = cor(Data.df[,c(1:17, 22:49)], use="complete.obs")
DP = cor.mtest(Data.df[,c(1:17, 22:49)])
corrplot(DC, p.mat=DP$p, method="ellipse", insig="blank")
# Trend and intercept are colinear, mean.ndvi and its quantiles (including min max) are highly correlated
# Then red with swir, osavi with ndvi, ndvi.mean with ndvi, evi with ndmi, blue with swir and ndvi, si with co
# TRI and roughness are also correlated with slope
# And NDMI, IQR are correleated with mean.ndvi
DC = cor(Data.df[,c(1:17, 25, 27:34, 39, 46:49)], use="complete.obs")
DP = cor.mtest(Data.df[,c(1:17, 25, 27:34, 39, 46:49)])
corrplot(DC, p.mat=DP$p, method="ellipse", insig="blank")
names(Data.df)[c(1:17, 25, 27:34, 39, 46:49)]

# Zero inflation
nearZeroVar(Data.df[,ClassNames], saveMetrics = TRUE)

# Impute values
apply(Data.df, 2, function(x){sum(is.na(x))}) / nrow(Data.df) * 100 # Most missing in terrain covars, 10%
library(yaImpute)
ImputeFormula = formula(co ~ co2+si2+trend+phase1+amplitude1+phase2+amplitude2+nir+elevation+slope+tpi+tri+roughness+x+y+location_id+rowid+aspect+mean.ndvi)
ImputeFormula = formula(location_id~min)
nrow(Data)
CompleteDataIdx = apply(Data, 1, function(x){!any(is.na(x))})
CompleteData = Data[CompleteDataIdx,]
nrow(CompleteData)

Data.df = Data
class(Data.df) = "data.frame"
Imputer = yai(x=Data.df[,c("x", "y")], y=Data.df[,c("min", "max")])
ExploreImputation = impute(Imputer, observed = FALSE)
names(ExploreImputation)
Data.df[which(is.na(Data.df$min)),]
nrow(Data)
nrow(ExploreImputation)

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

# With caret - actually works
pp = preProcess(Data.df[,CovariateNames], method="bagImpute")
Imputed = predict(pp, Data.df[,CovariateNames])
head(Data.df[is.na(Data.df$elevation),CovariateNames])
head(Imputed[is.na(Data.df$elevation),])

# Drop values instead; all values that have no harmonics:
Data.df = Data.df[!is.na(Data.df$phase2),]
nrow(Data.df)
# And all values that don't have observations in 2017
Data.df = Data.df[!is.na(Data.df$nir),]
Data.df = Data.df[!is.na(Data.df$slope),]

apply(Data[,5:17], 2, function(x){sum(x==0)}) / nrow(Data) * 100 # 84% of all data is 0
apply(Data[,5:17], 2, function(x){sum(x==100)}) / nrow(Data) * 100 # Only bare soil has a lot (18%) of 100

Data.df$no.water = Data.df$water == 0
Data.df$no.water = as.factor(Data.df$no.water)

WaterFormula = formula(paste("water ~", paste(CovariateNames, collapse="+")))
WaterFormulaBin = formula(paste("no.water ~", paste(CovariateNames, collapse="+")))
RFFullModelbin = ranger(WaterFormulaBin, data=Data.df)

CombinedPred = predict(RFFullModelbin, Data.df)$prediction
plot(Data.df$no.water~CombinedPred) # This is in fact a perfect prediction
CombinedPred = as.numeric(!as.logical(CombinedPred)) # Turn FALSE into 1 and TRUE into 0
# Predict the cases where there is more than 0 water
RFIsWaterModel = ranger(WaterFormula, data=Data.df[CombinedPred == 1,])
CombinedPred[CombinedPred == 1] = predict(RFIsWaterModel, Data.df[Data.df$no.water==FALSE,])$prediction

# Compare between single model and two-step model
RFFullModel = ranger(WaterFormula, data=Data.df)
plot(Data.df$water~predict(RFFullModel, Data.df)$prediction, col="blue")
points(Data.df$water~CombinedPred, col="green")
abline(0,1)

MAE.single = mean(abs(Data.df$water - predict(RFFullModel, Data.df)$prediction))
MAE.twostep = mean(abs(Data.df$water - CombinedPred)) # Much better

