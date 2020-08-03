# Scratchpad for data exploration. Used to be part of classify-*.r scripts.
library(caret)

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")

Data = LoadTrainingAndCovariates()
Data.df = Data
class(Data.df) = "data.frame"
rm(Data)
Data.df = AddZeroValueColumns(Data.df)

ClassNames = GetIIASAClassNames(TRUE)
CovariateNames = GetUncorrelatedPixelCovars() #names(Data[which(names(Data)=="min"):(length(Data)-1)])
CovariateNames = GetAllPixelCovars()

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

apply(Data.df[,5:17], 2, function(x){sum(x==0)}) / nrow(Data.df) * 100 # 84% of all data is 0
apply(Data.df[,5:17], 2, function(x){sum(x==100)}) / nrow(Data.df) * 100 # Only bare soil has a lot (18%) of 100

## Imbalanced classes
table(Data.df[["dominant_lc"]])
# Drop lichen and moss
Data.df = Data.df[!Data.df$dominant_lc == "lichen_and_moss",]
# Also drop the level, otherwise sampling would try to sample from 0 points
Data.df$dominant_lc = droplevels(Data.df$dominant_lc)
# We'd lose only 81 points if we exclude wetlands as well, but they are present in Africa

# Test ROSE
library(ROSE)
# fails due to only supporting 2 classes
ovun.sample(paste("dominant_lc ~", paste(CovariateNames, collapse="+")), data=Data.df, method="over", N=max(table(Data.df[["dominant_lc"]]))*length(ClassNames))

# Test DMwR
library(DMwR)
# Crashes if we don't use droplevels beforehand, and crashes if a column is logical
SmoteSample = SMOTE(formula(paste("dominant_lc ~", paste(CovariateNames, collapse="+"))), data=Data.df[,c(ClassNames, "dominant_lc")], k=1)
str(SmoteSample)
# It seems to balance the last level of dominant_lc against all others, rather than balance all among themselves
table(SmoteSample[["dominant_lc"]])
Data.df[Data.df$dominant_lc == "wetland_herbaceous","wetland_herbaceous"]

# Test with just the water class
Data.df$no.water = Data.df$water == 0
Data.df$no.water = as.factor(Data.df$no.water)

Result1 = NULL
Result2 = NULL
for (i in 1:length(folds))
{
ValidationSet = Data.df[folds[[i]],]
TrainingSet = Data.df[-folds[[i]],]
WaterFormula = formula(paste("water ~", paste(CovariateNames, collapse="+")))
WaterFormulaBin = formula(paste("as.factor(no.water) ~", paste(CovariateNames, collapse="+")))
RFFullModelbin = ranger(WaterFormulaBin, data=TrainingSet)

CombinedPred = predict(RFFullModelbin, ValidationSet)$prediction
#plot(ValidationSet$no.water~CombinedPred) # This is in fact a perfect prediction
CombinedPred = as.numeric(!as.logical(CombinedPred)) # Turn FALSE into 1 and TRUE into 0
# Predict the cases where there is more than 0 water
RFIsWaterModel = ranger(WaterFormula, data=TrainingSet[CombinedPred == 1,])
CombinedPred[CombinedPred == 1] = predict(RFIsWaterModel, ValidationSet[CombinedPred == 1,])$prediction
Result1 = c(Result1, CombinedPred)
# Compare between single model and two-step model
RFFullModel = ranger(WaterFormula, data=TrainingSet)
Result2 = c(Result2, predict(RFFullModel, ValidationSet)$prediction)
}
AccuracyStats(c(Result1)[order(unlist(folds))], Data.df$water)
AccuracyStats(c(Result2)[order(unlist(folds))], Data.df$water)

plot(ValidationSet$water~predict(RFFullModel, ValidationSet)$prediction, col="blue")
points(ValidationSet$water~CombinedPred, col="green")
abline(0,1)

AccuracyStats(predict(RFFullModel, ValidationSet)$prediction, ValidationSet$water)
AccuracyStats(CombinedPred, ValidationSet$water) # MAE is better, but RMSE is worse
