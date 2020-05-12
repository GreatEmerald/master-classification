# Multivariate (general) linear regression
library(hydroGOF)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("utils/accuracy-statistics.r")
source("pixel-based/utils/crossvalidation.r")
source("pixel-based/utils/subpixel-confusion-matrix.r")

Data.df.orig = LoadTrainingAndCovariates()
Data.df.orig = st_set_geometry(Data.df.orig, NULL)
# Manually rescale
Data.df = Data.df.orig
#Data.df = RescaleBasedOn(Data.df.orig, Data.df.orig, GetAllPixelCovars())
#Data.df = NAToMean(Data.df, GetAllPixelCovars())
#Data.df[is.na(Data.df)] = -9999
Data.df[is.na(Data.df)] = 0
Data.df = TidyData(Data.df, drop.cols=NULL)

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val = st_set_geometry(Data.val, NULL)
#Data.val = RescaleBasedOn(Data.val, Data.df.orig, GetAllPixelCovars())
#Data.val = NAToMean(Data.val, GetAllPixelCovars())
#Data.val[is.na(Data.val)] = -9999
Data.val[is.na(Data.val)] = 0
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050

AllCovars = GetAllPixelCovars()
Classes = GetCommonClassNames()
Truth = Data.val[,Classes]

FullFormula = paste(paste0("cbind(", paste(Classes, collapse=","), ")"), paste0(GetAllPixelCovars(), collapse="+"), sep="~")

l = lm(FullFormula, data=Data.df)
l_pred = predict(l, Data.val)
l_pred[l_pred < 0] = 0
l_pred = ScalePredictions(l_pred, FALSE)

AccuracyStatisticsPlots(l_pred[,Classes], Data.val[,Classes]) # RMSE 21.6, MAE 12.7
SCM(l_pred[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 56±0.04 kappa 0.43±0.05
NSE(unlist(as.data.frame(l_pred[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.48
PlotHex(as.data.frame(l_pred[,Classes]), Truth[,Classes], "Multivariate linear regression, all covariates")
PlotBox(as.data.frame(l_pred[,Classes]), Truth[,Classes], main="Multivariate linear regression, all covariates", binpredicted=TRUE)
write.csv(l_pred[,Classes], "../data/pixel-based/predictions/lm-all.csv", row.names = FALSE)

# If we replace NA with 0
AccuracyStatisticsPlots(l_pred[,Classes], Data.val[,Classes]) # RMSE 21.4, MAE 12.6
SCM(l_pred[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 56±0.04 kappa 0.43±0.05
NSE(unlist(as.data.frame(l_pred[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.49
PlotHex(as.data.frame(l_pred[,Classes]), Truth[,Classes], "Multivariate linear regression, all covariates, NA to 0")
PlotBox(as.data.frame(l_pred[,Classes]), Truth[,Classes], main="Multivariate linear regression, all covariates, NA to 0", binpredicted=TRUE)
write.csv(l_pred[,Classes], "../data/pixel-based/predictions/lm-all-na0.csv", row.names = FALSE)

# NA to mean is worse at 21.6/13.0
# Scaling and to mean is also worse at 21.7/13.0

# Which variables are significant?
car::Anova(l) # Error

l$rank

# Make a custom function to add variables (because dropping results in collinearities)
stepANOVA = function(formula, data)
{
    formula = as.formula(formula)
    InterceptFormula = update.formula(formula, .~1)
    InterceptModel = lm(InterceptFormula, data)
    CovarsToTest = labels(terms(formula))
    PVals = 0
    while(min(PVals) < 0.05)
    {
        PVals = sapply(CovarsToTest, function(Covar)
        {
            CovarF = update.formula(InterceptFormula, paste(".~.+", Covar))
            CovarM = lm(CovarF, data)
            Usefulness = anova(InterceptModel, CovarM, test="Spherical")
            # Use Greenhouse adjusted p-value
            CovarP = Usefulness$`G-G Pr`[2]
            CovarP
        })
        if (min(PVals) < 0.05)
        {
            CovarToAdd = CovarsToTest[which.min(PVals)]
            InterceptFormula = update.formula(InterceptFormula, paste(".~.+", CovarToAdd))
            print(InterceptFormula)
            InterceptModel = lm(InterceptFormula, data)
            CovarsToTest = CovarsToTest[-which.min(PVals)]
        }
    }
    return(InterceptModel)
}

stepl = stepANOVA(FullFormula, Data.df)
stepl$rank # We dropped 3 covariates...

labels(terms(l))[!labels(terms(l)) %in% labels(terms(stepl))]
# EVI.autumn.IQR, si2, amplitude2

# Is aspect really useful?
FWA = formula(cbind(tree, shrub, grassland, crops, urban_built_up, bare, water) ~ 
                  y + yabs + min + max + NDMI.year.median + NDMI.year.IQR + 
                  NIRv.year.median + NIRv.year.IQR + elevation + aspect + 
                  max.srad + soil.av.water + soil.bulkdens + soil.clay.pct + 
                  soil.log.cation + soil.log.coarfrag + soil.ph + wet.srad + 
                  mean.tavg + jan.prec.log + tavg.monthly.range + jul.prec.log + 
                  oct.prec.log + tavg.annual.range + annual.prec.log + 
                  mean.wind + mean.vapr + warm.prec.log + soil.wilt.wat + 
                  soil.sand.pct + slope.log + jan.srad + x + prec.seasonality + 
                  jul.srad + mean.srad + cold.prec.log + apr.prec.log + 
                  min.srad + dry.srad + co2 + EVI.winter.IQR + OSAVI.summer.IQR + 
                  NIRv.summer.IQR + NDMI.summer.IQR + NIRv.winter.IQR + 
                  NDMI.winter.IQR + OSAVI.winter.IQR + phase1 + EVI.summer.IQR + 
                  trend + isothermality + phase2 + co + OSAVI.autumn.IQR + 
                  NIRv.autumn.IQR + NDMI.autumn.IQR + amplitude1 + si + 
                  NIRv.spring.IQR + NDMI.spring.IQR + OSAVI.spring.IQR + 
                  EVI.spring.IQR + tpi)
FWOA = formula(cbind(tree, shrub, grassland, crops, urban_built_up, bare, water) ~ 
                   y + yabs + min + max + NDMI.year.median + NDMI.year.IQR + 
                   NIRv.year.median + NIRv.year.IQR + elevation + 
                   max.srad + soil.av.water + soil.bulkdens + soil.clay.pct + 
                   soil.log.cation + soil.log.coarfrag + soil.ph + wet.srad + 
                   mean.tavg + jan.prec.log + tavg.monthly.range + jul.prec.log + 
                   oct.prec.log + tavg.annual.range + annual.prec.log + 
                   mean.wind + mean.vapr + warm.prec.log + soil.wilt.wat + 
                   soil.sand.pct + slope.log + jan.srad + x + prec.seasonality + 
                   jul.srad + mean.srad + cold.prec.log + apr.prec.log + 
                   min.srad + dry.srad + co2 + EVI.winter.IQR + OSAVI.summer.IQR + 
                   NIRv.summer.IQR + NDMI.summer.IQR + NIRv.winter.IQR + 
                   NDMI.winter.IQR + OSAVI.winter.IQR + phase1 + EVI.summer.IQR + 
                   trend + isothermality + phase2 + co + OSAVI.autumn.IQR + 
                   NIRv.autumn.IQR + NDMI.autumn.IQR + amplitude1 + si + 
                   NIRv.spring.IQR + NDMI.spring.IQR + OSAVI.spring.IQR + 
                   EVI.spring.IQR + tpi)
MWOA = lm(FWOA, Data.df)
anova(MWOA, stepl, test="Spherical") # Says that it is

stepl_pred = predict(stepl, Data.val)
stepl_pred[stepl_pred < 0] = 0
stepl_pred = ScalePredictions(stepl_pred, FALSE)

# No real difference
AccuracyStatisticsPlots(stepl_pred[,Classes], Data.val[,Classes]) # RMSE 21.6, MAE 12.7
SCM(stepl_pred[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 56±0.04 kappa 0.43±0.05
NSE(unlist(as.data.frame(stepl_pred[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.48
PlotHex(as.data.frame(stepl_pred[,Classes]), Truth[,Classes], "Multivariate linear regression, stepwise covariates")
PlotBox(as.data.frame(stepl_pred[,Classes]), Truth[,Classes], main="Multivariate linear regression, stepwise covariates", binpredicted=TRUE)

# Without aspect
lwoa_pred = predict(MWOA, Data.val)
lwoa_pred[lwoa_pred < 0] = 0
lwoa_pred = ScalePredictions(lwoa_pred, FALSE)

# Slightly worse indeed
AccuracyStatisticsPlots(lwoa_pred[,Classes], Data.val[,Classes]) # RMSE 21.7, MAE 12.8
SCM(lwoa_pred[,Classes]/100, Truth[,Classes]/100, plot=TRUE, totals=TRUE) # OA 55±0.04 kappa 0.43±0.05
NSE(unlist(as.data.frame(lwoa_pred[,Classes]))/100, unlist(Truth[,Classes]/100)) # 0.48
PlotHex(as.data.frame(lwoa_pred[,Classes]), Truth[,Classes], "Multivariate linear regression, stepwise covariates without aspect")
PlotBox(as.data.frame(lwoa_pred[,Classes]), Truth[,Classes], main="Multivariate linear regression, stepwise covariates without aspect", binpredicted=TRUE)
