# Get statistics about the reference products
library(hydroGOF)
library(reshape2)
source("utils/accuracy-statistics.r")
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/crossvalidation.r")

InDir = "../data/pixel-based/reference-products"

Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
#Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL)

StatTable = NULL

## GSW
ReferenceValues = merge(st_read(file.path(InDir, "gsw.gpkg")), Data.val)
ReferenceValues = ReferenceValues[,c(names(ReferenceValues)[4], "water")]
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]]))
# 10.22 RMSE, 1.94 MAE, -0.63 ME (slight underestimation)
# But better than ours (12 & 3)
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]], TRUE))
# 1.81 RRMSE, 0.34 RMAE, -0.11 RME
NSE(ReferenceValues[[1]], ReferenceValues[[2]]) # 0.78
cor(ReferenceValues[[1]], ReferenceValues[[2]])^2 # 0.78
PlotHex(ReferenceValues[[1]], ReferenceValues[[2]], "Global Surface Water, 2015")
PlotBox(ReferenceValues[[1]], ReferenceValues[[2]], main="Global Surface Water, 2015")

## MEASURES (GFCC) Treecover

ReferenceValues = merge(st_read(file.path(InDir, "treecover.gpkg")), Data.val)
ReferenceValues = ReferenceValues[,c(names(ReferenceValues)[4], "tree")]
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]]))
# 28.80 RMSE, 18.33 MAE, -12.57 ME (underestimation!)
# Much worse than ours (19 & 13)
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]], TRUE))
# 0.91 RRMSE, 0.58 RMAE, -0.40 ME
NSE(ReferenceValues[[1]], ReferenceValues[[2]]) # 0.48
cor(ReferenceValues[[1]], ReferenceValues[[2]])^2 # 0.61
PlotHex(ReferenceValues[[1]], ReferenceValues[[2]], "GFCC tree cover, 2015")
PlotBox(ReferenceValues[[1]], ReferenceValues[[2]], main="GFCC tree cover, 2015")

## GHSL urban

ReferenceValues = merge(st_read(file.path(InDir, "ghsl.gpkg")), Data.val)
ReferenceValues = ReferenceValues[,c(names(ReferenceValues)[4], "urban_built_up")]
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]]))
# 18.67 RMSE, 5.43 MAE, 4.80 ME (overestimation!)
# Much worse than ours (10 & 3)
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]], TRUE))
# 6.34 RRMSE, 1.84 RMAE, 1.63 RME
NSE(ReferenceValues[[1]], ReferenceValues[[2]]) # -1.23
cor(ReferenceValues[[1]], ReferenceValues[[2]])^2 # 0.52
summary(lm(urban_built_up~GHSLBuiltup2014, ReferenceValues)) # High intercept and only 0.36 slope
PlotHex(ReferenceValues[[1]], ReferenceValues[[2]], "Global human settlement built-up, 2014")
PlotBox(ReferenceValues[[1]], ReferenceValues[[2]], main="Global human settlement built-up, 2014")

## Qinghua FROM-GLC impervious year of change

ReferenceValues = merge(st_read(file.path(InDir, "impervious.gpkg")), Data.val)
ReferenceValues = ReferenceValues[,c(names(ReferenceValues)[4], "urban_built_up")]
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]]))
# 11.18 RMSE, 2.57 MAE, 0.93 ME (less overestimation)
# Matches ours (10 & 3)
StatTable = rbind(StatTable, AccuracyStats(ReferenceValues[[1]], ReferenceValues[[2]], TRUE))
# 3.79 RRMSE, 0.87 RMAE, 0.32 RME
NSE(ReferenceValues[[1]], ReferenceValues[[2]]) # 0.20
cor(ReferenceValues[[1]], ReferenceValues[[2]])^2 # 0.65
PlotHex(ReferenceValues[[1]], ReferenceValues[[2]], "FROM-GLC impervious surface, 2015")
PlotBox(ReferenceValues[[1]], ReferenceValues[[2]], main="FROM-GLC impervious surface, 2015")

StatTable = cbind(StatTable, product=factor(c(rep("GSWater",2), rep("GFCC-Forest",2), rep("GHSL-Urban",2), rep("FROM-GLC-Urban",2)), c("GFCC-Forest", "GHSL-Urban", "FROM-GLC-Urban", "GSWater")), relative=c(FALSE, TRUE))

## Plots
StatTableLong = melt(StatTable, variable.name = "statistic")

devEMF::emf("../output/2020-10-06-modelcomparison.emf", height = 3)
ggplot(StatTableLong, aes(x=statistic, y=value, fill=product)) + geom_bar(stat="identity", position = "dodge") + facet_grid(rows="relative", scales = "free_y")
dev.off()

