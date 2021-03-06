# Script for getting variable importance using Random Forest
library(ranger)
library(caret)
library(ComplexHeatmap) # This is a bioconductor package, use BiocManager::install("ComplexHeatmap")

source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/covariate-names.r")
source("pixel-based/utils/crossvalidation.r")

Data.df = LoadTrainingAndCovariates()
Data.df[is.na(Data.df)] = -9999
#Data.sp = Data.df
Data.df = st_set_geometry(Data.df, NULL)
Data.df = AddZeroValueColumns(Data.df)
Data.df = TidyData(Data.df, drop.cols=NULL)
#Data.sp = Data.sp[rownames(Data.df),]

# Validation data
Data.val = LoadValidationAndCovariates()
Data.val[is.na(Data.val)] = -9999
Val.sp = Data.val
Data.val = st_set_geometry(Data.val, NULL)
Data.val = TidyData(Data.val, drop.cols=NULL) # Drops around 1050
Val.sp = Val.sp[rownames(Data.val),]

Truth = Data.val[,GetCommonClassNames()]

## Group covariates by type of information they provide

PermuteMe = function(CovarsInGroup, Model, Class, RealPrediction, statistic="RMSE")
{
    PermutedVal = Data.val
    set.seed(0xbeefcab)
    for (covar in CovarsInGroup)
        PermutedVal[[covar]] = sample(PermutedVal[[covar]])
        
    Permutation = predict(Model, PermutedVal)
    AccuracyStats(Permutation$predictions, Truth[[Class]])[[statistic]] - AccuracyStats(RealPrediction$predictions, Truth[[Class]])[[statistic]]
}

PermuteMe3 = function(CovarsInGroup, Model, RealPrediction, statistic="RMSE")
{
    PermutedVal = Data.val
    set.seed(0xbeefcab)
    for (covar in CovarsInGroup)
        PermutedVal[[covar]] = sample(PermutedVal[[covar]])
    
    Permutation = predict(Model, PermutedVal)
    AccuracyStatTable(Permutation, Truth[,Classes])[statistic] - AccuracyStatTable(RealPrediction, Truth[,Classes])[statistic]
}

GetPermutationImportance = function(statistic="RMSE", AdjustPerNumCovars = FALSE, ClassNames=GetCommonClassNames(), CovarGroups=GetAllPixelCovars(TRUE))
{
    FullFormula = paste0("~", paste(GetAllPixelCovars(), collapse = "+"))
    Importances = NULL
    for (Class in ClassNames)
    {
        print(Class)
        Formula = update.formula(FullFormula, paste0(Class, " ~ ."))
        rfmodel = ranger(Formula, Data.df)
        RealPrediction = predict(rfmodel, Data.val)

        Importances = rbind(Importances, sapply(CovarGroups, PermuteMe, Model=rfmodel, Class=Class, RealPrediction=RealPrediction, statistic=statistic))
        row.names(Importances)[nrow(Importances)] = Class
    }
    if (AdjustPerNumCovars)
        return(t(t(Importances) / sapply(CovarGroups, length)))
    return(Importances)
}

GetPermutationImportance3 = function(statistic="RMSE", AdjustPerNumCovars = FALSE, ClassNames=GetCommonClassNames(), CovarGroups=GetAllPixelCovars(TRUE))
{
    rfmodel = RFModel3("../data/pixel-based/predictions/", "randomforest-threestep-median.rds", PredictType="quantiles")
    RealPrediction = predict(rfmodel, Data.val)
    
    Importances = lapply(CovarGroups, PermuteMe3, Model=rfmodel, RealPrediction=RealPrediction, statistic=statistic)
    Importances = do.call(cbind, Importances)
    names(Importances) = if (is.list(CovarGroups)) names(CovarGroups) else CovarGroups
    
    if (AdjustPerNumCovars)
        return(t(t(Importances) / sapply(CovarGroups, length)))
    return(Importances)
}

plot_heatmap = function(Importances)
{
    fields::image.plot(1:nrow(Importances), 1:ncol(Importances), Importances,
        axes=FALSE, xlab = "", ylab = "", #xlab = "Class", ylab = "Covariate")
        bigplot=c(0.25, 0.85, 0.25, 0.95), smallplot=c(0.88, 0.9, 0.25, 0.95), col=fields::designer.colors(100, c("white", "darkred")))
    #points(0,0)
    axis(side=1, at=1:nrow(Importances), labels=rownames(Importances), las=2)
    axis(side=2, at=1:ncol(Importances), labels=colnames(Importances), las=1)


    #min.z <- min(Importances)
    #max.z <- max(Importances)
    #z.yellows <- min.z + (max.z - min.z)/64*c(20,45) 
    # Loop for label printing: black letters on light background and white letters on dark background
    for (i in 1:nrow(Importances))
    {
        for (j in 1:ncol(Importances))
        {
            #if ((Importances[i,j] > z.yellows[1]) & (Importances[i,j] < z.yellows[2]))
            #{
            text(i,j,round(Importances[i,j], 2), col="black", cex = 0.8)
            #}else
            #{
            #  text(i,j,round(Importances[i,j]), col="white", cex = 0.8)     
            #}
        }
    }
}

RawImportancesRMSE = GetPermutationImportance()
AdjustedImportancesRMSE = t(t(RawImportancesRMSE) / sapply(GetAllPixelCovars(TRUE), length))
svg("../rf-importances-rmse.svg", width = 12)
plot_heatmap(RawImportancesRMSE)
dev.off()
plot_heatmap(AdjustedImportancesRMSE)
RawImportancesMAE = GetPermutationImportance("MAE")
AdjustedImportancesMAE = t(t(RawImportancesMAE) / sapply(CovarGroups, length))
plot_heatmap(RawImportancesMAE)
plot_heatmap(AdjustedImportancesMAE)

# 3-step model
RawImportancesRMSE3 = GetPermutationImportance3(CovarGroups=GetAllPixelCovars())
plot_heatmap(RawImportancesRMSE3)
write.csv(RawImportancesRMSE3, "../data/pixel-based/varimp-rf3step-all.csv", row.names = FALSE)
RawImportancesMAE3 = GetPermutationImportance3(CovarGroups=GetAllPixelCovars(), statistic = "MAE")
write.csv(RawImportancesMAE3, "../data/pixel-based/varimp-rf3step-all-mae.csv", row.names = FALSE)

## Variable importance for remote sensing-only model

# Loose: RS + location, ungrouped
RSCovars = GetAllPixelCovars(TRUE)
RSCovars = c(RSCovars$location, RSCovars$spectral, RSCovars$harmonic)
RSLRMSE = GetPermutationImportance(CovarGroups = RSCovars)
plot_heatmap(RSLRMSE)
svg("../output/2020-04-14-rf-importances-rsonly-ungrouped-rmse.svg", width = 12)
plot_heatmap(RSLRMSE)
dev.off()
png("../output/2020-04-14-rf-importances-rsonly-ungrouped-rmse.png", width = 1200)
plot_heatmap(RSLRMSE)
dev.off()
devEMF::emf("../output/2020-04-14-rf-importances-rsonly-ungrouped-rmse.emf", width = 12, height = 9)
plot_heatmap(RSLRMSE)
dev.off()

# Grouped
RSCovars = GetAllPixelCovars(TRUE)
RSCovars = c(RSCovars["location"], RSCovars["spectral"], RSCovars["harmonic"])
RSLRMSE_G = GetPermutationImportance(CovarGroups = RSCovars)
plot_heatmap(RSLRMSE_G)
devEMF::emf("../output/2020-04-14-rf-importances-rsonly-grouped-rmse.emf", width = 12)
plot_heatmap(RSLRMSE_G)
dev.off()

# Tight: only RS
RSTRMSE = GetPermutationImportance(CovarGroups = GetAllPixelCovars(TRUE)$spectral)
plot_heatmap(RSTRMSE)
devEMF::emf("../output/2020-04-14-rf-importances-spectralonly-ungrouped-rmse.emf", width = 12)
plot_heatmap(RSTRMSE)
dev.off()

## More complex plots

# Get the whole matrix
PermAll = GetPermutationImportance(CovarGroups = GetAllPixelCovars())
colnames(PermAll) = GetAllPixelCovars()
# Cache it, takes a while to calculate
write.csv(PermAll, "../data/pixel-based/varimp-rf1step-all.csv", row.names = FALSE)
rownames(PermAll) = GetCommonClassNames()

## Simple barplots
library(ggplot2)
library(devEMF)

# One facet per class, top 15 variables
# Structure of the data.frame:
# RMSE | Class | Variable | Category
PALong = melt(PermAll, varnames=c("Class", "Variable"), value.name="RMSE")
PALong$Category = PALong$Variable
levels(PALong$Category) = GetAllPixelCovars(TRUE) # This is pretty amazing

ggplot(PALong, aes(Variable, RMSE, fill=Category)) + geom_col() + facet_wrap(.~Class, scales="free")

# Within each Class, we keep only the top 15 RMSEs.
KeepTop = 15
PAFiltered = do.call("rbind", by(PALong, PALong$Class, function(df){df[order(df$RMSE, decreasing=TRUE)[1:KeepTop],]}))
ggplot(PAFiltered, aes(Variable, RMSE, fill=Category)) + geom_col() + facet_wrap(.~Class, scales="free")

# Order properly. Note: a big hack;
# normally should be handled by a grid of plots, but this way we can keep a single legend.
PAFiltered$Combo = paste(PAFiltered$Class, PAFiltered$Variable)
emf("../output/2020-04-21-varimp-top15.emf", width=1272/100, height=634/100)
VIP = ggplot(PAFiltered, aes(RMSE, reorder(Combo, RMSE), fill=Category)) + geom_col() + facet_wrap(.~Class, scales="free_y") +
    scale_y_discrete(name="Variable", labels = setNames(as.character(PAFiltered$Variable), PAFiltered$Combo)) +
    #guides(col = guide_legend(ncol = 2)) +
    scale_fill_discrete(guide = guide_legend(direction = "horizontal")) + 
    theme(legend.position = c(0.8, 0.15))
dev.off()
ggsave("../output/2020-04-21-varimp-top15.pdf", VIP, width=1272/100, height=634/100)

## Complex heatmaps

KeepTop = 15
ImportanceStats = colSums(PermAll)
Perm15 = PermAll[,order(ImportanceStats, decreasing = TRUE)[1:KeepTop]]

VarCats = as.factor(colnames(Perm15))
levels(VarCats) = GetAllPixelCovars(TRUE)
VarCatCols = rainbow(length(levels(VarCats)))
names(VarCatCols) = levels(VarCats)

# Everything in one
emf("../output/2020-04-27-varimp-heatmap-all.emf", width=1272/100, height=634/100)
Heatmap(Perm15, name="RMSE", column_title = "Variable", row_title = "Class", #column_names_gp = gpar(fontsize = 7),
        top_annotation = HeatmapAnnotation(Category=VarCats, col=list(Category=VarCatCols)))
dev.off()

# By category
emf("../output/2020-04-27-varimp-heatmap-categorised.emf", width=1272/100, height=634/100)
Heatmap(Perm15, name="RMSE", row_title = "Class", show_column_dend = FALSE, show_row_dend = FALSE,
        #column_names_gp = gpar(fontsize = 7),
        column_title_gp = gpar(fontsize = 11), column_names_rot = 45,
        column_split = VarCats)
dev.off()

# Horizontal and with values
emf("../output/2020-04-27-varimp-heatmap-values.emf", width=1272/100, height=634/100)
Heatmap(t(Perm15), name="RMSE", column_title = "Class", show_column_dend = FALSE, show_row_dend = FALSE,
        #row_names_gp = gpar(fontsize = 7), row_title_gp = gpar(fontsize = 11),
        column_names_rot = 45, row_title_rot = 0,
        row_split = VarCats, col=circlize::colorRamp2(c(6, 0, -1), c("forestgreen", "lightyellow", "red")),
        cell_fun = function(j, i, x, y, w, h, col) {
            grid.text(round(t(Perm15)[i, j], 2), x, y, gp=gpar(fontsize = 12))
        })
dev.off()

# Top 10 per category

KeepTop = 10
PermDF.category = as.factor(colnames(PermAll))
levels(PermDF.category) = GetAllPixelCovars(TRUE)
ImportanceStatList = tapply(ImportanceStats, list(PermDF.category), function(x) sort(x, decreasing = TRUE)[1:min(KeepTop, length(x))])
names(ImportanceStatList) = NULL
ImportanceStatsCat = sort(unlist(ImportanceStatList), decreasing = TRUE)
Perm10 = PermAll[,names(ImportanceStatsCat)]

VarCats = as.factor(colnames(Perm10))
levels(VarCats) = GetAllPixelCovars(TRUE)

devEMF::emf("../output/2020-05-12-varimp-heatmap-top10.emf", height=800/100, width=800/100)
png("../output/2020-05-12-varimp-heatmap-top10.png", height=800, width=800)
Heatmap(t(Perm10), name="RMSE", column_title = "Class", show_column_dend = FALSE, show_row_dend = FALSE,
        #row_names_gp = gpar(fontsize = 7), row_title_gp = gpar(fontsize = 11),
        column_names_rot = 45, row_title_rot = 0,
        row_split = VarCats, col=circlize::colorRamp2(c(6, 0.5), c("forestgreen", "white")),
        cell_fun = function(j, i, x, y, w, h, col) {
            Value=t(Perm10)[i, j]
            #if (Value > 0.5)
                grid.text(round(Value, 2), x, y, gp=gpar(fontsize = 12, alpha=min(max(Value, 0.05), 1)))
        })
dev.off()

# Top 5

KeepTop = 5
PermDF.category = as.factor(colnames(PermAll))
levels(PermDF.category) = GetAllPixelCovars(TRUE)
ImportanceStatList = tapply(ImportanceStats, list(PermDF.category), function(x) sort(x, decreasing = TRUE)[1:min(KeepTop, length(x))])
names(ImportanceStatList) = NULL
ImportanceStatsCat = sort(unlist(ImportanceStatList), decreasing = TRUE)
Perm10 = PermAll[,names(ImportanceStatsCat)]

rownames(Perm10) = PrettifyNames(rownames(Perm10))
colnames(Perm10) = PrettifyNames(colnames(Perm10))
VarCats = as.factor(PrettifyNames(colnames(Perm10)))
ImportanceStatsGroup = tapply(ImportanceStats, list(PermDF.category), sum)
ImportanceStatsGroup = sort(ImportanceStatsGroup, decreasing = TRUE)
levels(VarCats) = PrettifyNames(GetAllPixelCovars(TRUE))
VarCats = factor(VarCats, levels=PrettifyNames(names(ImportanceStatsGroup)))

devEMF::emf("../output/2020-06-19-varimp-heatmap-top5.emf", height=600/100, width=600/100)
png("../output/2020-05-27-varimp-heatmap-top5.png", height=500, width=800)
pdf("../output/2020-11-06-varimp-heatmap-top5.pdf", height=600/100, width=700/100)
Heatmap(t(Perm10), name="MAE", column_title = "Class", show_column_dend = FALSE, show_row_dend = FALSE,
        row_names_gp = gpar(fontsize = 9), row_title_gp = gpar(fontsize = 10), column_title_gp = gpar(fontsize = 10), column_names_gp = gpar(fontsize = 10),
        column_names_rot = 45, row_title_rot = 0,
        row_split = VarCats, cluster_row_slices = FALSE, cluster_rows = FALSE, cluster_columns = FALSE,
        col=circlize::colorRamp2(c(3, 0.2), c("forestgreen", "white")),
        cell_fun = function(j, i, x, y, w, h, col) {
            Value=t(Perm10)[i, j]
            #if (Value > 0.5)
            grid.text(round(Value, 2), x, y, gp=gpar(fontsize = 9#, alpha=min(max(Value, 0.5), 1)
                                                     ))
        })
dev.off()

## Make a LaTeX table
Hmisc::latex(data.frame(Covariate=PrettifyNames(GetAllPixelCovars())), file="")
