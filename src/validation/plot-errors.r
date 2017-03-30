# Plots the error bar chart for all variables
library(ggplot2)

# Prettify class names: take a character vector, give a sorted pretty factor
PrettifyClasses = function(classnames)
{
    classnames = factor(classnames)
    levels(classnames) = list(
        "Total" = "Overall", "Cultivated" = "cropland", "Dec. trees" = "dec.trees", "Evgr. trees" = "evgr.trees",
        "Shrubs" = "shrubland", "Grass" = "grassland", "Wetland" = "wetland", "Bare soil" = "bare.soil",
        "Built-up" = "urban", "Water" = "water")
    return(classnames)
}

cmu = read.csv("../data/stat-cmeans-unoptimised.csv")
cmu$Method = "FCM"#"Fuzzy c-means"
cmu$Optimisation = "Unoptimised"
cmo = read.csv("../data/stat-cmeans.csv")
cmo$Method = "FCM"#"Fuzzy c-means"
cmo$Optimisation = "Optimised"
cmpu = read.csv("../data/stat-cmeans-pure-unoptimised.csv")
cmpu$Method = "FCM"#"Fuzzy c-means"
cmpu$Optimisation = "Unoptimised"
cmpo = read.csv("../data/stat-cmeans-pure.csv")
cmpo$Method = "FCM"#"Fuzzy c-means"
cmpo$Optimisation = "Optimised"

rfu = read.csv("../data/stat-randomforest-unoptimised.csv")
rfu$Method = "RFR"#"Random forest"
rfu$Optimisation = "Unoptimised"
rfo = read.csv("../data/stat-randomforest.csv")
rfo$Method = "RFR"#"Random forest"
rfo$Optimisation = "Optimised"
rfpu = read.csv("../data/stat-randomforest-pure-unoptimised.csv")
rfpu$Method = "RFR"#"Random forest"
rfpu$Optimisation = "Unoptimised"
rfpo = read.csv("../data/stat-randomforest-pure.csv")
rfpo$Method = "RFR"#"Random forest"
rfpo$Optimisation = "Optimised"

gbu = read.csv("../data/stat-gradientboost-unoptimised.csv")
gbu$Method = "MGB"#"Gradient boosting"
gbu$Optimisation = "Unoptimised"
gbo = read.csv("../data/stat-gradientboost.csv")
gbo$Method = "MGB"#"Gradient boosting"
gbo$Optimisation = "Optimised"

nnu = read.csv("../data/stat-neuralnetworks-unoptimised.csv")
nnu$Method = "NN"#"Neural networks"
nnu$Optimisation = "Unoptimised"
nno = read.csv("../data/stat-neuralnetworks.csv")
nno$Method = "NN"#"Neural networks"
nno$Optimisation = "Optimised"
nnpu = read.csv("../data/stat-neuralnetworks-pure-unoptimised.csv")
nnpu$Method = "NN"#"Neural networks"
nnpu$Optimisation = "Unoptimised"
nnpo = read.csv("../data/stat-neuralnetworks-pure.csv")
nnpo$Method = "NN"#"Neural networks"
nnpo$Optimisation = "Optimised"

dum = read.csv("../data/stat-dummy.csv")
dum$Method = "Ctrl"#"Control"
dum$Optimisation = "Unoptimised"

dup = read.csv("../data/stat-dummy-pure.csv")
dup$Method = "Ctrl"#"Control"
dup$Optimisation = "Unoptimised"

AllErrors = rbind(cmu, cmo, rfu, rfo, nnu, nno, dum)
OverallErrors = subset(AllErrors, X=="Overall")
OverallErrors$Method = factor(OverallErrors$Method, levels = c("Ctrl", "NN", "FCM", "RFR"))#c("Control", "Neural networks", "Fuzzy c-means", "Random forest"))
OverallErrors$Optimisation = factor(OverallErrors$Optimisation, levels = c("Unoptimised", "Optimised"))
ErrorsLong = reshape(OverallErrors, varying=2:4, v.names="Error", direction = "long", timevar = "Statistic", times=c("Root mean squared error", "Mean absolute error", "Mean error"))#c("RMSE", "MAE", "ME"))
ErrorsLong = subset(ErrorsLong, ErrorsLong$Statistic != "Mean error")#"ME")

ggplot(ErrorsLong, aes(Statistic, Error, fill=Optimisation)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Method)


pdf("../thesis/thesis-figures/total-errors.pdf", width=4, height=4)
ggplot(ErrorsLong, aes(Method, Error, fill=Optimisation, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(legend.position="none") +
    geom_text(size=2.5, position = position_dodge(1)) +
    scale_fill_manual(values=c("turquoise", "gold"))
dev.off()

# Random forest optimised per class
RFOLong = reshape(rfo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
RFOLong$Statistic = factor(RFOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
RFOLong$X = PrettifyClasses(RFOLong$X)
pdf("../thesis/thesis-figures/perclass-errors-rf.pdf", width=5, height=3)
ggplot(RFOLong, aes(X, Error, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") #+ ggtitle("Random Forest, optimised")
dev.off()

# C-means optimised per class
CMOLong = reshape(cmo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
CMOLong$Statistic = factor(CMOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
CMOLong$X = PrettifyClasses(CMOLong$X)
pdf("../thesis/thesis-figures/perclass-errors-cm.pdf", width=5, height=3)
ggplot(CMOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") #+ ggtitle("Fuzzy c-means, optimised")
dev.off()

# Neural networks per class
NNOLong = reshape(nno, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
NNOLong$Statistic = factor(NNOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
NNOLong$X = PrettifyClasses(NNOLong$X)
pdf("../thesis/thesis-figures/perclass-errors-nn.pdf", width=5, height=3)
ggplot(NNOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") #+ ggtitle("Neural networks, optimised")
dev.off()

# Gradient boosting per class
GBOLong = reshape(gbo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
GBOLong$Statistic = factor(GBOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
GBOLong$X = PrettifyClasses(GBOLong$X)
pdf("../thesis/thesis-figures/perclass-errors-gb.pdf", width=5, height=3)
ggplot(GBOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") #+ ggtitle("Gradient boosting, optimised")
dev.off()

# Control per class
DUMLong = reshape(dum, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
DUMLong$Statistic = factor(DUMLong$Statistic, levels=c("RMSE", "MAE", "ME"))
DUMLong$X = PrettifyClasses(DUMLong$X)
pdf("../thesis/thesis-figures/perclass-errors-ctrl.pdf", width=5, height=3)
ggplot(DUMLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") #+ ggtitle("Control")
dev.off()

# Plot gradient boosting on its own plot with its own control
GBErrors = rbind(gbu, gbo, dup, nnpu, nnpo, rfpu, rfpo, cmpu, cmpo)
OverallGBErrors = subset(GBErrors, X=="Overall")
OverallGBErrors$Method = factor(OverallGBErrors$Method, levels = c("Ctrl", "NN", "FCM", "RFR", "MGB"))
OverallGBErrors$Optimisation = factor(OverallGBErrors$Optimisation, levels = c("Unoptimised", "Optimised"))
GBErrorsLong = reshape(OverallGBErrors, varying=2:4, v.names="Error", direction = "long", timevar = "Statistic", times=c("Root mean squared error", "Mean absolute error", "Mean error"))#times=c("RMSE", "MAE", "ME"))
GBErrorsLong = subset(GBErrorsLong, GBErrorsLong$Statistic != "Mean error")#"ME")

pdf("../thesis/thesis-figures/total-errors-gb.pdf", width=4, height=4)
ggplot(GBErrorsLong, aes(Method, Error, fill=Optimisation, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(legend.position="none") +
    geom_text(size=2.5, position = position_dodge(1)) +
    scale_fill_manual(values=c("turquoise", "gold"))
dev.off()

# Also plot timings
RawTimings = read.csv("../data/timing.csv")
RelevantTimings = subset(RawTimings, X %in% c("Dummy brick", "RF cropland", "RF dec.trees", "RF evgr.trees", "RF shrubland", "RF grassland", "RF bare soil", "RF wetland", "RF urban", "RF water", "RF phase2", "cmeans", "Gradient boosting", "Neural networks"))
RelevantTimings = subset(RelevantTimings, !Optimised)
RelevantTimings$Method = c("Ctrl", rep("RFR", 10), "FCM", "MGB", "NN")
pdf("../thesis/thesis-figures/timing.pdf", width=4, height=4)
ggplot(RelevantTimings, aes(Method, Time/60, fill = Class, label=round(Time/60))) + geom_bar(stat = "identity") +# scale_y_log10() +
    ylab("Time (minutes)") + theme(legend.position="none") +
    geom_text(size=2.5, position = position_stack(vjust = 0.5))
dev.off()

## Also check for significance: ANOVA on all the methods
# Import CSVs
cmue = read.csv("../data/errors-stat-cmeans-unoptimised.csv")
cmue$Algorithm = "Fuzzy c-means"
rfue = read.csv("../data/errors-stat-randomforest-unoptimised.csv")
rfue$Algorithm = "Random forest"
nnue = read.csv("../data/errors-stat-neuralnetworks-unoptimised.csv")
nnue$Algorithm = "Neural networks"
dume = read.csv("../data/errors-stat-dummy.csv")
dume$Algorithm = "Dummy"
cmoe = read.csv("../data/errors-stat-cmeans.csv")
cmoe$Algorithm = "Fuzzy c-means"
rfoe = read.csv("../data/errors-stat-randomforest.csv")
rfoe$Algorithm = "Random forest"
nnoe = read.csv("../data/errors-stat-neuralnetworks.csv")
nnoe$Algorithm = "Neural networks"

AllIndivErrorsUnop = rbind(cmue, rfue, nnue, dume)
AbsAnovaUnop = aov(AE~Algorithm, AllIndivErrorsUnop)
summary(AbsAnovaUnop)
TukeyHSD(AbsAnovaUnop)
SqAnovaUnop = aov(AE^2~Algorithm, AllIndivErrorsUnop)
summary(SqAnovaUnop)

AllIndivErrorsOp = rbind(cmoe, rfoe, nnoe, dume)
AbsAnovaOp = aov(AE~Algorithm, AllIndivErrorsOp)
summary(AbsAnovaOp)
TukeyHSD(AbsAnovaOp)
ggplot(AllIndivErrorsOp, aes(x = Algorithm, y = AE)) + geom_boxplot(fill = "grey80", colour = "blue")
plotmeans(AE~Algorithm, AllIndivErrorsOp)
SqAnovaOp = aov(AE^2~Algorithm, AllIndivErrorsOp)
summary(SqAnovaOp)
TukeyHSD(SqAnovaOp)
ggplot(AllIndivErrorsOp, aes(x = Algorithm, y = AE^2)) + geom_boxplot(fill = "grey80", colour = "blue")
