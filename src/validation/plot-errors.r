# Plots the error bar chart for all variables
library(ggplot2)

# Prettify class names: take a character vector, give a sorted pretty factor
PrettifyClasses = function(classnames)
{
    classnames = factor(classnames)
    levels(classnames) = list(
        "Total" = "Overall", "Crops" = "cropland", "Dec. trees" = "dec.trees", "Evgr. trees" = "evgr.trees",
        "Shrubs" = "shrubland", "Grassland" = "grassland", "Wetland" = "wetland", "Bare soil" = "bare.soil",
        "Urban" = "urban", "Water" = "water")
    return(classnames)
}

cmu = read.csv("../data/stat-cmeans-unoptimised.csv")
cmu$Algorithm = "FCM"#"Fuzzy c-means"
cmu$Optimisation = "Unoptimised"
cmo = read.csv("../data/stat-cmeans.csv")
cmo$Algorithm = "FCM"#"Fuzzy c-means"
cmo$Optimisation = "Optimised"
cmpu = read.csv("../data/stat-cmeans-pure-unoptimised.csv")
cmpu$Algorithm = "FCM"#"Fuzzy c-means"
cmpu$Optimisation = "Unoptimised"
cmpo = read.csv("../data/stat-cmeans-pure.csv")
cmpo$Algorithm = "FCM"#"Fuzzy c-means"
cmpo$Optimisation = "Optimised"

rfu = read.csv("../data/stat-randomforest-unoptimised.csv")
rfu$Algorithm = "RF"#"Random forest"
rfu$Optimisation = "Unoptimised"
rfo = read.csv("../data/stat-randomforest.csv")
rfo$Algorithm = "RF"#"Random forest"
rfo$Optimisation = "Optimised"
rfpu = read.csv("../data/stat-randomforest-pure-unoptimised.csv")
rfpu$Algorithm = "RF"#"Random forest"
rfpu$Optimisation = "Unoptimised"
rfpo = read.csv("../data/stat-randomforest-pure.csv")
rfpo$Algorithm = "RF"#"Random forest"
rfpo$Optimisation = "Optimised"

gbu = read.csv("../data/stat-gradientboost-unoptimised.csv")
gbu$Algorithm = "GB"#"Gradient boosting"
gbu$Optimisation = "Unoptimised"
gbo = read.csv("../data/stat-gradientboost.csv")
gbo$Algorithm = "GB"#"Gradient boosting"
gbo$Optimisation = "Optimised"

nnu = read.csv("../data/stat-neuralnetworks-unoptimised.csv")
nnu$Algorithm = "NN"#"Neural networks"
nnu$Optimisation = "Unoptimised"
nno = read.csv("../data/stat-neuralnetworks.csv")
nno$Algorithm = "NN"#"Neural networks"
nno$Optimisation = "Optimised"
nnpu = read.csv("../data/stat-neuralnetworks-pure-unoptimised.csv")
nnpu$Algorithm = "NN"#"Neural networks"
nnpu$Optimisation = "Unoptimised"
nnpo = read.csv("../data/stat-neuralnetworks-pure.csv")
nnpo$Algorithm = "NN"#"Neural networks"
nnpo$Optimisation = "Optimised"

dum = read.csv("../data/stat-dummy.csv")
dum$Algorithm = "Ctrl"#"Control"
dum$Optimisation = "Unoptimised"

dup = read.csv("../data/stat-dummy-pure.csv")
dup$Algorithm = "Ctrl"#"Control"
dup$Optimisation = "Unoptimised"

AllErrors = rbind(cmu, cmo, rfu, rfo, nnu, nno, dum)
OverallErrors = subset(AllErrors, X=="Overall")
OverallErrors$Algorithm = factor(OverallErrors$Algorithm, levels = c("Ctrl", "NN", "FCM", "RF"))#c("Control", "Neural networks", "Fuzzy c-means", "Random forest"))
ErrorsLong = reshape(OverallErrors, varying=2:4, v.names="Error", direction = "long", timevar = "Statistic", times=c("Root mean squared error", "Mean absolute error", "Mean error"))#c("RMSE", "MAE", "ME"))
ErrorsLong = subset(ErrorsLong, ErrorsLong$Statistic != "Mean error")#"ME")

ggplot(ErrorsLong, aes(Statistic, Error, fill=Optimisation)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Algorithm)


pdf("../plot/total-errors.pdf", width=4, height=4)
ggplot(ErrorsLong, aes(Algorithm, Error, fill=Optimisation, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(legend.position="none") +
    geom_text(size=2.5, position = position_dodge(1))
dev.off()

# Random forest optimised per class
RFOLong = reshape(rfo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
RFOLong$Statistic = factor(RFOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
RFOLong$X = PrettifyClasses(RFOLong$X)
pdf("../plot/perclass-errors-rf.pdf", width=5, height=3)
ggplot(RFOLong, aes(X, Error, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Random Forest, optimised")
dev.off()

# C-means optimised per class
CMOLong = reshape(cmo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
CMOLong$Statistic = factor(CMOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
CMOLong$X = PrettifyClasses(CMOLong$X)
pdf("../plot/perclass-errors-cm.pdf", width=5, height=3)
ggplot(CMOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Fuzzy c-means, optimised")
dev.off()

# Neural networks per class
NNOLong = reshape(nno, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
NNOLong$Statistic = factor(NNOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
NNOLong$X = PrettifyClasses(NNOLong$X)
#pdf("../plot/perclass-errors-cm.pdf", width=5, height=3)
ggplot(NNOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Neural networks, optimised")

# Gradient boosting per class
GBOLong = reshape(gbo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
GBOLong$Statistic = factor(GBOLong$Statistic, levels=c("RMSE", "MAE", "ME"))
GBOLong$X = PrettifyClasses(GBOLong$X)
#pdf("../plot/perclass-errors-cm.pdf", width=5, height=3)
ggplot(GBOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Gradient boosting, optimised")

# Control per class
DUMLong = reshape(dum, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
ggplot(DUMLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Control")

# Plot gradient boosting on its own plot with its own control
GBErrors = rbind(gbu, gbo, dup, nnpu, nnpo, rfpu, rfpo, cmpu, cmpo)
OverallGBErrors = subset(GBErrors, X=="Overall")
GBErrorsLong = reshape(OverallGBErrors, varying=2:4, v.names="Error", direction = "long", timevar = "Statistic", times=c("Root mean squared error", "Mean absolute error", "Mean error"))#times=c("RMSE", "MAE", "ME"))
GBErrorsLong = subset(GBErrorsLong, GBErrorsLong$Statistic != "Mean error")#"ME")

pdf("../plot/total-errors-gb.pdf", width=4, height=4)
ggplot(GBErrorsLong, aes(Algorithm, Error, fill=Optimisation, label=round(Error, 1))) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(legend.position="none") +
    geom_text(size=2.5, position = position_dodge(1))
dev.off()

# Also plot timings
RawTimings = read.csv("../data/timing.csv")
RelevantTimings = subset(RawTimings, X %in% c("Dummy brick", "RF cropland", "RF dec.trees", "RF evgr.trees", "RF shrubland", "RF grassland", "RF bare soil", "RF wetland", "RF urban", "RF water", "RF phase2", "cmeans", "Gradient boosting", "Neural networks"))
RelevantTimings = subset(RelevantTimings, !Optimised)
RelevantTimings$Algorithm = c("Ctrl", rep("RF", 10), "FCM", "GB", "NN")
pdf("../plot/timing.pdf", width=4, height=4)
ggplot(RelevantTimings, aes(Algorithm, Mins, fill = Class, label=round(Mins))) + geom_bar(stat = "identity") +# scale_y_log10() +
    ylab("Time (minutes)") + theme(legend.position="none") +
    geom_text(size=2.5, position = position_stack(vjust = 0.5))
dev.off()
