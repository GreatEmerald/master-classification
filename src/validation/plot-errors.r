# Plots the error bar chart for all variables
library(ggplot2)

cmu = read.csv("../data/stat-cmeans-unoptimised.csv")
cmu$Algorithm = "FCM"#"Fuzzy c-means"
cmu$Optimisation = "Unoptimised"
cmo = read.csv("../data/stat-cmeans.csv")
cmo$Algorithm = "FCM"#"Fuzzy c-means"
cmo$Optimisation = "Optimised"

rfu = read.csv("../data/stat-randomforest-unoptimised.csv")
rfu$Algorithm = "RF"#"Random forest"
rfu$Optimisation = "Unoptimised"
rfo = read.csv("../data/stat-randomforest.csv")
rfo$Algorithm = "RF"#"Random forest"
rfo$Optimisation = "Optimised"

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
ggplot(RFOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Random Forest, optimised")

# C-means optimised per class
CMOLong = reshape(cmo, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
ggplot(CMOLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Fuzzy c-means, optimised")

# Control per class
DUMLong = reshape(dum, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
ggplot(DUMLong, aes(X, Error)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) +
    xlab("Class") + ggtitle("Control")

# Plot gradient boosting on its own plot with its own control
GBErrors = rbind(gbu, gbo, dup)
OverallGBErrors = subset(GBErrors, X=="Overall")
GBErrorsLong = reshape(OverallGBErrors, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")
GBErrorsLong = subset(GBErrorsLong, GBErrorsLong$Statistic != "ME")

ggplot(GBErrorsLong, aes(Algorithm, Error, color=Optimisation, fill=Optimisation)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Statistic)

# Also plot timings
RawTimings = read.csv("../data/timing.csv")
RelevantTimings = subset(RawTimings, X %in% c("Dummy brick", "RF total", "cmeans", "Gradient boosting", "Neural networks"))
RelevantTimings$Algorithm = c("Control", "Random forest", "Fuzzy c-means", "Gradient boosting", "Neural networks")
ggplot(RelevantTimings, aes(Algorithm, Mins, fill = Algorithm)) + geom_bar(stat = "identity") +# scale_y_log10() +
    ylab("Time (minutes)") + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
