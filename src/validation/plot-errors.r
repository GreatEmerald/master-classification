# Plots the error bar chart for all variables
library(ggplot2)

cmu = read.csv("../data/stat-cmeans-unoptimised.csv")
cmu$Algorithm = "Fuzzy c-means"
cmu$Optimisation = "Unoptimised"
cmo = read.csv("../data/stat-cmeans.csv")
cmo$Algorithm = "Fuzzy c-means"
cmo$Optimisation = "Optimised"

rfu = read.csv("../data/stat-randomforest-unoptimised.csv")
rfu$Algorithm = "Random forest"
rfu$Optimisation = "Unoptimised"
rfo = read.csv("../data/stat-randomforest.csv")
rfo$Algorithm = "Random forest"
rfo$Optimisation = "Optimised"

gbu = read.csv("../data/stat-gradientboost-unoptimised.csv")
gbu$Algorithm = "Gradient boosting"
gbu$Optimisation = "Unoptimised"
gbo = read.csv("../data/stat-gradientboost.csv")
gbo$Algorithm = "Gradient boosting"
gbo$Optimisation = "Optimised"

nnu = read.csv("../data/stat-neuralnetworks-unoptimised.csv")
nnu$Algorithm = "Neural networks"
nnu$Optimisation = "Unoptimised"
nno = read.csv("../data/stat-neuralnetworks.csv")
nno$Algorithm = "Neural networks"
nno$Optimisation = "Optimised"

AllErrors = rbind(cmu, cmo, rfu, rfo, gbu, gbo, nnu, nno)
OverallErrors = subset(AllErrors, X=="Overall")
ErrorsLong = reshape(OverallErrors, varying=2:4, v.names="Error", direction = "long", times=c("RMSE", "MAE", "ME"), timevar = "Statistic")

ggplot(ErrorsLong, aes(Statistic, Error, fill=Optimisation)) +
    geom_bar(stat = "identity", position="dodge") +
    facet_wrap( ~ Algorithm)

ggplot(ErrorsLong, aes(Algorithm, Error, color=Optimisation, fill=Optimisation)) +
    geom_bar(stat = "identity", position="dodge") +
#    geom_point(stat = "identity") +
    facet_wrap( ~ Statistic)

# Also plot timings
RawTimings = read.csv("../data/timing.csv")
RelevantTimings = subset(RawTimings, X %in% c("Dummy brick", "RF total", "cmeans", "Gradient boosting", "Neural networks"))
RelevantTimings$Algorithm = c("Control", "Random forest", "Fuzzy c-means", "Gradient boosting", "Neural networks")
ggplot(RelevantTimings, aes(Algorithm, Mins, fill = Algorithm)) + geom_bar(stat = "identity") +# scale_y_log10() +
    ylab("Time (minutes)") + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
