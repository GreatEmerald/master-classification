# Classify images based on the fuzzy c-means algorithm
library(Hmisc)
library(GSIF)
library(caret)
source("utils/load-data.r")

# Load data
alldata = LoadClassificationData()
pixels = LoadTrainingPixels()
#puredata = alldata[alldata$pure]

# There are two ways: either use the built-in method of using a multinomial logistic regression
# with endmembers, or use weighted averages with all pixels.

# Start with builtin

cmeans.mnlr = spfkm(formula("dominant~red+nir+blue+swir+osavi+lswi+height+slope+aspect+tpi"),
    alldata[alldata$pure,], pixels)
# Hard total classification accuracy: 13%, pretty bad
sum(cmeans.mnlr@predicted@data[[1]] == alldata$dominant) / length(alldata)
# If looking at only pure data: 20%, still really bad
sum(cmeans.mnlr@predicted@data[alldata$pure,][[1]] == alldata[alldata$pure,]$dominant) / length(alldata[alldata$pure,])
# RMSE
sqrt(mean(unlist(cmeans.mnlr@mu@data*100 - alldata@data[names(cmeans.mnlr@mu@data)])^2))
# MAE
mean(abs(unlist(cmeans.mnlr@mu@data*100 - alldata@data[names(cmeans.mnlr@mu@data)])))
# ME
mean(unlist(cmeans.mnlr@mu@data*100 - alldata@data[names(cmeans.mnlr@mu@data)]))

# Now with weighted means

set.seed(0xc0ffeed)
folds = createFolds(alldata$cropland, 2)

fold = folds$Fold1
ClassMeans = function(x)
{
    wtd.mean(alldata@data[fold,x["training"]], alldata@data[fold,x["validation"]])
}
combos = expand.grid(validation=1:9, training=13:22)
cm = apply(combos, 1, ClassMeans)
c.means = matrix(cm, nrow=9, dimnames=list(names(alldata)[1:9], names(alldata)[13:22]))

ClassSDs = function(x)
{
    sqrt(wtd.var(alldata@data[fold,x["training"]], alldata@data[fold,x["validation"]], normwt = TRUE))
}
csd = apply(combos, 1, ClassSDs)
c.sds = matrix(csd, nrow=9, dimnames=list(names(alldata)[1:9], names(alldata)[13:22]))

cmeans.wm = spfkm(formula("dominant~red+nir+blue+swir+osavi+lswi+height+slope+aspect+tpi"),
    alldata[fold,], pixels, class.c = c.means, class.sd = c.sds)

# Hard total classification accuracy: 11%, even worse
sum(cmeans.wm@predicted@data[[1]] == alldata$dominant) / length(alldata)
# If looking at input data: 14%, still really bad
sum(cmeans.mnlr@predicted@data[fold,][[1]] == alldata[fold,]$dominant) / length(alldata[fold,])
