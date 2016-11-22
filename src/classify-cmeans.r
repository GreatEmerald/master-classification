# Classify images based on the fuzzy c-means algorithm
library(Hmisc)
library(GSIF)
library(caret)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

# Load data
alldata = LoadClassificationData()
pixels = LoadTrainingPixels()

# There are two ways: either use the built-in method of using a multinomial logistic regression
# with endmembers, or use weighted averages with all pixels.

# Start with builtin

terms = names(pixels)
FullFormula = formula(paste0("dominant~", paste(terms, collapse="+")))
FullFormula = update.formula(FullFormula, "~ . -ndvi") # Drop NDVI due to collinearity with OSAVI

cmeans.mnlr = spfkm(FullFormula, alldata[alldata$pure,], pixels)

# Hard total classification accuracy: 12%, pretty bad
sum(cmeans.mnlr@predicted@data[[1]] == alldata$dominant) / length(alldata)
# If looking at only pure data: 20%, still really bad
sum(cmeans.mnlr@predicted@data[alldata$pure,][[1]] == alldata[alldata$pure,]$dominant) / length(alldata[alldata$pure,])
AccuracyStats(cmeans.mnlr@mu@data*100, alldata@data[names(cmeans.mnlr@mu@data)])
# Compared to pure uncertainty: 11% of each class... and it's better.
AccuracyStats(100.0/9.0, alldata@data[names(cmeans.mnlr@mu@data)])
# Compared to pure water: our classifier is a bit better!
uncertainty = alldata@data[names(cmeans.mnlr@mu@data)]
uncertainty$water = 100
uncertainty[,names(uncertainty) != "water"] = 0
AccuracyStats(uncertainty, alldata@data[names(cmeans.mnlr@mu@data)])

# Try normalising values
logitTransform = function(p) { log(p/(1-p)) }
asinTransform = function(p) { asin(sqrt(p)) }
pixelsTransformed = pixels
hist(asinTransform(pixels$red)); pixelsTransformed$red = asinTransform(pixels$red)
hist(asinTransform(pixels$nir)); pixelsTransformed$nir = asinTransform(pixels$nir)
hist(asinTransform(pixels$blue)); pixelsTransformed$blue = asinTransform(pixels$blue)
hist(asinTransform(pixels$swir)); pixelsTransformed$swir = asinTransform(pixels$swir)
hist(asinTransform(pixels$osavi))
hist(asinTransform(pixels$lswi))
hist(log(pixels$height))
hist(asinTransform(pixels$slope)); pixelsTransformed$slope = asinTransform(pixels$slope)
hist(pixels$aspect)
hist(pixels$tpi)

cmeans.mnlr.transf = spfkm(FullFormula, alldata[alldata$pure,], pixelsTransformed)

# Hard total classification accuracy: 11%, worse
sum(cmeans.mnlr.transf@predicted@data[[1]] == alldata$dominant) / length(alldata)
# If looking at only pure data: 20%, still really bad
sum(cmeans.mnlr.transf@predicted@data[alldata$pure,][[1]] == alldata[alldata$pure,]$dominant) / length(alldata[alldata$pure,])
# Marginally better, 37.6 rather than 38
AccuracyStats(cmeans.mnlr.transf@mu@data*100, alldata@data[names(cmeans.mnlr.transf@mu@data)])
# Compared to pure uncertainty: 11% of each class... and it's better.
AccuracyStats(100.0/9.0, alldata@data[names(cmeans.mnlr.transf@mu@data)])
# Compared to pure water: our classifier is a bit better!
AccuracyStats(uncertainty, alldata@data[names(cmeans.mnlr@mu@data)])

# Drop terms to find the best combination


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
sum(cmeans.wm@predicted@data[fold,][[1]] == alldata[fold,]$dominant) / length(alldata[fold,])

# RMSE
sqrt(mean(unlist(cmeans.wm@mu@data*100 - alldata@data[names(cmeans.wm@mu@data)])^2))
# MAE
mean(abs(unlist(cmeans.wm@mu@data*100 - alldata@data[names(cmeans.wm@mu@data)])))
# ME
mean(unlist(cmeans.wm@mu@data*100 - alldata@data[names(cmeans.wm@mu@data)]))
# These stats are better, especially RMSE, so fewer big mistakes
