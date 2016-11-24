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
FullFormula = update.formula(FullFormula, "~ . -is.water") # For some reason is.water gives an error, drop

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
hist(pixels$is.water)

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
terms = labels(terms(FullFormula))
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
for (term in terms)
{
    Formula = update.formula(FullFormula, paste0("~ . -", term))
    print(Formula)
    cmeans = spfkm(Formula, alldata[alldata$pure,], pixelsTransformed)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
}
# Best RMSE (37.5) when dropping OSAVI. Doesn't bias it either.
StepFormula = update.formula(FullFormula, "~ . -osavi")
terms = labels(terms(StepFormula))
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
for (term in terms)
{
    Formula = update.formula(StepFormula, paste0("~ . -", term))
    print(Formula)
    cmeans = spfkm(Formula, alldata[alldata$pure,], pixelsTransformed)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
}
# Best unbiased RMSE (36.9) when dropping LSWI
StepFormula = update.formula(StepFormula, "~ . -lswi")
terms = labels(terms(StepFormula))
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
for (term in terms)
{
    Formula = update.formula(StepFormula, paste0("~ . -", term))
    print(Formula)
    cmeans = spfkm(Formula, alldata[alldata$pure,], pixelsTransformed)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
}
# Best unbiased RMSE (36.7) when dropping blue
StepFormula = update.formula(StepFormula, "~ . -blue")
terms = labels(terms(StepFormula))
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
for (term in terms)
{
    Formula = update.formula(StepFormula, paste0("~ . -", term))
    print(Formula)
    cmeans = spfkm(Formula, alldata[alldata$pure,], pixelsTransformed)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
}
# Nothing better than dominant ~ red + nir + swir + height + slope + aspect + tpi. Stop.
StepFormula = update.formula(StepFormula, "~ . -tpi")
terms = labels(terms(StepFormula))
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
for (term in terms)
{
    Formula = update.formula(StepFormula, paste0("~ . -", term))
    print(Formula)
    cmeans = spfkm(Formula, alldata[alldata$pure,], pixelsTransformed)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
}

# So that doesn't help much at all. But the fuzzification factor may change things quite a bit.
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), fuzzy.e = numeric())
workingsteps = seq(0, 10, 0.1)
workingsteps = workingsteps[!workingsteps %in% 1.0] # Skip some due to errors
for (fuzz in workingsteps)
{
    cmeans = spfkm(FullFormula, alldata[alldata$pure,], pixelsTransformed, fuzzy.e = fuzz)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, fuzzy.e = fuzz))
}

# Now with weighted means

set.seed(0xc0ffeed)
folds = createFolds(alldata$cropland, 2)
fold = folds$Fold1

GetClassMeans = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:23)
{
    combos = expand.grid(validation=validation.idx, training=training.idx)
    ClassMeans = function(x)
    {
        wtd.mean(alldata@data[samples,x["training"]], alldata@data[samples,x["validation"]])
    }
    cm = apply(combos, 1, ClassMeans)
    c.means = matrix(cm, nrow=length(validation.idx),
        dimnames=list(names(alldata)[validation.idx], names(alldata)[training.idx]))
    return(c.means)
}

GetClassSDs = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:23)
{
    combos = expand.grid(validation=validation.idx, training=training.idx)
    ClassSDs = function(x)
    {
        sqrt(wtd.var(alldata@data[samples,x["training"]], alldata@data[samples,x["validation"]],
            normwt = TRUE))
    }
    csd = apply(combos, 1, ClassSDs)
    c.sds = matrix(csd, nrow=length(validation.idx),
        dimnames=list(names(alldata)[validation.idx], names(alldata)[training.idx]))
    return(c.sds)
}

cmeans.wm = spfkm(FullFormula, alldata[fold,], pixels,
    class.c = GetClassMeans(fold), class.sd = GetClassSDs(fold))

# Hard total classification accuracy: 11%, even worse
sum(cmeans.wm@predicted@data[[1]] == alldata$dominant) / length(alldata)
# If looking at input data: 14%, still really bad
sum(cmeans.wm@predicted@data[fold,][[1]] == alldata[fold,]$dominant) / length(alldata[fold,])

# These stats are better, especially RMSE, so fewer big mistakes
AccuracyStats(cmeans.wm@mu@data*100, alldata@data[names(cmeans.wm@mu@data)])

step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), fuzzy.e = numeric())
workingsteps = seq(0, 10, 0.1)
workingsteps = workingsteps[!workingsteps %in% 1.0] # Skip 1.0, that errors
for (fuzz in seq(1.1, 10, 0.1))
{
    cmeans = spfkm(FullFormula, alldata[fold,], pixels, fuzzy.e = fuzz,
        class.c = GetClassMeans(fold), class.sd = GetClassSDs(fold))
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, fuzzy.e = fuzz))
}

AccuracyStats(100.0/9.0, alldata@data[names(cmeans@mu@data)])
# Increasing fuzzification helps... but not as much as pure fuzz. Welp.
