# Classify images based on the fuzzy c-means algorithm
library(Hmisc)
library(GSIF)
library(caret)
library(ggplot2)
source("utils/load-data.r")
source("utils/accuracy-statistics.r")

OutputDir = "../data/"

# Load data
alldata = LoadClassificationData()
alldata$dominant = as.character(alldata$dominant)
pixels = LoadTrainingPixels(exclude="is.water") # Not excluding is.water leads to very strange issues
# Sort pixels to match our dataframe
pixels = pixels[match(alldata@data$cell.no, pixels@grid.index),]

set.seed(0xc0ffeed)
folds = createFolds(alldata$cropland, 4)

# There are two ways: either use the built-in method of using a multinomial logistic regression
# with endmembers, or use weighted averages with all pixels.

# Start with builtin

terms = names(pixels)
FullFormula = formula(paste0("dominant~", paste(terms, collapse="+")))

cmeans.mnlr = spfkm(FullFormula, alldata[alldata$pure,], pixels)

# Hard total classification accuracy: 53%, pretty good
sum(cmeans.mnlr@predicted@data[[1]] == alldata@data[["dominant"]]) / length(alldata)
# RMSE of 23.61
AccuracyStats(cmeans.mnlr@mu@data*100, alldata@data[, names(cmeans.mnlr@mu@data)])
# Compared to pure uncertainty: 11% of each class is worse at 27.38
AccuracyStats(100.0/9.0, alldata@data[names(cmeans.mnlr@mu@data)])
# Compared to pure water: our classifier is much better!
uncertainty = alldata@data[names(cmeans.mnlr@mu@data)]
uncertainty$water = 100
uncertainty[,names(uncertainty) != "water"] = 0
AccuracyStats(uncertainty, alldata@data[names(cmeans.mnlr@mu@data)])

# Try normalising values
logitTransform = function(p) { log((p/max(p))/(1-(p/max(p)))) }
asinTransform = function(p) { asin(sqrt(p/max(p))) }
pixelsTransformed = pixels
hist(asinTransform(pixels$red)); pixelsTransformed$red = asinTransform(pixels$red)
hist(pixels$nir)
hist(asinTransform(pixels$blue)); pixelsTransformed$blue = asinTransform(pixels$blue)
hist(pixels$swir)
hist(asinTransform(pixels$osavi))
hist(pixels$lswi)
hist(pixels$height)
hist(asinTransform(pixels$slope)); pixelsTransformed$slope = asinTransform(pixels$slope)
hist(pixels$aspect)
hist(pixels$tpi)
hist(pixels$mean.ndvi)
hist(pixels$phase1)
hist(asinTransform(pixels$amplitude1)); pixelsTransformed$amplitude1 = asinTransform(pixels$amplitude1)
hist(pixels$phase2)
hist(asinTransform(pixels$amplitude2)); pixelsTransformed$amplitude2 = asinTransform(pixels$amplitude2)

transdata = alldata
transdata$red = asinTransform(transdata$red)
transdata$blue = asinTransform(transdata$blue)
transdata$slope = asinTransform(transdata$slope)
transdata$amplitude1 = asinTransform(transdata$amplitude1)
transdata$amplitude2 = asinTransform(transdata$amplitude2)

cmeans.mnlr.transf = spfkm(FullFormula, transdata[transdata$pure,], pixelsTransformed)

# Marginally better, 23.27 rather than 23.61
AccuracyStats(cmeans.mnlr.transf@mu@data*100, alldata@data[, names(cmeans.mnlr.transf@mu@data)])
# Compared to pure uncertainty: 11% of each class
AccuracyStats(100.0/9.0, alldata@data[names(cmeans.mnlr.transf@mu@data)])
# Compared to pure water
AccuracyStats(uncertainty, alldata@data[names(cmeans.mnlr@mu@data)])

# What about fuzzification?

step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), fuzzy.e = numeric())
workingsteps = seq(1.1, 10, 0.1)
for (fuzz in workingsteps)
{
    cmeans = spfkm(FullFormula, alldata[alldata$pure,], pixels, fuzzy.e = fuzz)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[, names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, fuzzy.e = fuzz))
}
View(step[order(step$RMSE),])
# Best RMSE 21.78 at 1.5, this makes a lot of difference

# Drop terms to find the best combination

StepFormula = FullFormula
CmeansStep = function(StepFormula, class.c = NULL, class.sd = NULL, subset = alldata$pure)
{
    terms = labels(terms(StepFormula))
    step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), Formula = character())
    for (term in terms)
    {
        Formula = update.formula(StepFormula, paste0("~ . -", term))
        print(Formula)
        try({
            cmeans <- spfkm(Formula, alldata[subset,], pixels, class.c = class.c, class.sd = class.sd, fuzzy.e=1.5)
            AS = AccuracyStats(cmeans@mu@data*100, alldata@data[, names(cmeans@mu@data)])
            step = rbind(step, data.frame(AS, Formula = Reduce(paste0, deparse(Formula))))
        })
    }
    return(step)
}
step = CmeansStep(StepFormula)
View(step[order(step$RMSE),])
# Best RMSE (21.21) when dropping blue. Doesn't bias it either.
StepFormula = update.formula(FullFormula, "~ . -blue")
step = CmeansStep(StepFormula)
View(step[order(step$RMSE),])
# Best unbiased RMSE (21.00) when dropping aspect
StepFormula = update.formula(StepFormula, "~ . -aspect")
step = CmeansStep(StepFormula)
View(step[order(step$RMSE),])
# Nothing better than dominant ~ red + nir + swir + osavi + lswi + height + slope + 
#    tpi + mean.ndvi + phase1 + amplitude1 + phase2 + amplitude2. Stop.

# So that doesn't help much at all, less than 1 RMSE.

# Now with weighted means
GetClassMeans = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:28)
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

GetClassSDs = function(samples = 1:nrow(alldata), validation.idx = 1:9, training.idx = 13:28)
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

TestFormula = formula("dominant ~ red + nir + blue + swir + osavi + lswi + height + 
    slope + aspect + tpi + mean.ndvi + phase1 + amplitude1 + 
    phase2 + amplitude2")
cmeans.wm = spfkm(FullFormula, alldata[fold,], pixels,
    class.c = GetClassMeans(), class.sd = GetClassSDs())

# Hard total classification accuracy: 49%, slightly worse
sum(cmeans.wm@predicted@data[[1]] == alldata@data[order(alldata@data$cell.no),"dominant"]) / length(alldata)

# RMSE of 24.37, also slightly worse
AccuracyStats(cmeans.wm@mu@data*100, alldata@data[order(alldata@data$cell.no),names(cmeans.wm@mu@data)])
AccuracyStatTable(cmeans.wm@mu@data*100, alldata@data[order(alldata@data$cell.no),names(cmeans.wm@mu@data)])
# Compared to everything 11%: RMSE of 27.4
AccuracyStats(100.0/9.0, alldata@data[names(cmeans.wm@mu@data)])

qplot(blue, swir, data=alldata@data[alldata@data$pure,], colour=dominant)
qplot(osavi, mean.ndvi, data=alldata@data[alldata@data$pure,], colour=dominant)

# Fuzz
step = data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric(), fuzzy.e = numeric())
workingsteps = seq(1.1, 10, 0.1)
for (fuzz in workingsteps)
{
    cmeans = spfkm(FullFormula, alldata, pixels, class.c = GetClassMeans(), class.sd = GetClassSDs(), fuzzy.e = fuzz)
    AS = AccuracyStats(cmeans@mu@data*100, alldata@data[order(alldata@data$cell.no), names(cmeans@mu@data)])
    step = rbind(step, data.frame(AS, fuzzy.e = fuzz))
}
View(step[order(step$RMSE),])
# RMSE of 22.32, best consistently at 1.5

# Try dropping using this method
StepFormula = FullFormula
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping amplitude1: 21.99
StepFormula = update.formula(StepFormula, "~ . -amplitude1")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping blue: 21.84
StepFormula = update.formula(StepFormula, "~ . -blue")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping amplitude2: 21.69
StepFormula = update.formula(StepFormula, "~ . -amplitude2")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping tpi: 21.60
StepFormula = update.formula(StepFormula, "~ . -tpi")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping red: 21.57
StepFormula = update.formula(StepFormula, "~ . -red")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping lswi: 21.54
StepFormula = update.formula(StepFormula, "~ . -lswi")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping aspect: 21.51
StepFormula = update.formula(StepFormula, "~ . -aspect")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# Dropping swir: 21.44
StepFormula = update.formula(StepFormula, "~ . -swir")
step = CmeansStep(StepFormula, class.c = GetClassMeans(), class.sd = GetClassSDs(), subset=TRUE)
View(step[order(step$RMSE),])
# No more improvements, stop. Best formula:
# dominant ~ nir + osavi + height + slope + mean.ndvi + phase1 + phase2

cmeans.wm = spfkm(StepFormula, alldata[fold,], pixels,
    class.c = GetClassMeans(), class.sd = GetClassSDs())
AccuracyStatTable(cmeans.wm@mu@data*100, alldata@data[names(cmeans.wm@mu@data)])

qplot(aspect, tpi, data=alldata@data[alldata@data$pure,], colour=dominant)

# Repeat with 4-fold cross-validation
CMCV = function(StepFormula, filename=paste0(OutputDir, "stat-cmeans.csv"), weighted=FALSE, ...)
{
    RoundPrediction = data.frame()
    for (i in 1:length(folds))
    {
        if (weighted)
            cmeans = spfkm(StepFormula, alldata[-folds[[i]],], pixels,
                class.c = GetClassMeans(-folds[[i]]), class.sd = GetClassSDs(-folds[[i]]), ...)
        else
            cmeans = spfkm(StepFormula, alldata[-folds[[i]],], pixels, ...)
        if (nrow(RoundPrediction) == 0)
            RoundPrediction = cmeans@mu@data[folds[[i]],]*100
        else
            RoundPrediction = rbind(RoundPrediction, cmeans@mu@data[folds[[i]],]*100)
    }
    Validator = alldata@data[unlist(folds), names(cmeans@mu@data)]
    AST = AccuracyStatTable(RoundPrediction, Validator)
    print(AST)
    plot(unlist(RoundPrediction), unlist(Validator))
    write.csv(AST, filename)
}

# Unoptimised
CMCV(FullFormula, filename = paste0(OutputDir, "stat-cmeans-unoptimised.csv"))#, class.c = GetClassMeans(), class.sd = GetClassSDs())
# Optimised
OptimalFormula1 = formula("dominant ~ red + nir + swir + osavi + lswi + height + slope + tpi + mean.ndvi + phase1 + amplitude1 + phase2 + amplitude2")
OptimalFormula2 = formula("dominant ~ nir + osavi + height + slope + mean.ndvi + phase1 + phase2")
CMCV(OptimalFormula2, fuzzy.e=1.5, weighted=TRUE)

# Train on pure, predict fuzzy
folds=list(which(!alldata@data$pure))
CMCV(FullFormula, filename = paste0(OutputDir, "stat-cmeans-pure-unoptimised.csv"))
CMCV(OptimalFormula2, fuzzy.e=1.5, weighted=TRUE,  filename = paste0(OutputDir, "stat-cmeans-pure.csv"))
