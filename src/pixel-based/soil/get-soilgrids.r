library(rjson)
source("pixel-based/utils/load-sampling-data.r")

Data.df = LoadTrainingAndCovariates()

SGURL = function(points)
{
    return(paste0("https://rest.soilgrids.org/query?lon=", points$x, "&lat=", points$y))
}

urls = SGURL(Data.df)

GetSGMatrix = function(url)
{
    SGData = rjson::fromJSON(RCurl::getURL(url))
    QR = unlist(SGData$properties)
    RRow = as.numeric(QR)
    return(matrix(RRow, ncol=length(RRow), dimnames=list(NULL, names(QR))))
}
MyResults = lapply(urls, GetSGMatrix)

MyMatrix = plyr::rbind.fill.matrix(MyResults)

EmptyCovars = apply(MyMatrix, 2, function(x){all(is.na(x))}) # Drop empty (=non-numeric) columns
MyMatrix = MyMatrix[,!EmptyCovars] # 390 covars

# Remove covariates with no variance (all equal values)
Variances = apply(MyMatrix, 2, sd, na.rm=TRUE)
MyMatrix = MyMatrix[,Variances!=0]
write.csv(MyMatrix, "../data/soilgrids.csv")

sum(complete.cases(MyMatrix)) / nrow(MyMatrix) # 55% of the cases are complete
CovarNAs = apply(MyMatrix, 2, function(x){sum(is.na(x)) / nrow(MyMatrix)})
sort(CovarNAs) # 2% missing from each, 50% missing from 8 covars, 12% from 7 more

SmallerMatrix = MyMatrix[,-which(CovarNAs > 0.1)] # 374 covars
sum(complete.cases(SmallerMatrix)) / nrow(SmallerMatrix) # 98% of complete cases


library(nortest)
Normality = apply(SmallerMatrix, 2, function(x){lillie.test(na.omit(x))$p.value})

