# Fuzzy nearest prototype helper functions: calculate centroids based on actual weighted mean

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
