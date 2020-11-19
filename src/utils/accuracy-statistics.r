# Utility for returning all relevant classification accuracy statistics

AccuracyStats = function(predicted, observed, relative=FALSE)
{
    RMSE = sqrt(mean(unlist(predicted - observed)^2))
    MAE = mean(abs(unlist(predicted - observed)))
    ME = mean(unlist(predicted - observed))
    Result = data.frame(RMSE, MAE, ME)
    if (relative)
        Result = Result/mean(unlist(observed))
    return(Result)
}

AccuracyStatTable = function(predicted, observed, relative=FALSE)
{
    Result = AccuracyStats(predicted, observed, relative=relative)
    row.names(Result) = "Overall"
    for (i in 1:ncol(observed))
    {
        RMSE = sqrt(mean(unlist(predicted[,i] - observed[,i])^2))
        MAE = mean(abs(unlist(predicted[,i] - observed[,i])))
        ME = mean(unlist(predicted[,i] - observed[,i]))
        ColResult = data.frame(RMSE, MAE, ME)
        if (relative)
            ColResult = ColResult/mean(unlist(observed[,i]))
        Result = rbind(Result, ColResult)
        row.names(Result)[i+1] = names(observed[i])
    }
    return(Result)
}

# Simply return a table of differences; used for ANOVA
CalcErrors = function(predicted, observed, ...)
{
    SE = unlist(predicted - observed)^2
    AE = abs(unlist(predicted - observed))
    return(data.frame(AE, SE, ...))
}
    