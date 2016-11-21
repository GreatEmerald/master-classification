# Utility for returning all relevant classification accuracy statistics

AccuracyStats = function(predicted, observed)
{
    RMSE = sqrt(mean(unlist(predicted - observed)^2))
    MAE = mean(abs(unlist(predicted - observed)))
    ME = mean(unlist(predicted - observed))
    return(data.frame(RMSE, MAE, ME))
}
