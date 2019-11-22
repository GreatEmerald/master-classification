# Script for filtering temporal outliers

#library(probaV)
library(foreach)
library(sf)
library(pbapply)
source("pixel-based/utils/load-sampling-data.r")
source("pixel-based/utils/ProbaVDataDirs.r")
source("pixel-based/utils/db-io.r")

OutputDB = "../data/pixel-based/timeseries/timeseries-cloudfree.gpkg"
InputDB = "../data/pixel-based/timeseries/timeseries-masked.gpkg"

#TileList = GetTileList(LoadGlobalRasterPoints())
Dates = LoadRawDataDirs()$date
ColDates = paste0("X", gsub("-", "", Dates))

# Modified probaV::smoothLoess to plot the result
smoothLoessPlot = function (tsx, QC_good = NULL, dates = NULL, threshold = c(-50, 
  Inf), res_type = c("distance", "sd_distance", "all", "filled", 
  "omit", "QC"), span=0.3, family="gaussian", threshstat="none", plot=TRUE, ...) 
{
    if (is.null(QC_good)) {
        QC_good <- as.numeric(!is.na(tsx))
    }
    else {
        QC_good <- as.numeric(QC_good)
    }
    x <- as.numeric(tsx)
    x[QC_good == 0] <- NA
    if (all(is.na(x)))
    {
        warning("Input is all NA")
        return(x)
    }
    if (plot)
        plot(x, type="o", ...)
    if (is.null(dates)) {
        dates <- index(tsx)
    }
    dates <- as.numeric(dates)
    loe <- try(loess(formula = x ~ dates, na.action = "na.omit", span=span, family=family))
    if (class(loe) == "try-error")
        return(x)
    loe_pred <- predict(loe, dates)
    if (plot)
        lines(loe_pred, col="green")
    distance <- (loe_pred - x)
    predmae = mean(abs(distance), na.rm=TRUE)
    predrmse = sqrt(mean(distance^2, na.rm=TRUE))
    xsd = sd(x, na.rm=TRUE)
    xmad = mad(x, na.rm=TRUE)
    if (plot)
        title(sub=paste("MAE:", round(predmae), "RMSE:", round(predrmse), "sd:", round(xsd), "mad:", round(xmad)))
    threshstat = switch(threshstat, none=1, sd=xsd, mad=xmad, mae=predmae, rmse=predrmse)
    threshold = threshold * threshstat
    if (!is.null(threshold)) {
        QC_good[distance < threshold[1] & !is.na(distance)] <- 2
        QC_good[distance > threshold[2] & !is.na(distance)] <- 2
    }
    if (class(tsx) == "zoo") {
        tsx <- zoo(cbind(x = as.numeric(tsx), QC_good, filled = loe_pred), 
                   index(tsx))
        return(tsx)
    }
    else {
        x_omit <- x
        x_omit[QC_good != 1] <- NA
        if (plot)
            points(x_omit, type="o", col="red")
        res <- switch(res_type, all = data.frame(x = as.numeric(tsx), 
                                                 QC_good = QC_good, filled = loe_pred, distance = round(distance)), 
                      filled = loe_pred, omit = x_omit, QC = QC_good, distance = distance, 
                      sd_distance = (distance/sd(x, na.rm = T)))
        return(res)
    }
}

BlueSF = st_read(InputDB, "RADIOMETRY-3") # Red, NIR, Blue, SWIR
BlueXY = data.frame(X=BlueSF$X, Y=BlueSF$Y)
gc(TRUE)
BlueMatrix = as.matrix(as.data.frame(BlueSF)[,ColDates])
rm(BlueSF)
gc(TRUE)

## Test thresholding ##
TestTS = sample(1:nrow(BlueMatrix), 50)
for (i in TestTS)
    smoothLoessPlot(BlueMatrix[i,], dates=Dates, res_type="omit", span=0.2, threshold=c(-100, 90), family="symmetric", main=i)

for (i in TestTS)
    smoothLoessPlot(BlueMatrix[i,], dates=Dates, res_type="omit", span=0.2, threshold=c(-3, 4), family="symmetric", main=i, threshstat = "mae")

for (i in TestTS)
    smoothLoessPlot(BlueMatrix[i,], dates=Dates, res_type="omit", span=0.2, threshold=c(-2, 2), family="symmetric", main=i, threshstat = "rmse")

for (i in TestTS)
    smoothLoessPlot(BlueMatrix[i,], dates=Dates, res_type="omit", span=0.2, threshold=c(-3, 4), family="symmetric", main=i, threshstat = "mad")

for (i in TestTS)
    smoothLoessPlot(BlueMatrix[i,], dates=Dates, res_type="omit", span=0.2, threshold=c(-2, 2), family="symmetric", main=i, threshstat = "sd")


i=297100
i=250595 # test bottom
i=146733
i=210376
i=214477 # Test bottom
i=207984
i=357404 # Snow!
i=29110
i=242024
i=279749
i=283071
i=216074
i=181

# sd with -2, 2 looks best

## End thresholding ##

# Get blue mask
CloudMask = pbapply(BlueMatrix, 1, smoothLoessPlot, dates=Dates, res_type="omit", span=0.2, threshstat = "sd", threshold=c(-2, 2), family="symmetric", plot=FALSE)
CloudMask = t(CloudMask)

# Statistics
mean(is.na(BlueMatrix)) # 60.2% of all data is NA
mean(is.na(CloudMask)) # 61.4% of all data is now NA, so we only removed 1%

rm(BlueMatrix)
gc(TRUE)

pb = txtProgressBar(1, 5, style = 3)
for (VI in c("NDVI", paste0("RADIOMETRY-", 1:4)))
{
    InputSF = st_read(InputDB, VI)
    stopifnot(all(InputSF$X == BlueXY$X))
    InputMatrix = as.matrix(as.data.frame(InputSF)[,ColDates])
    InputMatrix[is.na(CloudMask)] = NA
    InputSF[,ColDates] = InputMatrix
    rm(InputMatrix)
    gc(TRUE)
    st_write(InputSF, OutputDB, VI)
    rm(InputSF)
    gc(TRUE)
    setTxtProgressBar(pb, pb$getVal()+1)
}
close(pb)
