# Testing harmonics in order to understand whether they make any sense whatsoever
library(probaV)
library(zoo)
library(HarmonicRegression)
source("utils/set-temp-path.r")
source("utils/load-data.r")

OutputDir = "../../userdata/timeseries/"
OutputFile = paste0(OutputDir, "harmonics.envi")
LogFile = paste0(OutputDir, "harmonics.log")

TileOfInterest = "X20Y01"
SemiCleanDir = "../../userdata/semicleaned/"
BandPattern = "(NDVI)_sm.tif$"
VrtFilename = paste0(OutputDir, "harmonic-test.vrt")

tau = 2*pi

Vrt = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = TRUE)

TS = timeVrtProbaV(SemiCleanDir, pattern = BandPattern, vrt_name = VrtFilename, tile = TileOfInterest,
    return_raster = FALSE)

# Try a pure cropland
Samples = LoadClassificationData()
z = zoo(c(Vrt[Samples[which.max(Samples$water),]$cell.no]), as.Date(getZ(Vrt), "%Y%j"))

# use smoothloes to create time series

# retrieve band and date information from dataframe
Bands = TS[TS$date == TS$date[1], 'band']
Dates = TS[TS$band == Bands[1], 'date']

# We excluded the threshold parameter and simply plot the output of the smoothLoess function. 
f = smoothLoess(tsx = z, QC_good=NULL, dates=Dates, res_type=c("all"), span=0.3)
plot(f, main = "plotting time series.
     x = NDVI values for a given time,
     QC_good = Quality value, where 0 is missing and 1 is available,
     filled = the result of smoothing from the polynomial regression loess model", xlab = "Dates index")

model_metrics = getHarmMetrics(f$x, dates=Dates, QC_good = f$QC_good, sig = .95, order = 2, return_model = TRUE)
model_metrics
anova(model_metrics)
fit = fitted(model_metrics)
originaldata = na.omit(as.numeric(f$x))
indices = as.integer(names(fit))
plot(fit~indices)
plot(originaldata~indices, col="red", type="l")
phaser = function(co, si)
{
    return(atan2(si, co) %% tau)
}
amplituder = function(co, si)
{
    return(sqrt(co^2 + si^2))
}
phaser(model_metrics$coefficients[["co"]], model_metrics$coefficients[["si"]])
amplituder(model_metrics$coefficients[["co"]], model_metrics$coefficients[["si"]])
phaser(model_metrics$coefficients[["co2"]], model_metrics$coefficients[["si2"]])
amplituder(model_metrics$coefficients[["co2"]], model_metrics$coefficients[["si2"]])

HR = harmonic.regression(as.matrix(f$x), lubridate::decimal_date(Dates), Tau=1)
plot(HR$fit.vals~lubridate::decimal_date(Dates))
plot(HR$norm.fit.vals~lubridate::decimal_date(Dates))
HR$coeffs
HR$pars
HR$pvals < 0.05
phaser(HR$coeffs[2], HR$coeffs[3])
amplituder(HR$coeffs[2], HR$coeffs[3])

# Apply phaser and amplituder to our test data
LakeCoeffs = brick(OutputFile)
# min max intercept co si co2 si2 co3 si3 trend (blue, NDVI)
spplot(LakeCoeffs[[20]])
p1 = phaser(LakeCoeffs[[14]], LakeCoeffs[[15]])
spplot(p1)
a1 = amplituder(LakeCoeffs[[14]], LakeCoeffs[[15]])
spplot(a1)
p2 = phaser(LakeCoeffs[[16]], LakeCoeffs[[17]])
spplot(p2)
a2 = amplituder(LakeCoeffs[[16]], LakeCoeffs[[17]])
spplot(a2)
p3 = phaser(LakeCoeffs[[18]], LakeCoeffs[[19]])
spplot(p3)
a3 = amplituder(LakeCoeffs[[18]], LakeCoeffs[[19]])
spplot(a3)
brick(p1, a1, p2, a2, p3, a3, filename=paste0(OutputDir, "phase-amplitude.tif"))
