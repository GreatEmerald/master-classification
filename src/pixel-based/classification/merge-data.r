# Merge all covariates into a single data.frame for classification

source("pixel-based/utils/load-sampling-data.r")

TrainingData = LoadGlobalTrainingData()
DataDir = "../data/pixel-based/covariates"
Harmonics = read.csv(file.path(DataDir, "harmonics.csv"))
names(Harmonics)[1] = "location_id"
SpectralCovars = read.csv(file.path(DataDir, "spectral.csv"))
names(SpectralCovars)[1] = "location_id"
TerrainCovars = read.csv(file.path(DataDir, "terrain.csv"))
names(TerrainCovars)[1] = "location_id"

TrainingAndCovars = merge(TrainingData, Harmonics, by="location_id") # silly that it's not vectoried
TrainingAndCovars = merge(TrainingAndCovars, SpectralCovars, by="location_id")
TrainingAndCovars = merge(TrainingAndCovars, TerrainCovars, by="location_id")

# Remove geometry, we read it from x/y instead
oldClass(TrainingAndCovars) = "data.frame"
TrainingAndCovars$geometry = NULL

# Remove samples for which we have no time series data at all (mean NDVI is NA)
TrainingAndCovars = TrainingAndCovars[!is.na(TrainingAndCovars$mean.ndvi),]

# Save
write.csv(TrainingAndCovars, file.path(DataDir, "all.csv"))

# Verify that we can load
LoadTest = LoadTrainingAndCovariates()
all.equal(LoadTest, TrainingAndCovars)
