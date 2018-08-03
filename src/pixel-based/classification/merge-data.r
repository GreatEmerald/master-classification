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
write.csv(TrainingAndCovars, file.path(DataDir, "all.csv"))

TrainingAndCovars = LoadTrainingAndCovariates()
