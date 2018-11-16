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


## Same for validation

ValidationData = LoadGlobalValidationData()
DataDir = "../data/pixel-based/covariates"
Harmonics = read.csv(file.path(DataDir, "harmonics-validation.csv"))
names(Harmonics)[1] = "location_id"
SpectralCovars = read.csv(file.path(DataDir, "spectral-validation.csv"))
names(SpectralCovars)[1] = "location_id"
TerrainCovars = read.csv(file.path(DataDir, "terrain-validation.csv"))
names(TerrainCovars)[1] = "location_id"

ValidationAndCovars = merge(ValidationData, Harmonics, by="location_id") # silly that it's not vectoried
ValidationAndCovars = merge(ValidationAndCovars, SpectralCovars, by="location_id")
ValidationAndCovars = merge(ValidationAndCovars, TerrainCovars, by="location_id")

# Remove geometry, we read it from x/y instead
class(ValidationAndCovars) = "data.frame"
ValidationAndCovars$geometry = NULL

# Remove samples for which we have no time series data at all (mean NDVI is NA)
nrow(ValidationAndCovars) # 3616
ValidationAndCovars = ValidationAndCovars[!is.na(ValidationAndCovars$mean.ndvi),]
nrow(ValidationAndCovars) # 3604

# Fix the timestamps
TimestampsA = as.POSIXct(ValidationAndCovars$timestamp, "UTC", format="%Y-%m-%d %H:%M:%S")
TimestampsB = as.POSIXct(ValidationAndCovars$timestamp, "UTC", format="%d-%m-%Y %H:%M")
TimestampsA[is.na(TimestampsA)] = na.omit(TimestampsB)
ValidationAndCovars$timestamp = as.character(TimestampsA)

# Save
write.csv(ValidationAndCovars, file.path(DataDir, "all-validation.csv"))

# Verify that we can load
LoadTest = LoadValidationAndCovariates()
all.equal(LoadTest, ValidationAndCovars)
