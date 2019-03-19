# Add additional bioclimatic parameters
# Based on dismo::biovars but on non-temperature/precipitation

AllClimateData = read.csv("../data/pixel-based/covariates/climate.csv")

srads = sprintf("wc2.0_30s_srad_%02d", 01:12)
AllClimateData$min.srad = apply(AllClimateData[,srads], 1, min, na.rm=TRUE)
AllClimateData[["min.srad"]][AllClimateData$min.srad == Inf] = NA
AllClimateData$max.srad = apply(AllClimateData[,srads], 1, max, na.rm=TRUE)
AllClimateData[["max.srad"]][AllClimateData$max.srad == Inf] = NA
AllClimateData$mean.srad = apply(AllClimateData[,srads], 1, mean, na.rm=TRUE)
AllClimateData[["mean.srad"]][AllClimateData$mean.srad == NaN] = NA

winds = sprintf("wc2.0_30s_wind_%02d", 01:12)
AllClimateData$min.wind = apply(AllClimateData[,winds], 1, min, na.rm=TRUE)
AllClimateData[["min.wind"]][AllClimateData$min.wind == Inf] = NA
AllClimateData$max.wind = apply(AllClimateData[,winds], 1, max, na.rm=TRUE)
AllClimateData[["max.wind"]][AllClimateData$max.wind == Inf] = NA
AllClimateData$mean.wind = apply(AllClimateData[,winds], 1, mean, na.rm=TRUE)
AllClimateData[["mean.wind"]][AllClimateData$mean.wind == NaN] = NA

vaprs = sprintf("wc2.0_30s_vapr_%02d", 01:12)
AllClimateData$min.vapr = apply(AllClimateData[,vaprs], 1, min, na.rm=TRUE)
AllClimateData[["min.vapr"]][AllClimateData$min.vapr == Inf] = NA
AllClimateData$max.vapr = apply(AllClimateData[,vaprs], 1, max, na.rm=TRUE)
AllClimateData[["max.vapr"]][AllClimateData$max.vapr == Inf] = NA
AllClimateData$mean.vapr = apply(AllClimateData[,vaprs], 1, mean, na.rm=TRUE)
AllClimateData[["mean.vapr"]][AllClimateData$mean.vapr == NaN] = NA

write.csv(AllClimateData, "../data/pixel-based/covariates/climate.csv", row.names=FALSE)
