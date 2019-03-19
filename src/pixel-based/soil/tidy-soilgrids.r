DataDir = "../data/pixel-based"

SoilCovars = read.csv(file.path(DataDir, "soil", "landgis-rerun.csv"))
SoilGrids = read.csv(file.path(DataDir, "soil", "soilgrids-rerun.csv"))
SoilGrids[SoilGrids==2^16-1] = NA

sum(complete.cases(SoilGrids)) / nrow(SoilGrids) # 39% of the cases are complete
CovarNAs = apply(SoilGrids, 2, function(x){sum(is.na(x)) / nrow(SoilGrids)})
sort(CovarNAs) # 2% missing from each, 50% missing from 8 covars, 12% from 7 more

ncol(SoilGrids) # 375
SmallerMatrix = SoilGrids[,-which(CovarNAs > 0.03)]
ncol(SmallerMatrix) # 336 covars
sum(complete.cases(SmallerMatrix)) / nrow(SmallerMatrix) # 92% of complete cases
nrow(SmallerMatrix)
# Drop taxonomy altogether
SmallerMatrix[,grep("TAX", names(SmallerMatrix))] = NULL
ncol(SmallerMatrix) # 152
sum(complete.cases(SmallerMatrix)) / nrow(SmallerMatrix) # Still 92% of complete cases

# Merge LandGIS with SoilGrids
MergedData = merge(SoilCovars, SmallerMatrix, by="location_id")
nrow(MergedData) == nrow(SmallerMatrix) # Nothing is lost (except points outside Africa in SoilCovars)
sum(complete.cases(MergedData)) / nrow(MergedData) # 92% of complete cases, very slightly fewer

# Write to file
write.csv(MergedData, file.path(DataDir, "covariates", "soil-merged.csv"), row.names=FALSE)

cor(MergedData[,c("sol_ph.0cm", "PHIHOX.M.sl1")], use="complete")
plot(sol_bulkdens.0cm~BLDFIE.M.sl1, MergedData)
plot(sol_bulkdens.10cm~BLDFIE.M.sl2, MergedData)
plot(sol_bulkdens.30cm~BLDFIE.M.sl4, MergedData)
plot(sol_bulkdens.60cm~BLDFIE.M.sl5, MergedData)
plot(sol_bulkdens.100cm~BLDFIE.M.sl6, MergedData)
plot(sol_bulkdens.200cm~BLDFIE.M.sl7, MergedData)
# We know bulk density correlates with water content
plot(sol_bulkdens.0cm~AWCtS.M.sl1, MergedData) # Little correlation
plot(BLDFIE.M.sl1~AWCtS.M.sl1, MergedData) # Extreme correlation
text(MergedData$AWCtS.M.sl1)

library(plotly)
plot_ly(MergedData, x=~BLDFIE.M.sl1, y=~AWCtS.M.sl1, text=~location_id)
plot_ly(MergedData, x=~sol_bulkdens.200cm, y=~AWCtS.M.sl1, text=~location_id)

plot(sol_clay.0cm~CLYPPT.M.sl1, MergedData)
plot(sol_clay.10cm~CLYPPT.M.sl2, MergedData)
plot(sol_clay.30cm~CLYPPT.M.sl4, MergedData)
plot(sol_clay.60cm~CLYPPT.M.sl5, MergedData)
plot(sol_clay.100cm~CLYPPT.M.sl6, MergedData)
plot(sol_clay.200cm~CLYPPT.M.sl7, MergedData)

plot(sol_sand.0cm~SNDPPT.M.sl1, MergedData)
plot(sol_sand.10cm~SNDPPT.M.sl2, MergedData)
plot(sol_sand.30cm~SNDPPT.M.sl4, MergedData)
plot(sol_sand.60cm~SNDPPT.M.sl5, MergedData)
plot(sol_sand.100cm~SNDPPT.M.sl6, MergedData)
plot(sol_sand.200cm~SNDPPT.M.sl7, MergedData)

plot(sol_organic.0cm~ORCDRC.M.sl1, MergedData)
plot(sol_organic.10cm~ORCDRC.M.sl2, MergedData)
plot(sol_organic.30cm~ORCDRC.M.sl4, MergedData)
plot(sol_organic.60cm~ORCDRC.M.sl5, MergedData)
plot(sol_organic.100cm~ORCDRC.M.sl6, MergedData)
plot(sol_organic.200cm~ORCDRC.M.sl7, MergedData)
hist(MergedData$sol_organic.200cm, breaks=100)
hist(MergedData$ORCDRC.M.sl7, breaks=100)
hist(log(MergedData$sol_organic.200cm+1), breaks=100)
hist(log(MergedData$ORCDRC.M.sl7+1), breaks=100)
plot(log(sol_organic.0cm+1)~log(ORCDRC.M.sl1+1), MergedData)
plot(log(sol_organic.10cm+1)~log(ORCDRC.M.sl2+1), MergedData)
plot(log(sol_organic.30cm+1)~log(ORCDRC.M.sl4+1), MergedData)
plot(log(sol_organic.60cm+1)~log(ORCDRC.M.sl5+1), MergedData)
plot(log(sol_organic.100cm+1)~log(ORCDRC.M.sl6+1), MergedData)
plot(log(sol_organic.200cm+1)~log(ORCDRC.M.sl7+1), MergedData)
# Not much correlation

plot(sol_ph.0cm~PHIHOX.M.sl1, MergedData)
plot(sol_ph.10cm~PHIHOX.M.sl2, MergedData)
plot(sol_ph.30cm~PHIHOX.M.sl4, MergedData)
plot(sol_ph.60cm~PHIHOX.M.sl5, MergedData)
plot(sol_ph.100cm~PHIHOX.M.sl6, MergedData)
plot(sol_ph.200cm~PHIHOX.M.sl7, MergedData)

# there is no correlation *at all*. Fantastic

# When I retry, the values are much different! So something must have changed in the mean while.
