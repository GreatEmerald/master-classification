DataDir = "../data/pixel-based"

SoilCovars = read.csv(file.path(DataDir, "covariates", "landgis.csv"))
SoilGrids = read.csv(file.path(DataDir, "soil", "soilgrids.csv"))


sum(complete.cases(SoilGrids)) / nrow(SoilGrids) # 39% of the cases are complete
CovarNAs = apply(SoilGrids, 2, function(x){sum(is.na(x)) / nrow(SoilGrids)})
sort(CovarNAs) # 2% missing from each, 50% missing from 8 covars, 12% from 7 more

SmallerMatrix = SoilGrids[,-which(CovarNAs > 0.0222)]
ncol(SmallerMatrix) # 324 covars
sum(complete.cases(SmallerMatrix)) / nrow(SmallerMatrix) # 96% of complete cases
nrow(SmallerMatrix)

# Merge LandGIS with SoilGrids

MergedData = merge(SoilCovars, SmallerMatrix, by="location_id") # Nothing is lost
sum(complete.cases(MergedData)) / nrow(MergedData) # 94% of complete cases

# Drop taxonomy
MergedData[,grep("TAX", names(MergedData))] = NULL

cor(MergedData[,c("sol_ph.0cm", "PHIHOX.M.sl1")], use="complete")
plot(sol_bulkdens.0cm~BLDFIE.M.sl1, MergedData)
plot(sol_bulkdens.10cm~BLDFIE.M.sl2, MergedData)
plot(sol_bulkdens.30cm~BLDFIE.M.sl4, MergedData)
plot(sol_bulkdens.60cm~BLDFIE.M.sl5, MergedData)
plot(sol_bulkdens.100cm~BLDFIE.M.sl6, MergedData)
plot(sol_bulkdens.200cm~BLDFIE.M.sl7, MergedData)

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

plot(sol_ph.0cm~PHIHOX.M.sl1, MergedData)
plot(sol_ph.10cm~PHIHOX.M.sl2, MergedData)
plot(sol_ph.30cm~PHIHOX.M.sl4, MergedData)
plot(sol_ph.60cm~PHIHOX.M.sl5, MergedData)
plot(sol_ph.100cm~PHIHOX.M.sl6, MergedData)
plot(sol_ph.200cm~PHIHOX.M.sl7, MergedData)

# there is no correlation *at all*. Fantastic
