# Process a QGIS-exported CSV file: sanity checks, hardening, purity determination
library(sp)

filename = "../data/samples.csv"
samples = read.csv(filename)
coordinates(samples) = ~X+Y
row.names(samples) = samples$pkuid
samples$pkuid = NULL

# Sanity check: all should add up to 100%
# "named integer(0)" is good and expected
which(rowSums(samples@data) != 100)

# Add "hardened" column: dominant land cover class
samples$dominant = factor(apply(samples@data, 1, which.max), labels = colnames(samples@data)[1:9], ordered = FALSE)
# Add a column that identifies endmembers
samples$pure = apply(samples@data[,1:9], 1, max) >= 90

# Statistics!
# "dominant" should be >=50 for all
table(samples$dominant)
# Should be >= 15 for all
table(samples[samples$pure,]$dominant)
# Progress indicator!
table(samples$dominant)/50*100
table(samples[samples$pure,]$dominant)/15*100
# What to work on next!
which.min(table(samples$dominant))
which.min(table(samples[samples$pure,]$dominant))
