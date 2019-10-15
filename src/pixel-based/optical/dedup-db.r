# Snippet for removing duplicates from geodatabases
library(sf)
for (VI in c("NDVI", "SM", paste0("RADIOMETRY-", 1:4)))
{
    AllPoints = st_read("../data/pixel-based/timeseries/timeseries.gpkg", VI)
    Dedup = !duplicated(AllPoints[c("X", "Y")])
    st_write(AllPoints[Dedup,], "../data/pixel-based/timeseries/timeseries-dedup.gpkg", VI)
    rm(AllPoints, Dedup)
}

for (VI in c("NDVI", "SM", paste0("RADIOMETRY-", 1:4)))
{
    AllPoints = st_read("../data/pixel-based/timeseries/timeseries.gpkg", VI)
    Dedup = !is.na(AllPoints$Y)
    st_write(AllPoints[Dedup,], "../data/pixel-based/timeseries/timeseries-dedup.gpkg", VI)
    rm(AllPoints, Dedup)
}
