# Extract coordinates from CSV files into tile-based coordinate lists
# Input: training, validation and prediction CSVs
# Output: one CSV file per tile with X and Y coordinates per line in WGS84

library(foreach)
source("pixel-based/utils/ProbaVTileID.r")

# Input
CSVFiles = list.files("../data/pixel-based/raw-points", pattern=glob2rx("*.csv"), full.names = TRUE)
CSVs = lapply(CSVFiles, read.csv)

# Merge all from these columns
colnames_x = c("X", "x", "subpix_mean_x")
colnames_y = c("Y", "y", "subpix_mean_y")

XYDF = foreach(CSV = CSVs, .combine=rbind) %do%
{
    Xs = CSV[[colnames_x[colnames_x %in% names(CSV)]]]
    Ys = CSV[[colnames_y[colnames_y %in% names(CSV)]]]
    XYDF = data.frame(x = Xs, y = Ys)
    XYDF$Tiles = ProbaVTileID(XYDF)
    XYDF
}

# Now split into tiles and save each as a CSV with just the coordinates
# We will match them back also via coordinates (with some let due to loss of precision)

if (!dir.exists("../work/extract-from"))
    dir.create("../work/extract-from", recursive=TRUE)

XYTiles = split(XYDF[,1:2], XYDF$Tiles)

foreach(XYTileIndex = 1:length(XYTiles)) %do%
{
    write.table(XYTiles[[XYTileIndex]],
        paste0("../work/extract-from/", names(XYTiles[XYTileIndex]) , ".csv"),
        row.names = FALSE, col.names = FALSE)
}
