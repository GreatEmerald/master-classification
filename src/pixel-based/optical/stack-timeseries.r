# Generates VRTs for all the Proba-V imagery to use as a stack.
library(gdalUtils)
library(raster)

DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
OutputDir = "../../userdata/master-classification/pixel-based/vrt/sr/"

TileList = expand.grid(paste0("X", 15:23), paste0("Y0", 5:6), stringsAsFactors = FALSE)

# Shared with clean-builtin; TODO: move to utils
lf = list.files(DataDir)
lf = lf[nchar(lf) == 4]
Col1Dirs = character()
for (dir in lf)
{
    lsf = list.files(paste0(DataDir, "/", dir))
    lsf = lsf[nchar(lsf) == 8]
    Col1Dirs = c(Col1Dirs, paste0(dir,'/',lsf))
}

TileFiles = list.files(file.path(DataDir, Col1Dirs), pattern=glob2rx("PROBAV_S5_TOC_X00Y04*RADIOMETRY.tif"),
                       recursive=TRUE, full.names = TRUE)
gdalbuildvrt(TileFiles, file.path(OutputDir, "Tile.vrt"), separate=TRUE)
