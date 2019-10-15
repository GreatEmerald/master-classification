# Build VRTs for each tile, taking datadirs into account

library(gdalUtils)
source("pixel-based/utils/ProbaVDataDirs.r")

TileDir = "../work/extract-from"
DataDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M"
VRTDir = "../work/raw-vrts"
VIs = c("SM", "RADIOMETRY", "NDVI")
DataDirs = LoadRawDataDirs("../data/pixel-based/DataDirs-NG.csv", FALSE) # There was some bug tha caused all VRTs to be from all dates

if (!dir.exists(VRTDir))
    dir.create(VRTDir, recursive = TRUE)

# We need a list of tiles; read from the work dir, as we have all the CSVs with the tile name
Tiles = substr(list.files(TileDir), 1, 6)


# Then, stack each tile into VRTs per VI.
for (Tile in Tiles)
{
    for (VI in VIs)
    {
        VIFiles = list.files(DataDir, glob2rx(paste0("PROBAV_S5_TOC_", Tile, "_????????_100M_V???_", VI, ".tif")), full.names = TRUE, recursive = TRUE)
        if (VI == "RADIOMETRY") # 4 bands, so can't stack this
        {
            for (Band in 1:4)
            {
                VRTFile = paste0(VRTDir, "/", Tile, "-", VI, "-", Band, ".vrt")
                if (!file.exists(VRTFile))
                {
                    gdalbuildvrt(VIFiles, VRTFile, separate = TRUE, b = Band)
                    if (Band != 1)
                        system(paste0('sed -i "s|<SourceBand>1</SourceBand>|<SourceBand>', Band ,'</SourceBand>|" ', VRTFile))
                }
            }
        } else {
            VRTFile = paste0(VRTDir, "/", Tile, "-", VI, ".vrt")
            if (!file.exists(VRTFile))
                gdalbuildvrt(VIFiles, VRTFile, separate = TRUE)
        }
    }
}
