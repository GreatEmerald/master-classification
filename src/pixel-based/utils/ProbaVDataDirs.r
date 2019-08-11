# Build a list of Proba-V directories that have complete content, and save it into a file for later reference

# Get a list of all Proba-V data directories
ProbaVDataDirs = function(ProductDir = "/data/MTDA/TIFFDERIVED/PROBAV_L3_S5_TOC_100M")
{
    lf = list.dirs(ProductDir, recursive=FALSE)
    lf = lf[nchar(basename(lf)) == 4] # Filter to YYYY, should be no-op
    lsf = list.dirs(lf, recursive=FALSE)
    lsf = lsf[nchar(basename(lsf)) == 8] # YYYYMMDD
    return(lsf)
}

ProbaVValidDirs = function(RequiredFiles = c("RADIOMETRY", "NDVI", "SM"), RequiredTiles=c("X16Y06", "X17Y06"), ...)
{
    DataDirs = ProbaVDataDirs(...)
    
    SearchPattern = paste0("PROBAV_S5_TOC_(", paste(RequiredTiles, collapse = "|"), ").*(",
                           paste(RequiredFiles, collapse = "|"), ").tif")
    
    ValidDirs = character()
    for (DataDir in DataDirs)
    {
        CollectionDirs = list.dirs(DataDir, recursive=FALSE) # There may be several collections, keep the last complete on the list
        ValidCollectionDir = character()
        for (CollectionDir in CollectionDirs)
        {
            CollectionFiles = list.files(CollectionDir, pattern=SearchPattern)
            if (length(CollectionFiles) == length(RequiredFiles) * length(RequiredTiles))
                ValidCollectionDir = CollectionDir
            else {
                print(paste("Collection directory", CollectionDir, "does not contain required files, skipping."))
                for (Tile in RequiredTiles)
                {
                    if (!any(grep(Tile, CollectionFiles)))
                        print(paste("Tile not found:", Tile))
                }
            }
        }
        if (length(ValidCollectionDir) > 0)
            ValidDirs = c(ValidDirs, ValidCollectionDir)
    }
    return(ValidDirs)
}

LoadRawDataDirs = function(CacheFile = "../data/DataDirs.csv", CheckValidity=TRUE, ...)
{
    
    if (!file.exists(CacheFile))
    {
        if (CheckValidity) {
            DataDirs = ProbaVValidDirs(...)
        } else {
            DataDirs = list.dirs(ProbaVDataDirs(...), recursive=FALSE)
        }
        DataDirsDates = data.frame(dir=DataDirs, date=as.Date(basename(dirname(DataDirs)), format="%Y%m%d"))
        write.csv(DataDirsDates, CacheFile, row.names=FALSE)
    } else {
        print(paste("Reusing existing list of data directories from", CacheFile))
        DataDirsDates = read.csv(CacheFile, stringsAsFactors=FALSE)
        DataDirsDates$date = as.Date(DataDirsDates$date)
    }
    return(DataDirsDates)
}
