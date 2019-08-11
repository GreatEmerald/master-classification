# Next-gen setup for data retrieval:
# 1) Run get-point-coords.r to generate a space-delimited CSV with all coordinates of points
# 2) Run make-timeseries-vrts.r to generate VRTs per tile per band
# 3) Run extract-ng.sh to get an XML of time series values per band per tile.
#    This runs on *all* points regardless of source for the sake of efficiency (reading tiles only once)
# 4) Run this script to ingest the information into one large GeoPackage.
# 5) Proceed with the analysis. The dataset needs to be split into training, validation and prediction.

library(sf)
library(readr) # To put a root node over XMLs
library(xml2) # New-style XMLs
library(foreach)
library(doSNOW)

source("pixel-based/utils/ProbaVDataDirs.r")
source("pixel-based/utils/ProbaVTileID.r")

# We have the old-style CSVs from the first round to ingest, and also the new-style XMLs.
# The database can hold also covariate data etc., it's all just a matter of adding more columns.

# We want to make sure we only write in missing values.
# So read in the database first and then add new lines if they don't yet exist.
# If they do, check if we can fill in any of NAs.

SourceCoordDir = "../work/extract-from/"
ImportXMLDir = "../work/extracted-points/"
OutDir = "../data/pixel-based/timeseries/"
DBFile = file.path(OutDir, "timeseries.gpkg")

DataDirs = LoadRawDataDirs("../data/pixel-based/DataDirs-NG.csv", FALSE)
SourceCoordFiles = list.files("../work/extract-from/", full.names=TRUE)
ImportXMLs = list.files(ImportXMLDir, full.names = TRUE)
NameParse = strsplit(basename(ImportXMLs), "-")
TileNames = sapply(NameParse, `[[`, 1)
VINames = sapply(NameParse, function(x){sub(".xml", "", paste(x[-1], collapse = "-"))})

if (!dir.exists(OutDir))
    dir.create(OutDir)

options(stringsAsFactors=FALSE, warn = 1)

# Return a vector of NAs for filling in with values
# Note that we also have columns X, Y, and geom
GetSchema = function()
{
    CurrentLine = rep(NA_integer_, nrow(DataDirs))
    names(CurrentLine) = paste0("X", basename(dirname(DataDirs$dir)))
    return(CurrentLine[!duplicated(names(CurrentLine))]) # dedup
}

FindDuplicates = function(df) {
    Distances = abs(CurrentDataset$X-df["X"])+abs(CurrentDataset$Y-df["Y"])
    Duplicates = sum(Distances==0) # Adjust fuzz threshold if necessary
    return(Duplicates)
}

GetMinDistance = function(df) {
    Distances = abs(CurrentDataset$X-df["X"])+abs(CurrentDataset$Y-df["Y"])
    return(min(Distances))
}

GetMemUsage = function(...)
{
    print(sort(sapply(c(ls(.GlobalEnv), ...),function(x){object.size(get(x))}), TRUE)[1:5]/1024/1024) # In MiB
}

#cl = makeCluster(2, outfile="")
#registerDoSNOW(cl)

# Read in XML files
# Go VI-by-VI
for (VI in unique(VINames))
{
    # Will error in case file or layer doesn't exist, set it to NULL then
    CurrentDataset = tryCatch(st_read(DBFile, VI), error=function(e)NULL)
    
    # Progress bar
    pbi = 1
    #pb = txtProgressBar(min = pbi, max = sum(VINames == VI), style = 3)
    iterations = sum(VINames == VI)
    pb = txtProgressBar(max = iterations, style = 3)
    progress = function(n) setTxtProgressBar(pb, n)
    
    # Parse each file
    #VIDF=NULL
    #for (fileidx in c(229, 235))#rev(which(VINames == VI)))
    VIDF = foreach (fileidx=which(VINames == VI), .combine=rbind, .multicombine=TRUE,
                    .options.snow = list(progress = progress), .packages=c("readr", "xml2", "foreach")) %do%
    {
        
        #print(paste("a", fileidx))
        Tile = TileNames[fileidx]
        VIName = VINames[fileidx]
        XMLFile = ImportXMLs[fileidx]
        
        # Fix the XMLs, because it's only valid with a root node
        XMLString = read_file(XMLFile)
        if (XMLString == "")
        {
            print(paste("Input file", XMLFile, "is empty. Presuming that it is the one that is currently being extracted, skipping."))
            return(NULL)
        }
        ValidXML = paste0('<Tile id="', Tile, '">', XMLString, "</Tile>")
        rm(XMLString)
        XMLObj = read_xml(ValidXML)
        
        XYCSVFile = SourceCoordFiles[which(basename(SourceCoordFiles) == paste0(Tile, ".csv"))]
        XYTable = read.table(XYCSVFile, sep=" ")
        PointObj = xml_find_all(XMLObj, "//Report")
        if (length(PointObj) != nrow(XYTable))
            stop(paste("Points extracted:", length(PointObj), "Points in the CSV file:", nrow(XYTable), "CSV file:", XYCSVFile, "XML file:", XMLFile))
        
        #print(paste("b", fileidx))
        #TSDF=NULL
        #for (pointidx in 1:nrow(XYTable))
        TSDF = foreach(pointidx=1:nrow(XYTable), .combine=rbind, .multicombine=TRUE, .packages="xml2") %do%
        {
            # Get XY coords
            X = XYTable[pointidx, 1]
            Y = XYTable[pointidx, 2]
            # If we already have such an entry, skip it
            #print("e")
            #GetMemUsage(ls())
            
            if (!is.null(CurrentDataset))
            {
                Duplicates = FindDuplicates(data.frame(X, Y))
                if (is.null(Duplicates))
                    stop("Duplicates doesn't exist!")
                if (Duplicates > 0)
                {
                    if (Duplicates > 1)
                        warning(paste("Non-unique point matched! X:", X, "Y:", Y))
                    return(NULL)
                }
            }
            #print("f")
            
            # Schema
            CurrentLine = GetSchema()
            # Get dates
            SourceFilenames = xml_text(xml_find_all(PointObj[[pointidx]], ".//File"))
            if (length(SourceFilenames) == 0) # Files not found
            {
                # Was an alert issued?
                Alert = xml_text(xml_find_all(PointObj[[pointidx]], ".//Alert"))
                if (length(Alert) > 0) {
                    warning(paste("Skipping point due to an alert in XML", XMLFile, "point", X, Y, ":", Alert))
                    return(NULL)
                } else stop(paste("No files found in XML", XMLFile, "and no alerts issued either, stopping."))
            }
            TSFilenames = strsplit(basename(SourceFilenames), "_")
            stopifnot(sapply(TSFilenames, `[[`, 4) == Tile)
            TSDates = sapply(TSFilenames, `[[`, 5)
            TSValues = xml_integer(xml_find_all(PointObj[[pointidx]], ".//Value"))
            if (length(TSDates) <= 0 || length(TSValues) <= 0)
                stop(paste("Could not find any values in file", XMLFile ,"in point", XYTable[pointidx,1], XYTable[pointidx,2]))
            #print(paste("TSDates", length(TSDates)))
            #print(paste("TSValues", length(TSValues)))
            stopifnot(length(TSDates) == length(TSValues)) # We hope that they match, otherwise looping a search takes too long
            #print("g")
            #print(TSValues)
            for (dataidx in 1:length(TSValues))
            {
                #print(paste("h", TSValues, TSValues[dataidx]))
                if (!is.na(TSValues[dataidx])) # Never replace with an NA
                {
                    #if (TSDates[dataidx] == "20161201")
                    #    print(paste0("Setting X", TSDates[dataidx],"=", CurrentLine[names(CurrentLine) == paste0("X", TSDates[dataidx])], " to ", TSValues[dataidx]))
                    CurrentLine[names(CurrentLine) == paste0("X", TSDates[dataidx])] = TSValues[dataidx]
                }
            }
            #print("h")
            
            Result = cbind.data.frame(X=X, Y=Y, t(CurrentLine))
            #print(str(Result))
            
            gc()
            
            Result
            #TSDF = rbind(TSDF, Result)
        }
        #print(paste("c", fileidx))
        pbi = pbi+1
        setTxtProgressBar(pb, pbi)
        
        if (is.null(TSDF))
            return(NULL)
        #print(paste("d", fileidx))
        Result = cbind.data.frame(Tile=Tile, TSDF)
        rm(TSDF)
        #str(Result)
        Result
        #VIDF = rbind(VIDF, Result)
    }
    close(pb)
    
    if (!is.null(VIDF))
    {
        print(paste("VI", VI, "parsed, converting into format to put into", DBFile))
        # Remove duplicates - otherwise we may be biasing the classifier
        VIDF = VIDF[!duplicated(VIDF),]
        
        # Make spatial
        VISpatial = st_as_sf(VIDF, coords=c("X", "Y"), dim="XY", remove=FALSE)
        rm(VIDF)
        names(VISpatial)[names(VISpatial) == "geometry"] = "geom"
        st_geometry(VISpatial) = "geom" # rename to match GPKG name
        st_crs(VISpatial) = 4326
        
        # Append
        VISpatial = rbind(CurrentDataset, VISpatial)
        st_write(VISpatial, DBFile, VI, layer_options="OVERWRITE=YES")
        print(paste("VI", VI, "successfully updated in file", DBFile))
    } else print(paste("No changes detected for", VI, "so the input file is not touched."))
}

rm(PointObj)
rm(ValidXML)
rm(VISpatial)
rm(CurrentDataset)
gc()

# Also read in old-style CSVs
OldDir = "../work/old-style/"

ImportCSVs = list.files(OldDir, pattern=glob2rx("*.csv"), full.names = TRUE)
NameParse = strsplit(basename(ImportCSVs), "-")
VINames = sapply(NameParse, function(x){ifelse(length(x)>2, paste0(x[1], "-", x[2]), x[1])})

for (VI in unique(VINames))
{
    if (file.exists(DBFile)) {
        CurrentDataset = st_read(DBFile, VI)
    } else CurrentDataset = NULL
    # Parse each file
    CSVList = lapply(ImportCSVs[VINames==VI], read.csv)
    CSVList = Reduce(rbind, CSVList) # Make one huge DF (this requires 1 GiB RAM)
    # Make sure we have all columns
    CurrentLine = GetSchema()
    for (ColName in names(CurrentLine))
    {
        if (all(colnames(CSVList) != ColName)) # Don't have the column yet
            CSVList[,ColName] = NA # Make NA columns for missing dates
    }

    # Add tile names
    CSVList[,"Tile"] = ProbaVTileID(CSVList[,c("x","y")])
    
    names(CSVList)[names(CSVList) == "x"] = "X"
    names(CSVList)[names(CSVList) == "y"] = "Y"
    
    # Reduce the dataframe into unique rows only
    #MinDist = apply(CSVList[,c("X","Y")], 1, GetMinDistance)
    Duplicates = apply(CSVList[,c("X","Y")], 1, FindDuplicates)
    print(paste("Matched rows by coordinates, duplicates:", sum(Duplicates > 0), "of which multiple:", sum(Duplicates > 1)))
    CSVList = CSVList[Duplicates == 0,]

    if (!is.null(nrow(CSVList)) && nrow(CSVList) > 0)
    {
        # Remove internal duplicates - otherwise we may be biasing the classifier
        CSVList = CSVList[!duplicated(CSVList),]
        
        # Make spatial
        VISpatial = st_as_sf(CSVList, coords=c("X", "Y"), dim="XY", remove=FALSE)
        rm(CSVList)
        names(VISpatial)[names(VISpatial) == "geometry"] = "geom"
        st_geometry(VISpatial) = "geom" # rename to match GPKG name
        st_crs(VISpatial) = 4326
    
        # Append
        VISpatial = rbind(CurrentDataset, VISpatial)
        st_write(VISpatial, DBFile, VI, layer_options="OVERWRITE=YES")
        rm(VISpatial)
    }
}

#stopCluster(cl)
