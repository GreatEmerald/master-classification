# grep-based stack-timeseries-ng
# Parses gdallocationinfo points into a database

source("pixel-based/utils/ProbaVDataDirs.r")
source("pixel-based/utils/db-io.r")

SourceCoordDir = "../work/extract-from/"
ImportXMLDir = "../../userdata/master-classification/work/extracted-points/ndvi"
ProcessedXMLDir = file.path(ImportXMLDir, "..", "processed-grep")
OutDir = "../data/pixel-based/timeseries/"
DBFile = file.path(OutDir, "timeseries-grep.gpkg")

DataDirs = LoadRawDataDirs("../data/pixel-based/DataDirs-NG.csv", FALSE)
ImportXMLs = list.files(ImportXMLDir, glob2rx("*.xml"), full.names = TRUE)
NameParse = strsplit(basename(ImportXMLs), "-")
TileNames = sapply(NameParse, `[[`, 1)
VINames = sapply(NameParse, function(x){sub(".xml", "", paste(x[-1], collapse = "-"))})
# Input coords
XYCSVFiles = file.path(SourceCoordDir, paste0(TileNames, ".csv"))

if (!dir.exists(OutDir))
    dir.create(OutDir)
if (!dir.exists(ProcessedXMLDir))
    dir.create(ProcessedXMLDir)

options(stringsAsFactors=FALSE, warn = 1)

GetSchema = function(cols=1)
{
    CurrentLine = matrix(NA_integer_, cols, nrow(DataDirs))
    colnames(CurrentLine) = paste0("X", basename(dirname(DataDirs$dir)))
    CurrentLine = CurrentLine[,!duplicated(colnames(CurrentLine))] # dedup
    if (cols == 1) # This collapsed the above into a vector, reset it to a matrix
        CurrentLine = t(CurrentLine)
    return(CurrentLine)
}

stopifnot(length(ImportXMLs) == length(XYCSVFiles) && length(XYCSVFiles) == length(VINames))

# Go through every file
pb = txtProgressBar(min = 1, max = length(ImportXMLs), style = 3)
for (fileidx in 1:length(ImportXMLs))
{
    ImportXML = ImportXMLs[fileidx]
    print(ImportXML)
    XYCSVFile = XYCSVFiles[fileidx]
    VIName = VINames[fileidx]
    
    # Source coords
    XYTable = read.table(XYCSVFile, sep=" ")
    # Matrix we want to fill in
    ValueMatrix = GetSchema(nrow(XYTable))
    # Regex: match (*/????/)????????(/PROBAV_*</File>) or Alert
    ImportDates = system(paste0("grep -oP '(?<=\\/[0-9]{4}\\/)[0-9]{8}(?=\\/PROBAV_.*<\\/File>)|<Alert>' ", ImportXML), TRUE)
    # Regex: match (<Value>)*(</Value>)
    ImportValues = as.integer(system(paste0("grep -oP '(?<=<Value>).*(?=<\\/Value>)' ", ImportXML), TRUE))
    
    # We need a for loop here because it's pretty complex
    FillRow = 1
    j=1 # matching index in ImportValues
    for (i in 1:length(ImportDates))
    {
        # Is it an alert? Then skip this row
        if (ImportDates[i] == "<Alert>") {
            if (i > 1) # We shouldn't increase it if it's already the first
                FillRow = FillRow + 1
            next
        }
        
        # Are we no longer monotonically increasing? Then go to next row
        if (i > 1 && (ImportDates[i-1] == "<Alert>" ||
                      as.integer(ImportDates[i]) < as.integer(ImportDates[i-1])))
            FillRow = FillRow + 1
        
        ValueMatrix[FillRow, paste0("X", ImportDates[i])] = ImportValues[j]
        
        j = j+1
    }
    
    stopifnot(j-1 == length(ImportValues)) # Make sure we accounted for all the values (-1 because we increment at the end)
    
    # Merge with coords
    ValueMatrix = data.frame(X=XYTable[[1]], Y=XYTable[[2]], ValueMatrix)
    
    # Write to disk
    ValueMatrix = DFtoSF(ValueMatrix)
    
    st_write(ValueMatrix, DBFile, VIName)
    
    rm(XYTable, ValueMatrix, ImportDates, ImportValues)
    gc()
    
    # Move the processed XML out of the way
    file.rename(ImportXML, file.path(ProcessedXMLDir, basename(ImportXML)))
    
    setTxtProgressBar(pb, fileidx)
}
close(pb)
