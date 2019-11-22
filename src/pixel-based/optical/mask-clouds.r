# Given a database with an SM table, apply it to all other tables.
# Run after stack-timeseries-grep and before get-harmonics

library(sf)

InputDB = "../data/pixel-based/timeseries/timeseries-grep.gpkg"
OutputDB = "../data/pixel-based/timeseries/timeseries-masked.gpkg"
FilterQC=c(240, 248, 244, 252) # Which values to keep
VIs = c("NDVI", paste0("RADIOMETRY-", 1:4))

SM = st_read(InputDB, "SM")
SM$coord = apply(as.data.frame(SM)[,c("X","Y")], 1, paste, collapse="Y")

# Dedup
SM = SM[!duplicated(SM[c("X", "Y")]),]

# Extract the numeric part
SMMatrix = as.matrix(as.data.frame(SM)[,names(SM)[!names(SM) %in% c("X", "Y", "coord", "geom")]])

# Takes a row, outputs altered row
# Alternatively could try to match two matrices and replace as if it was a vector
MaskPixels = function(VIRow)
{
    # Find matching SM
    SMIndex = VIRow$coord %in% SM$coord
    stopifnot(is.finite(SMIndex))
    SMRow = SM[SMIndex,]
    for (col in names(SMRow))
    {
        if (col == "X" || col == "Y" || col == "geom")
            next
        if (SMRow[[col]] %in% FilterQC)
            VIRow[[col]] = NA
    }
    return(VIRow)
}

for (VI in VIs)
{
    VITable = st_read(InputDB, VI)
    # Make a "coord" unique col
    VITable$coord = apply(as.data.frame(VITable)[,c("X","Y")], 1, paste, collapse="Y")
    VITable = VITable[!duplicated(VITable[c("X", "Y")]),] # dedup
    VIOrder = match(SM$coord, VITable$coord) # reorder
    stopifnot(!any(is.na(VIOrder))) # All much match
    VITable = VITable[VIOrder,] # Make the order match to align matrices
    # Extract just the matrices
    VIMatrix = as.matrix(as.data.frame(VITable)[,names(VITable)[!names(VITable) %in% c("X", "Y", "coord", "geom")]])
    stopifnot(all.equal(dim(VIMatrix), dim(SMMatrix)))
    gc()
    # Mask with SM; we interpret both matrices as a flat vector
    VIMatrix[!SMMatrix %in% FilterQC] = NA
    gc()
    # Put it all back together
    # VITable[,colnames(VIMatrix)] = VIMatrix # Dies out of memory
    VITable = cbind(VITable[,c("X", "Y", "geom")], VIMatrix)
    st_write(VITable, OutputDB, VI)
    rm(VIMatrix, VITable)
    gc()
}
