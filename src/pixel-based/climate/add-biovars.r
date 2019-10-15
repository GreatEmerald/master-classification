# Add additional bioclimatic parameters

library(sf)
library(pryr)

OutFile = "../data/pixel-based/climate/climate-extended.gpkg"

# Based on dismo::biovars but on non-temperature/precipitation

AllClimateData = st_read("../data/pixel-based/climate/climate.gpkg")

print("Calculating solar radiation biovars")
srads = sprintf("wc2.0_30s_srad_%02d", 01:12)
AllClimateData$min.srad = apply(as.data.frame(AllClimateData)[,srads], 1, min, na.rm=TRUE)
AllClimateData[["min.srad"]][AllClimateData$min.srad == Inf] = NA
AllClimateData$max.srad = apply(as.data.frame(AllClimateData)[,srads], 1, max, na.rm=TRUE)
AllClimateData[["max.srad"]][AllClimateData$max.srad == Inf] = NA
AllClimateData$mean.srad = apply(as.data.frame(AllClimateData)[,srads], 1, mean, na.rm=TRUE)
AllClimateData[["mean.srad"]][is.nan(AllClimateData$mean.srad)] = NA

print("Calculating wind speed biovars")
winds = sprintf("wc2.0_30s_wind_%02d", 01:12)
AllClimateData$min.wind = apply(as.data.frame(AllClimateData)[,winds], 1, min, na.rm=TRUE)
AllClimateData[["min.wind"]][AllClimateData$min.wind == Inf] = NA
AllClimateData$max.wind = apply(as.data.frame(AllClimateData)[,winds], 1, max, na.rm=TRUE)
AllClimateData[["max.wind"]][AllClimateData$max.wind == Inf] = NA
AllClimateData$mean.wind = apply(as.data.frame(AllClimateData)[,winds], 1, mean, na.rm=TRUE)
AllClimateData[["mean.wind"]][is.nan(AllClimateData$mean.wind)] = NA

print("Calculating water vapour biovars")
vaprs = sprintf("wc2.0_30s_vapr_%02d", 01:12)
AllClimateData$min.vapr = apply(as.data.frame(AllClimateData)[,vaprs], 1, min, na.rm=TRUE)
AllClimateData[["min.vapr"]][AllClimateData$min.vapr == Inf] = NA
AllClimateData$max.vapr = apply(as.data.frame(AllClimateData)[,vaprs], 1, max, na.rm=TRUE)
AllClimateData[["max.vapr"]][AllClimateData$max.vapr == Inf] = NA
AllClimateData$mean.vapr = apply(as.data.frame(AllClimateData)[,vaprs], 1, mean, na.rm=TRUE)
AllClimateData[["mean.vapr"]][is.nan(AllClimateData$mean.vapr)] = NA

# Apply function for getting the month number from colnames.
# apply_fun is the actual function to run
GetMonthNumberApply = function(x, apply_fun)
{
    DesiredName = names(apply_fun(x))
    if (length(DesiredName) <= 0)
        return(NA)
    return(substr(DesiredName, 16, 17))
}

# Additional biovars based on `which`: all variables at the hottest/coldest month, wettest/driest month
tmins = sprintf("wc2.0_30s_tmin_%02d", 01:12)
tmaxs = sprintf("wc2.0_30s_tmax_%02d", 01:12)
precs = sprintf("wc2.0_30s_prec_%02d", 01:12)
AllClimateData$cold = apply(as.data.frame(AllClimateData)[,tmins], 1, GetMonthNumberApply, which.min)
AllClimateData$warm = apply(as.data.frame(AllClimateData)[,tmaxs], 1, GetMonthNumberApply, which.max)
AllClimateData$dry = apply(as.data.frame(AllClimateData)[,precs], 1, GetMonthNumberApply, which.min)
AllClimateData$wet = apply(as.data.frame(AllClimateData)[,precs], 1, GetMonthNumberApply, which.max)

# Apply function that obtains the data from the right column by number
# variable: variable name (prec/vapr/etc.)
# according_to: ColdMonthNum/WarmMonthNum/DryMonthNum/WetMonthNum
GetDataFromIndex = function(x, variable, according_to)
{
    according_to = x[[according_to]]
    if (is.na(according_to))
        return(NA)
    x[[grep(paste0(variable, "_", according_to), names(x))]]
}

# Make all combinations (some probably already included in bioXX)
Combinations = expand.grid(c("srad", "wind", "vapr", "prec", "tmin", "tmax", "tavg"),
                           c("cold", "warm", "wet", "dry"), stringsAsFactors = FALSE)

for (c in 1:nrow(Combinations))
{
    VarName = paste(Combinations[c,2], Combinations[c,1], sep = ".")
    print(VarName)
    
    ChunkSize = 5000
    DataLength = nrow(AllClimateData)
    Iterator = seq(1, DataLength, ChunkSize)
    pb = txtProgressBar(min = 1, max = DataLength, style = 3)
    Variable = numeric(DataLength)
    for (i in Iterator)
    {
        ChunkEnd = min(i+ChunkSize-1, DataLength)
        DataChunk = as.data.frame(AllClimateData)[i:ChunkEnd,]
        Variable[i:ChunkEnd] = apply(DataChunk, 1, GetDataFromIndex, Combinations[c,1], Combinations[c,2])
        
        setTxtProgressBar(pb, ChunkEnd)
    }
    AllClimateData[[VarName]] = Variable
    rm(Variable)
    gc()
}

st_write(AllClimateData, OutFile, delete_dsn=TRUE)

# Note: values where all data is NA look like zero everywhere in the row, with derivatives being -inf in some cases and in some cases null
