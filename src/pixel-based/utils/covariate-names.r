# Utils for getting covariate names, based on their correlations

# List of names of pixel-based covariates that are known to not be correlated. Useful for random forest VarImp.
GetUncorrelatedPixelCovars = function()
{
    return(c("x", "y", "yabs", "mean.ndvi", "ndvi.iqr", "red", "nir", "ndmi", "evi", "co", "si", "co2", "si2", "trend", "phase1", "amplitude1", "phase2", "amplitude2", "elevation", "slope", "aspect", "tpi", "wc2.0_30s_tmax_01", "wc2.0_30s_prec_01", "wc2.0_30s_srad_01", "wc2.0_30s_wind_01", "wc2.0_30s_vapr_01", "wc2.0_30s_prec_03", "wc2.0_30s_srad_03", "wc2.0_30s_prec_04", "wc2.0_30s_srad_04", "wc2.0_30s_wind_04", "wc2.0_30s_prec_05", "wc2.0_30s_srad_05", "wc2.0_30s_tavg_08", "wc2.0_30s_prec_08", "wc2.0_30s_srad_08", "wc2.0_30s_vapr_08", "wc2.0_30s_srad_09", "wc2.0_30s_prec_10", "wc2.0_30s_srad_10", "wc2.0_30s_wind_10", "wc2.0_30s_prec_11", "bio1", "bio2", "bio8", "bio9", "bio10", "bio11", "bio12", "bio15", "bio17", "bio18", "bio19", "min.srad", "max.srad", "mean.srad", "min.wind", "max.wind", "min.vapr", "max.vapr", "sol_bulkdens.10cm", "sol_clay.10cm", "sol_organic.10cm", "sol_organic.200cm", "OCSTHA.M.sd1", "OCSTHA.M.sd2", "AWCh1.M.sl1", "CRFVOL.M.sl2", "SLTPPT.M.sl2", "AWCh2.M.sl2", "AWCtS.M.sl2", "CECSOL.M.sl2", "WWP.M.sl2", "AWCh1.M.sl3", "AWCh1.M.sl4", "ORCDRC.M.sl5", "AWCh1.M.sl7", "AWCh2.M.sl7", "BDRICM.M.BDRICM_M", "BDRLOG.M.BDRLOG_M", "BDTICM.M.BDTICM_M", "TMDMOD_2011.M.Jan", "TMDMOD_2011.M.Mar", "TMDMOD_2011.M.May", "TMDMOD_2011.M.Nov"))
}

GetAllPixelCovars = function(grouped=FALSE)
{
    AllCovars = list(location=c("x", "y", "yabs"),
        spectral=c("min", "max", "NDMI.year.median", "NDMI.year.IQR", "NDMI.spring.IQR", "NDMI.summer.IQR", "NDMI.autumn.IQR", "NDMI.winter.IQR", "OSAVI.spring.IQR", "OSAVI.summer.IQR", "OSAVI.autumn.IQR", "OSAVI.winter.IQR", "EVI.spring.IQR", "EVI.summer.IQR", "EVI.autumn.IQR", "EVI.winter.IQR", "NIRv.year.median", "NIRv.year.IQR", "NIRv.spring.IQR", "NIRv.summer.IQR", "NIRv.autumn.IQR", "NIRv.winter.IQR"),
        harmonic=c("co", "si", "co2", "si2", "trend", "phase1", "amplitude1", "phase2", "amplitude2"),
        terrain=c("elevation", "slope.log", "aspect", "tpi"),
        climate=GetClimateCovars(),
        #soillandgis=GetLandGISCovars(),
        soilgrids=GetSoilGridsCovars())#,
        #soilgridstaxo=GetSoilGridsClasses()) #paste0("bio", 1:19)
    if (!grouped)
        return(unlist(AllCovars))
    return(AllCovars)
}

# Deprecated
GetCovarNames = function(type)
{
    if (type == "climate")
        return(GetClimateCovars())
}

GetClimateCovars = function()
{
    # Months = sprintf("%02d", 01:12)
    # WCDS = c("tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr")
    # MDS = expand.grid(WCDS, Months)
    # nonbio = c("srad", "wind", "vapr")
    # nbstats = c("min", "max", "mean")
    # NBS = expand.grid(nbstats, nonbio)
    # return(c(paste("wc2.0_30s", MDS[,1], MDS[,2], sep="_"), paste0("bio", 1:19), paste0(NBS[,1], ".", NBS[,2])))
    
    return(c("jan.prec.log", "apr.prec.log", "jul.prec.log", "oct.prec.log", "jan.srad", "jul.srad", "mean.tavg", "tavg.monthly.range", "isothermality", "tavg.annual.range", "annual.prec.log", "prec.seasonality", "min.srad", "max.srad", "mean.srad", "mean.wind", "mean.vapr", "cold.prec.log", "warm.prec.log", "wet.srad", "dry.srad"))
}

GetLandGISCovars = function()
{
    Prefix = "sol_"
    Depths = paste0(c(0, 10, 30, 60, 100, 200), "cm")
    Vars = c("bulkdens", "clay", "organic", "ph", "sand")
    VarDepth = expand.grid(Depths, Vars)
    Covars = paste0(Prefix, VarDepth[,2], ".", VarDepth[,1])
    return(Covars)
}

GetSoilGridsCovars = function()
{
    #XDVars = c("ALUM3S", "EALKCL", "ECAX", "EMGX", "ENAX", "EXKX", "NTO")
    #XDDepth = expand.grid(XDVars, 1:2)
    #XDVarNames = paste0(XDDepth[,1], ".M.xd", XDDepth[,2]) # Too many missing values in XDs
    
    # SLVars = c("CLYPPT", "CRFVOL", "PHIHOX", "PHIKCL", "SLTPPT", "SNDPPT",
    #     "AWCh1", "AWCh2", "AWCh3", "AWCtS", "BLDFIE", "CECSOL", "ORCDRC", "TEXMHT", "WWP")
    # SDVars = c("OCSTHA")
    # SDDepth = expand.grid(SDVars, 1:6)
    # SDVarNames = paste0(SDDepth[,1], ".M.sd", SDDepth[,2])
    # SLDepth = expand.grid(SLVars, 1:7)
    # SLVarNames = paste0(SLDepth[,1], ".M.sl", SLDepth[,2])
    # MonthVars = c("PREMRG")#, "TMDMOD_2001", "TMDMOD_2011", "TMNMOD_2001", "TMNMOD_2011")
    # Months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    # MonthVN = expand.grid(MonthVars, Months)
    # MonthVarNames = paste0(MonthVN[,1], ".M.", MonthVN[,2])
    # SingleVars = c("BDRICM.M.BDRICM_M", "BDRLOG.M.BDRLOG_M", "BDTICM.M.BDTICM_M",
    #     #"DRAINFAO.M.DRAINFAO_M", "GLC100m.M.GLC2010") # Disable GLC and also too little data for DRAINFAO
    #     "TMDMOD_2001.M.Feb", "TMDMOD_2001.M.Mar", "TMDMOD_2001.M.Apr", "TMDMOD_2001.M.May", "TMDMOD_2001.M.Nov",
    #     "TMDMOD_2011.M.Jan", "TMDMOD_2011.M.Feb", "TMDMOD_2011.M.Mar", "TMDMOD_2011.M.May", "TMDMOD_2011.M.Nov",
    #     "TMNMOD_2011.M.May", "TMNMOD_2011.M.Jul") # Not enough obs for some months, add manually
    #return(c(SDVarNames, SLVarNames, MonthVarNames, SingleVars))
    
    return(c("soil.av.water", "soil.bulkdens", "soil.log.cation", "soil.clay.pct", "soil.log.coarfrag", "soil.ph", "soil.sand.pct", "soil.wilt.wat"))
}

# Not used, these are derived from covariates above
GetSoilGridsClasses = function()
{
    USDA = c("TAXOUSDA.Gelepts", "TAXOUSDA.Albolls", "TAXOUSDA.Aqualfs", "TAXOUSDA.Aquands", "TAXOUSDA.Aquents", "TAXOUSDA.Aquepts", "TAXOUSDA.Aquerts", "TAXOUSDA.Aquods", "TAXOUSDA.Aquolls", "TAXOUSDA.Aquox", "TAXOUSDA.Aquults", "TAXOUSDA.Arents", "TAXOUSDA.Argids", "TAXOUSDA.Borolls", "TAXOUSDA.Calcids", "TAXOUSDA.Cambids", "TAXOUSDA.Cryalfs", "TAXOUSDA.Cryands", "TAXOUSDA.Cryepts", "TAXOUSDA.Cryids", "TAXOUSDA.Cryods", "TAXOUSDA.Cryolls", "TAXOUSDA.Durids", "TAXOUSDA.Fibrists", "TAXOUSDA.Fluvents", "TAXOUSDA.Folists", "TAXOUSDA.Gelands", "TAXOUSDA.Gelods", "TAXOUSDA.Gypsids", "TAXOUSDA.Hemists", "TAXOUSDA.Histels", "TAXOUSDA.Humods", "TAXOUSDA.Humults", "TAXOUSDA.Ochrepts", "TAXOUSDA.Orthels", "TAXOUSDA.Orthents", "TAXOUSDA.Orthods", "TAXOUSDA.Perox", "TAXOUSDA.Psamments", "TAXOUSDA.Rendolls", "TAXOUSDA.Salids", "TAXOUSDA.Saprists", "TAXOUSDA.Torrands", "TAXOUSDA.Torrerts", "TAXOUSDA.Torrox", "TAXOUSDA.Turbels", "TAXOUSDA.Udalfs", "TAXOUSDA.Udands", "TAXOUSDA.Udepts", "TAXOUSDA.Uderts", "TAXOUSDA.Udolls", "TAXOUSDA.Udox", "TAXOUSDA.Udults", "TAXOUSDA.Ustalfs", "TAXOUSDA.Ustands", "TAXOUSDA.Ustepts", "TAXOUSDA.Usterts", "TAXOUSDA.Ustolls", "TAXOUSDA.Ustox", "TAXOUSDA.Ustults", "TAXOUSDA.Vitrands", "TAXOUSDA.Xeralfs", "TAXOUSDA.Xerands", "TAXOUSDA.Xerepts", "TAXOUSDA.Xererts", "TAXOUSDA.Xerolls", "TAXOUSDA.Xerults")
    WRB = c("TAXNWRB.Acric.Ferralsols", "TAXNWRB.Acric.Plinthosols", "TAXNWRB.Albic.Arenosols", "TAXNWRB.Albic.Luvisols", "TAXNWRB.Alic.Nitisols", "TAXNWRB.Aluandic.Andosols", "TAXNWRB.Aric.Regosols", "TAXNWRB.Calcaric.Regosols", "TAXNWRB.Calcic.Chernozems", "TAXNWRB.Calcic.Gleysols", "TAXNWRB.Calcic.Gypsisols", "TAXNWRB.Calcic.Histosols", "TAXNWRB.Calcic.Kastanozems", "TAXNWRB.Calcic.Luvisols", "TAXNWRB.Calcic.Solonetz", "TAXNWRB.Calcic.Vertisols", "TAXNWRB.Cryic.Histosols", "TAXNWRB.Cutanic.Alisols", "TAXNWRB.Endogleyic.Cambisols", "TAXNWRB.Endogleyic.Planosols", "TAXNWRB.Ferralic.Arenosols", "TAXNWRB.Ferralic.Cambisols", "TAXNWRB.Fibric.Histosols", "TAXNWRB.Gleyic.Luvisols", "TAXNWRB.Gleyic.Podzols", "TAXNWRB.Gleyic.Solonetz", "TAXNWRB.Gypsic.Solonchaks", "TAXNWRB.Haplic.Acrisols", "TAXNWRB.Haplic.Acrisols..Alumic.", "TAXNWRB.Haplic.Acrisols..Ferric.", "TAXNWRB.Haplic.Acrisols..Humic.", "TAXNWRB.Haplic.Albeluvisols", "TAXNWRB.Haplic.Alisols", "TAXNWRB.Haplic.Andosols", "TAXNWRB.Haplic.Arenosols", "TAXNWRB.Haplic.Arenosols..Calcaric.", "TAXNWRB.Haplic.Calcisols", "TAXNWRB.Haplic.Calcisols..Sodic.", "TAXNWRB.Haplic.Cambisols", "TAXNWRB.Haplic.Cambisols..Calcaric.", "TAXNWRB.Haplic.Cambisols..Chromic.", "TAXNWRB.Haplic.Cambisols..Dystric.", "TAXNWRB.Haplic.Cambisols..Eutric.", "TAXNWRB.Haplic.Cambisols..Humic.", "TAXNWRB.Haplic.Cambisols..Sodic.", "TAXNWRB.Haplic.Chernozems", "TAXNWRB.Haplic.Cryosols", "TAXNWRB.Haplic.Ferralsols", "TAXNWRB.Haplic.Ferralsols..Rhodic.", "TAXNWRB.Haplic.Ferralsols..Xanthic.", "TAXNWRB.Haplic.Fluvisols", "TAXNWRB.Haplic.Fluvisols..Arenic.", "TAXNWRB.Haplic.Fluvisols..Calcaric.", "TAXNWRB.Haplic.Fluvisols..Dystric.", "TAXNWRB.Haplic.Fluvisols..Eutric.", "TAXNWRB.Haplic.Gleysols", "TAXNWRB.Haplic.Gleysols..Dystric.", "TAXNWRB.Haplic.Gleysols..Eutric.", "TAXNWRB.Haplic.Gypsisols", "TAXNWRB.Haplic.Kastanozems", "TAXNWRB.Haplic.Leptosols", "TAXNWRB.Haplic.Leptosols..Eutric.", "TAXNWRB.Haplic.Lixisols", "TAXNWRB.Haplic.Lixisols..Chromic.", "TAXNWRB.Haplic.Lixisols..Ferric.", "TAXNWRB.Haplic.Luvisols", "TAXNWRB.Haplic.Luvisols..Chromic.", "TAXNWRB.Haplic.Luvisols..Ferric.", "TAXNWRB.Haplic.Nitisols..Rhodic.", "TAXNWRB.Haplic.Phaeozems", "TAXNWRB.Haplic.Planosols..Dystric.", "TAXNWRB.Haplic.Planosols..Eutric.", "TAXNWRB.Haplic.Podzols", "TAXNWRB.Haplic.Regosols..Dystric.", "TAXNWRB.Haplic.Regosols..Eutric.", "TAXNWRB.Haplic.Regosols..Sodic.", "TAXNWRB.Haplic.Solonchaks", "TAXNWRB.Haplic.Solonchaks..Sodic.", "TAXNWRB.Haplic.Solonetz", "TAXNWRB.Haplic.Umbrisols", "TAXNWRB.Haplic.Vertisols", "TAXNWRB.Haplic.Vertisols..Eutric.", "TAXNWRB.Hemic.Histosols", "TAXNWRB.Hypoluvic.Arenosols", "TAXNWRB.Leptic.Cambisols", "TAXNWRB.Leptic.Luvisols", "TAXNWRB.Leptic.Phaeozems", "TAXNWRB.Leptic.Regosols", "TAXNWRB.Leptic.Umbrisols", "TAXNWRB.Lithic.Leptosols", "TAXNWRB.Lixic.Plinthosols", "TAXNWRB.Luvic.Calcisols", "TAXNWRB.Luvic.Chernozems", "TAXNWRB.Luvic.Phaeozems", "TAXNWRB.Luvic.Planosols", "TAXNWRB.Luvic.Stagnosols", "TAXNWRB.Mollic.Gleysols", "TAXNWRB.Mollic.Leptosols", "TAXNWRB.Mollic.Solonetz", "TAXNWRB.Mollic.Vertisols", "TAXNWRB.Petric.Calcisols", "TAXNWRB.Petric.Durisols", "TAXNWRB.Plinthic.Acrisols", "TAXNWRB.Protic.Arenosols", "TAXNWRB.Rendzic.Leptosols", "TAXNWRB.Sapric.Histosols", "TAXNWRB.Solodic.Planosols", "TAXNWRB.Stagnic.Luvisols", "TAXNWRB.Turbic.Cryosols", "TAXNWRB.Umbric.Albeluvisols", "TAXNWRB.Umbric.Ferralsols", "TAXNWRB.Umbric.Gleysols", "TAXNWRB.Vertic.Cambisols", "TAXNWRB.Vertic.Luvisols", "TAXNWRB.Vetic.Acrisols", "TAXNWRB.Vitric.Andosols", "TAXNWRB.Vitric.Cryosols")
    return(c(WRB, USDA))
}

# Names of the classes in the original data collected by IIASA
GetIIASAClassNames = function(AfricanOnly = FALSE)
{
    # There is also "not_sure", but it's pointless to predict that
    ClassNames = c("bare", "burnt", "crops", "fallow_shifting_cultivation", "grassland", "shrub",
             "tree", "urban_built_up", "water", "wetland_herbaceous")
    if (AfricanOnly)
        return(ClassNames)
    
    return(c(ClassNames, "lichen_and_moss", "snow_and_ice", "not_sure"))
}

# Class names that are in both training and validation sets
GetCommonClassNames = function()
{
    return(c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water"))
}

# List of classes with enough observations
GetLargeClassNames = function(data, threshold=50)
{
    Result = levels(data$dominant_lc)
    Freqs = table(data$dominant_lc)
    return(Result[Freqs > 50])
}
