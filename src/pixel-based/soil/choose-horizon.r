# Empirically check which soil depth is best to use

source("pixel-based/utils/load-sampling-data.r")

SoilDataT = st_read("../data/pixel-based/soil/soilgrids-raw.gpkg", "Training")
Training = LoadGlobalTrainingData()

TrainingAndCovars = merge(Training, as.data.frame(SoilDataT), by.x=c("x","y"), by.y=c("X", "Y"))
rm(SoilDataT, Training)

# Pure agriculture
Agri = TrainingAndCovars[TrainingAndCovars$crops == 100, ]
# Pure trees
Tree = TrainingAndCovars[TrainingAndCovars$tree == 100, ]
# Pure grass
Gras = TrainingAndCovars[TrainingAndCovars$grassland == 100, ]
# Pure shrub
Shrb = TrainingAndCovars[TrainingAndCovars$shrub == 100, ]

PrettyDepth = function(Depth)
{
    switch(Depth, "1"="0-5 cm", "2"="5-15cm", "3"="15-30 cm", "4"="30-60 cm", "5"="60-100 cm", "6"="100-200cm")
}

PlotSoilParam = function(Var, Depth)
{
    MyData = rbind(
        data.frame(value=Agri[[paste0(Var,".M.sl", Depth)]], type="crops"),
        #data.frame(value=Tree[[paste0(Var,".M.sl", Depth)]], type="trees"),
        data.frame(value=Gras[[paste0(Var,".M.sl", Depth)]], type="grass"),
        data.frame(value=Shrb[[paste0(Var,".M.sl", Depth)]], type="shrub"))
    
    boxplot(Agri[[paste0(Var,".M.sl", Depth)]],
            #Tree[[paste0(Var,".M.sl", Depth)]],
            Gras[[paste0(Var,".M.sl", Depth)]],
            Shrb[[paste0(Var,".M.sl", Depth)]],
            main=paste(Var, PrettyDepth(Depth)),
            names=c("Crop",
                    #"Tree", 
                    "Grass",
                    "Shrub"))
    print(TukeyHSD(aov(value~type, MyData)))
}

# Most interesting covars: BLDFIE, AWCh/WWP, ORCDRC/OCSTHA
PlotSoilParam("BLDFIE", 1) # Big difference for trees, this is the biggest one
PlotSoilParam("BLDFIE", 2) # Biggest difference in grass vs crops
PlotSoilParam("BLDFIE", 3)
PlotSoilParam("BLDFIE", 4)
PlotSoilParam("BLDFIE", 5)
PlotSoilParam("BLDFIE", 6)

PlotSoilParam("ORCDRC", 1) # Small difference
PlotSoilParam("ORCDRC", 2) # Much bigger difference, mostly tree vs crop, but also biggest overall; grass vs crop
PlotSoilParam("ORCDRC", 3)
PlotSoilParam("ORCDRC", 4)
PlotSoilParam("ORCDRC", 5)
PlotSoilParam("ORCDRC", 6) # There is a difference between all three, but small

for (level in 1:6) { Agri[[paste0("PHIHOX.M.sl", level)]][Agri[[paste0("PHIHOX.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Gras[[paste0("PHIHOX.M.sl", level)]][Gras[[paste0("PHIHOX.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Shrb[[paste0("PHIHOX.M.sl", level)]][Shrb[[paste0("PHIHOX.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Tree[[paste0("PHIHOX.M.sl", level)]][Tree[[paste0("PHIHOX.M.sl", level)]] == 255] = NA }

PlotSoilParam("PHIHOX", 1) # Largest difference overall, shrubs stand out
PlotSoilParam("PHIHOX", 2) # Good compromise
PlotSoilParam("PHIHOX", 3)
PlotSoilParam("PHIHOX", 4)
PlotSoilParam("PHIHOX", 5)
PlotSoilParam("PHIHOX", 6) # Technically largest difference between non-trees, shrubs very acidic

for (level in 1:6) { Agri[[paste0("PHIKCL.M.sl", level)]][Agri[[paste0("PHIKCL.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Gras[[paste0("PHIKCL.M.sl", level)]][Gras[[paste0("PHIKCL.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Shrb[[paste0("PHIKCL.M.sl", level)]][Shrb[[paste0("PHIKCL.M.sl", level)]] == 255] = NA }
for (level in 1:6) { Tree[[paste0("PHIKCL.M.sl", level)]][Tree[[paste0("PHIKCL.M.sl", level)]] == 255] = NA }

PlotSoilParam("PHIKCL", 1)
PlotSoilParam("PHIKCL", 2) # Biggest, but without trees increases going down; shrubs stand out
PlotSoilParam("PHIKCL", 3)
PlotSoilParam("PHIKCL", 4)
PlotSoilParam("PHIKCL", 5)
PlotSoilParam("PHIKCL", 6) # Even bigger, but not for shrubs

PlotSoilParam("AWCh1", 1) # Most are fine, shrub vs crop is indistinguishable
PlotSoilParam("AWCh1", 2)
PlotSoilParam("AWCh1", 3)
PlotSoilParam("AWCh1", 4)
PlotSoilParam("AWCh1", 5) # This is the best for non-trees, grass > crop
PlotSoilParam("AWCh1", 6)

PlotSoilParam("AWCh2", 1) # Gets successively worse
PlotSoilParam("AWCh2", 2) # Both the first two are good for all non-trees
PlotSoilParam("AWCh2", 3)
PlotSoilParam("AWCh2", 4)
PlotSoilParam("AWCh2", 5)
PlotSoilParam("AWCh2", 6)

PlotSoilParam("AWCh3", 1) # Same, less of a difference than AWCh2
PlotSoilParam("AWCh3", 2)
PlotSoilParam("AWCh3", 3)
PlotSoilParam("AWCh3", 4)
PlotSoilParam("AWCh3", 5)
PlotSoilParam("AWCh3", 6)

PlotSoilParam("WWP", 1) # Biggest difference
PlotSoilParam("WWP", 2)
PlotSoilParam("WWP", 3)
PlotSoilParam("WWP", 4)
PlotSoilParam("WWP", 5)
PlotSoilParam("WWP", 6)

# Layer 2 seems a good compromise
