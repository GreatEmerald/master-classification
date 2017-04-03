# Tool for visulising the harmonic metrics in one plot.
source("utils/load-data.r")

alldata = LoadClassificationData()

tscurve = function(classname, ...)
{
    clsi = alldata@data$dominant == classname & alldata@data$pure
    curvefunc = function(x)
    {
        # 1 -> 0, 12 -> tau
        x = x-1 # 0-11
        x = x/11*2*pi
        mean(alldata@data[clsi,"mean.ndvi"])+
        mean(alldata@data[clsi,"amplitude1"])*cos(x+mean(alldata@data[clsi,"phase1"]))+
        mean(alldata@data[clsi,"amplitude2"])*cos(2*(x+mean(alldata@data[clsi,"phase2"])))
    }
    curve(curvefunc, from=1, to=12, ylab="NDVI", xlab="Month", xaxp=c(1, 12, 11), ylim=c(0, 1), ...)
}

opar = par()
#par(mar=c(5.1,4.1,1.1,8.1))
tscurve("cropland", col="gold")
tscurve("dec.trees", col="green3", add=TRUE)
tscurve("evgr.trees", col="darkgreen", add=TRUE)
tscurve("shrubland", col="purple", add=TRUE)
tscurve("grassland", col="lawngreen", add=TRUE)
tscurve("wetland", col="springgreen", add=TRUE)
tscurve("bare.soil", col="khaki4", add=TRUE)
tscurve("urban", col="red", add=TRUE)
tscurve("water", col="blue", add=TRUE)
par(xpd=TRUE)
legend(10, 1.3, lty=1,
    legend=c("Crops", "Dec. trees", "Evgr. trees", "Shrubs", "Grass", "Wetland", "Barren", "Built-up", "Water"),
    col=c("gold", "green3", "darkgreen", "purple", "lawngreen", "springgreen", "khaki4", "red", "blue"))
par(opar)
