setwd("/Users/massimilianoapruzzese/Documents/lab")
install.packages("ncdf4")
library(ncdf4)
library(raster)
library(rgdal)
library(spatstat) # for random points
library(maptools)

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

dev.off()
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black', "green"))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

# forest: class 2; agriculture: class 1
d1c.for <- reclassify(d1c, cbind(1, NA))

# for
d2c.for <- reclassify(d2c, cbind(1,NA))

# plot dc1 dc2 forest
dev.off()
par(mfrow=c(1,2))
plot(d1c.for,col=cl)
plot(d2c.for, col=cl)

### clump function to create patches
install.packages("igraph")
library(igraph)
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

dev.off()
par(mfrow=c(1,2))
cl <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) #
plot(d1c.for.patches,col=cl)
plot(d2c.for.patches, col=cl)
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

### max patches d1 = 301
### max patches d2 = 1212
time <- c("Bf deforest", "Af deforest")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

#ggplot
library(ggplot2)
dev.off()
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

