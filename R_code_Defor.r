# setwd
setwd("/Users/massimilianoapruzzese/Documents/lab")
library(raster)

# caricare dati da esterno con brick
defor1 <- brick("/Users/massimilianoapruzzese/Documents/lab/defor1.jpg")
defor2 <- brick("/Users/massimilianoapruzzese/Documents/lab/defor2.jpg")
defor1

# names      : defor1.1, defor1.2, defor1.3
# defor1.1 = NIR
# defor1.2 = red
# defor1.3 = green

# plotRGB
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# class non super visionata
library(RStoolbox)
d1c <- unsuperClass(defor1, nClasses=2)
d1c
plot(d1c$map)

# possibilità 2
cl <- colorRampPalette(c('green','black'))(100) # 
plot(d1c$map, col=cl)

# class defor2
d2c <- unsuperClass(defor2, nClasses = 2)
plot(d2c$map, col=cl)
dev.off()
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

freq(d1c$map)
# foreste 305319
# aree aperte 35973

totd1 <- 305319+35973
totd1
# totd1 = 341292

# %
percent1 <- freq(d1c$map) *100/totd1
percent1
# aree 89.5%, foresta 10.5%

freq(d2c$map)
# foreste 165055
# aree coperte 177671
totd2 <- 165055 + 177671
totd2
# totd2 = 342726

# %
percent2 <- freq(d2c$map) * 100 / totd2
percent2
# aree 52%, foresta 48%

cover <- c("Forest", "Agriculture")
before <- c(89.5, 10.5)
after <- c(52, 48)
output <- data.frame(cover,before, after)
View(output)

# plottare output
library(ggplot2)
?ggplot2

# Defor.RData

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

################## plottare output ggplot2
library(ggplot2)
?ggplot2

# before
g1 <- ggplot(output, (aes(x=cover, y=before, color=cover)))+ geom_bar(stat="identity", fill="white")

# after
g2 <- ggplot(output, (aes(x=cover, y=after, color=cover)))+ geom_bar(stat="identity", fill="white")

# altro pacchetto per fare un par con ggplot
install.packages("gridExtra")
library(gridExtra)
?gridExtra

grid.arrange(g1, g2, nrow=1)



