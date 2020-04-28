# Codice per analisi dei point Patterns

#innstallare ggplot2
install.packages("ggplot2")
library(ggplot2)
install.packages("spatstat")
library(spatstat)


# preparare la work directory (Mac)
setwd("/Users/massimilianoapruzzese/Documents/lab")

# importare dataset ( Es. covid)
covid <- read.table("covid_agg.csv", head=TRUE)

# vedere tabella
head(covid)

# plot ($= collega una colonna al proprio dataset)
plot(covid$country, covid$case)
plot(covid$country, covid$case, las=0) #las= label della x= parallele
plot(covid$country, covid$case, las=1) #las= label della x= orizzontali
plot(covid$country, covid$case, las=2) #las= label della x= perpendicolari
plot(covid$country, covid$case, las=3) #las= label della x= verticali

# diminuire grandezza dei punti (cex)
plot(covid$country, covid$case, las=3, cex.lab=0.5) #non proprio funzionante

# in questo caso cex.axis=
plot(covid$country, covid$case, las=3, cex.axis=0.5)

# ggplot2 per fare plot fighi

# ggplot2
data(mpg)
head(mpg)

# data
# aest (estetica del grafico e variabili)
# tipo di geometria
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_line() # tipo per l'analisi dei trend
ggplot(mpg, aes(x=displ, y=hwy)) + geom_poligon()

# ggplot di covid
gglpot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

# density (le aree con la densità più alta rispetto alla quantità di dati)
# create dataset for spatstat
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90)) #(c= range dei vettori)

d <- density(covids)
plot(d)
points(covids, pch=19)

# natureearth= database internazionale
# salvare R data(workspace)

# save .RDATA

# recuperiamo i dati interno alla cartella "lab"
setwd("/Users/massimilianoapruzzese/Documents/lab")

# caricare .RDATA
load("spatstat_31_03_2020.RData")

# ls() vedere i file
ls()

library(spatstat)
plot(d)

# palette dei colori
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)

plot(d, col=cl)

# Esercizio della mappa di densità dal verde al blu

cl2 <- colorRampPalette(c('red','orange','yellow','green', 'blue')) (300)
plot(d, col=cl2)

points(covids)

# inserire i file delle coastlines
coastlines <- readOGR("ne_10m_coastline.shp")
# Prima installare e richiamare libreria rgdal

install.packages("rgdal")
library(rgdal)

# plottare coastlines
plot(coastlines, add=TRUE)

# Esercizio: plot della mappa di densità con nuova colorazione e aggiunta delle coastlines

# cambiare colore alle coastline
plot(coastlines, add=T, col= "grey")

# Interpolation
attach(covid)
marks(covids) <- covid$cases
s <- Smooth(covids)
plot(s)
cl3 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl3, main="COVID")
points(covids)
plot(coastlines, add=T, col= "dark grey")
dev.off()

par(mfrow=c(2, 1)) # both plots

plot(d, col=cl2, main="Density")
points(covids)
plot(coastlines, add=T, col= "dark grey")
plot(s, col=cl3, main="Interpolation")
points(covids)
plot(coastlines, add=T, col= "dark grey")

## S Marino
setwd("/Users/massimilianoapruzzese/Documents/lab")
load("Tesi.RData")
library(spatsat)
attach(Tesi)

# spatsat x, y, c(xmin, xmax) c(ymin, ymax)
summary(Tesi)
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95))
Tesid <- density(Tesippp) 
plot(Tesid)

# Interpolazione
load("sanmarino.RData")
class(dT)
"im"
class(Tesi)
"tbl_df"     "tbl"        "data.frame"
class(Tesid)
"im"
class(Tesippp)
"ppp"

marks(Tesippp) <- Tesi$Species_richness
interpol <- Smooth(Tesippp)

# plot sanmarino
plot(interpol)
points(Tesippp, col="green")
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp, col="green") # mettere i confini sopra

# exercise multiframe
par(mfrow=c(2,1))
plot(dT, main="density")
points(Tesippp, col="green")
plot(sanmarino, main="San Marino")
plot(interpol, add=T, main="San Marino")
plot(sanmarino, add=T)
points(Tesippp, col="green")

# orizontal
par(mfrow=c(1, 2))



























