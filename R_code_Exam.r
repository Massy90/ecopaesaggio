# R code exam
# 1. R_code_first.r   
#2. R_code_spatial.r   
#3. R_code_spatial2.r
#4. R_code_point_pattern   
#5. R_code_teleril.r   
#6. R_code_landcover.r   
#7. R_code_multitemp.r   
#8. R_code_multitemp_NO2.r   
#9. R_code_snow.r   
#10. R_code_patches.r 

### 1. r code first
# PRIMO SVILUPPO DI CODICE

 install.packages(sp)
 library(sp)
 data("meuse")
 meuse
 
 summery(meuse)

 pairs(meuse[3:6])

 pairs(meuse[,3:6], col="green")

 pairs(meuse[,3:6], col="green", pch=19, cex=0.3, main="first pairs")

 pairs(meuse[,3:7], col="green", pch=19, cex=0.3, main="first pairs")

# funzioni prof.

panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=white", ...)
}
pairs(meuse[, 3:6], lower.panel= panel.correlation, upper.panel= panel.smoothing, diagonal panel= 
panel.histograms) 

# PLOTTARE OBJECTIVES DI UNA TABELLA
plot(meuse$cadmium, meuse$copper)

attach(meuse)

plot(cadmium,copper)

> plot(cadmium,copper, pch=17, col="blue", main="primo plot")
> plot(cadmium,copper, pch=17, col="blue", main="primo plot", xlab="cadmio", ylab="rame")
#####################################################################
#####################################################################
#####################################################################

### r code Spatial
# R spatial funzioni spaziali

# INSTALLARE  e RICHIAMARE pacchetto sp
install.packages("sp")
library(sp)

# RICHIAMARE DATI
data(meuse)

# VISUALIZZARE PRIME 6 RIGHE
head(meuse)

# PLOT cadmium e lead & ALLEGARE il DATAFRAME (database)

attach(meuse)

plot(cadmium, lead, col="pink", pch=2, cex=2)

# exercise: plot di copper e zinco con carattere triangolo e colore verde

plot(copper, zinc, col="green", pch=17, cex=2)

# CAMBIARE LE ETICHETTE xlab ylab
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco")

# FUNZIONE MULTIFRAME O MULTIPANNEL (PIù GRAFICI INSIEME)
par(mfrow=c(1,2))
plot(cadmium, lead, col="pink", pch=2, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2)

# CAMBIO POSIZIONE GRAFICI NEL MULTIFRAME, 2 RIGHE, 1 COLONNA 
par(mfrow=c(2,1))
plot(cadmium, lead, col="blue", pch=17, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2)

# MULTIFRAME AUTOMATICO
install.packages("GGally")

ggpairs(meuse[,3:6])

# SPATIAL PLOT
head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

# FUNZIONE SPPLOT PER PLOTTARE I DATI SPAZIALI
spplot(meuse, "zinc")
#########################################################################
#########################################################################
#########################################################################

### r code Spatial.2
####### R spatial 2

library(sp)

# dati da usare
data(meuse)
head(meuse)

# coord. del dataset (in R= dataframe)
coordinates(meuse)=~x+y

#spplot dei dati di zinco
spplot(meuse, "zinc")

# exercise: spplot dei dati di rame
spplot(meuse, "copper")

# bubble CREA UN BUBBLE PLOT DI DATI SPAZIALI, CON GRANDEZZA DEI BUBBLE PARI ALLA DENSITà DEL DATO SPAZIALE
bubble(meuse,"zinc")

# bubble in rosso x rame
bubble(meuse,"copper", col="red")

# foraminiferi (Sofia), carbon capture(Marco)

# CREARE UN ARRAY (O VETTORE)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# PLOTTARE DATI PER VEDERE SE C'è UNA RELAZIONE TRA ESSI
plot(foram, carbon, col="green", cex=2, pch=19)

# Dati dall'esterno: covid-19
# SPECIFICARE AD R DA CHE CARTELLA PRENDERE I DATI, OVVERO LA SET WORKING DIRECTORY

# Per Mac:
setwd("/Users/massimilianoapruzzese/Documents/lab") 

# FUNZIONE PER LEGGERE UNA TABELLA 
read.table("covid_agg.csv")

# dDIRE AD R DI INSERIRE I "TITOLI": HEADERS
covid <- read.table("covid_agg.csv", head=TRUE)

########################################################################
#########################################################################
#########################################################################

### r code Point Pattern

# Codice per analisi dei point Patterns

#innstallare ggplot2
install.packages("ggplot2")
library(ggplot2)
install.packages("spatstat")
library(spatstat)


# preparare la work directory (Mac)
setwd("/Users/massimilianoapruzzese/Documents/lab")

# importare dataset (Es. covid)
covid <- read.table("covid_agg.csv", head=TRUE)
head(covid)

# PLOT ($= COLLEGA UNA COLONNA CHE VOGLIAMO, AL DATASET)
plot(covid$country, covid$case)
plot(covid$country, covid$case, las=0) #las= label della x= parallele
plot(covid$country, covid$case, las=1) #las= label della x= orizzontali
plot(covid$country, covid$case, las=2) #las= label della x= perpendicolari
plot(covid$country, covid$case, las=3) #las= label della x= verticali

# ARGOMENTO PER INGRANDIRE/DIMINUIRE DIMENSIONE DEI PUNTI (cex)
plot(covid$country, covid$case, las=3, cex.lab=0.5) #non proprio funzionante

# IN QUESTO CASO cex.axis=
plot(covid$country, covid$case, las=3, cex.axis=0.5)

# ggplot2 PER PLOTTARE IN MODO COOL!!!

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

# density (LE AREE CON LA DENSITà PIù ALTA RISPETTO ALLA QUANTITà DI DATI)

# CREATE dataset for spatstat and PLOT
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90)) #(c= range dei vettori)
d <- density(covids)
plot(d)
points(covids, pch=19)

### RECUPERO DATI ALL'INTERNO DELLA CARTELLA "lab"
setwd("/Users/massimilianoapruzzese/Documents/lab")

# CARICARE .RDATA
load("spatstat_31_03_2020.RData")

# ls() PER VEDERE I FILES
ls()
# CARICARE spatsat
library(spatstat)
plot(d)

# CREAZIONE DELLA PALETTE DI COLORI DA ASSEGNARE NEI NOSTRI PLOT
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d, col=cl)

# Esercizio della mappa di densità dal verde al blu
cl2 <- colorRampPalette(c('red','orange','yellow','green', 'blue')) (300)
plot(d, col=cl2)
points(covids)

# INSERIRE DATI SHP IN COASTLINE
coastlines <- readOGR("ne_10m_coastline.shp")

# PRIMA INSTALLARE E POI RICHIAMARE LIBRERIA rgdal
install.packages("rgdal")
library(rgdal)

# plottare COASTLINE
plot(coastlines, add=TRUE)

# Esercizio: plot della mappa di densità con nuova colorazione e aggiunta delle coastlines

# CAMBIARE COLORE alle coastline
plot(coastlines, add=T, col= "grey")

# INTERPOLAZIONE
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

############### S. Marino
# dT=density map, Tesi=dataset originale, Tesippp=point pattern

setwd("/Users/massimilianoapruzzese/Documents/lab")
load("Tesi.RData")
library(spatsat)
attach(Tesi)

# spatsat x, y, c(xmin, xmax) c(ymin, ymax)
summary(Tesi)
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95))
Tesid <- density(Tesippp) 
plot(Tesid)

# INTERPOLAZIONE
load("sanmarino.RData")
# CONTROLLO DATASET
class(dT)
"im"
class(Tesi)
"tbl_df"     "tbl"        "data.frame"
class(Tesid)
"im"
class(Tesippp)
"ppp"

# ESTRAI O MODIFICA I MARKS ASSOCIATI A UN PLOT DI SET DI DATI
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
points(ppp, col="green")
plot(sanmarino, main="San Marino")
plot(interpol, add=T, main="San Marino")
plot(sanmarino, add=T)
points(Tesippp, col="green")

# PLOT ORIZZONTALE
par(mfrow=c(1, 2))
########################################################################
#########################################################################
#########################################################################

### 5. r code Telerilevamento

# codice R per analisi di immagini satellitari

#set work space
setwd("/Users/massimilianoapruzzese/Documents/lab")

# INSTALLARE O RICHIAMARE PACCHETTI
library(raster)
library(ggplot2)
library(RStoolbox)
library(rgdal)

# file da caricare: p224r63_2011_masked.grd
p224r63<- brick("p224r63_2011_masked.grd")
p224r63_2011 <- brick("p224r63_2011_masked.grd") # ci piace di +

# plottare
plot(p224r63_2011)

##### salvare R.data

# setwd
setwd("/Users/massimilianoapruzzese/Documents/lab")
# Load file
load(name_file.Rdata)

# plot
plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# IMPOSTARE color rampe palette
cl <- colorRampPalette(c('black','grey','light grey'))(100)

# CAMBIARE col al plot
plot(p224r63_2011, col=cl)

# COLORRAMP PALETE SOLO DELLA BANDA 1 COL BLU
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

## attach(data frame) NON FUNZIONA CON PACCHETTO RASTER
## $ SIMBOLO CHE LEGA LA COLONNA (ES. LA BANDA) AL DATASET (IMAGE)

plot(p224r63_2011$B1_sre, col=clb)

# Exercise : plottare banda dell'infrared
# color ramp palette che varia dal red, orange, yellow

clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_2011$B4_sre, col=clIR)

# PLOT A 4 BANDE (multiframe)
par(mfrow=c(2,2))

#blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

#green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg)

#red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_2011$B3_sre, col=clr)

#NIR
clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_2011$B4_sre, col=clIR)

# chiudere immagini appena plottate
dev.off()

# natural colors
# R G B= 3 componenti
# plotRGB(p224r63, r=3, g=2, b=1)
# B1: blue
# B2: green
# B3: red

plotRGB(p224r63_2011, r=3, g=2, b=1)

# SE L'IMMAGINE è SCURA -> SI USA L'ARGOMENTO stretch! pe strecciare i colori
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")

# utilizziamo NIR per vedere la vegetazione
plotRGB(p224r63, r=4, g=3, b=2, stretch="Lin")

# SALVARE .pdf
pdf("primgraf.pdf")
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")
dev.off

#multiframe
par(mfrow=c(1,2))
plotRGB(p224r63, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")

# utilizziamo NIR per vedere la vegetazione
# Exerc: nir nella componente green
plotRGB(p224r63, r=3, g=4, b=1, stretch="Lin")

# nir -> blue
plotRGB(p224r63, r=3, g=2, b=4, stretch="Lin")

### p224r63_1988 <- brick("p224r63_1988_masked.grd") 
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# plottare
plot(p224r63_1988)

# 4 bands plot (multiframe)
par(mfrow=c(2,2))

# blu
plot(p224r63_1988$B1_sre, col=clb)

#green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)

#red
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre, col=clr)
#NIR
clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_1988$B4_sre, col=clIR)

# chiudere immagini appena plottate
dev.off()

# natural colors
# R G B= 3 componenti
# plotRGB(p224r63, r=3, g=2, b=1)
# B1: blue
# B2: green
# B3: red

plotRGB(p224r63_1988, r=3, g=2, b=1)

# scura -> si utilizza stretch! pe strecciare i colori (argomento)
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# utilizziamo NIR per vedere la vegetazione
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# salvare pdf.
pdf("primgraf.pdf")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
dev.off

#multiframe
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Exercise: plot image -> NIR

# MULTIFRAME PLOTS 1988 e 2011
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

# SPECTRAL INDICES
# div1988 = nir1988-red1988

dvi1988 <- (p224r63_1988$B4_sre - p224r63_1988$B3_sre)

# exs: div 2011
dvi2011 <- (p224r63_2011$B4_sre - p224r63_2011$B3_sre)

# multitemporal analysis
difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)

# visualize the uotput
# multiframe 1988rgb, 2011rgb, difdiv

par(mfrow=c(3, 1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# changing the grain (CAMBIARE LA RISOLUZIONE)
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
par(mfrow=c(2, 1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolut.
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

# original 30m
par(mfrow=c(3, 1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

# multiframe finale
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

###################################################################
###################################################################
###################################################################

### 6. r code Multitemp

# setwd
setwd("/Users/massimilianoapruzzese/Documents/lab")
library(raster)

# caricare dati da esterno con brick
defor1 <- brick("/Users/massimilianoapruzzese/Documents/lab/defor1.jpg")
defor2 <- brick("/Users/massimilianoapruzzese/Documents/lab/defor2.jpg")

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

# CLASSIFICAZIONE NON SUPER VISIONATA (classificazione dei colori in base ai pixel)
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
# plot di entrambe le immagini classificate
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
library(ggplot2)
p1<-ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") 
p2<-ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

####################################################
####################################################
#####################################################

### 8. r code Multitemp_NO2

# Rcode for analysing NO2 data from ESA jan to mar 2020

setwd("/Users/massimilianoapruzzese/Documents/lab")

library(raster)

# raster
EN01 <- raster("EN_0001.png")
dev.off()
plot(EN01)
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# difference
dev.off()
difno2 <- EN13-EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 

# PLOT ALL THE DATA
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

# par plots
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

# IMPORTATE TUTTE LE IMMAGINI INSIEME E PLOTTARE (stack)
setwd("/Users/massimilianoapruzzese/Documents/lab/esa_no2")
cl <- colorRampPalette(c('red','orange','yellow'))(100)
rlist <- list.files(pattern=".png", full.names = T)

# RETURN THE LIST
list1 <- lapply(rlist, raster)
EN <- stack(list1)
plot(EN, col=cl)

############################################################
############################################################
#############################################################

### 9. r code Snow

setwd("/Users/massimilianoapruzzese/Documents/lab")
install.packages("ncdf4")
library(ncdf4)
library(raster)
library(rgdal)
library(spatstat) # for random points
library(maptools)

# PRIMO PLOT DI SNOWMAY
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col =cl)

# cartella snow -> PER PLOT MULTITEMPORALE DI SNOWMAY
setwd("/Users/massimilianoapruzzese/Documents/lab/snow")
rlist <- list.files(pattern=".tif", full.names = T)
list <- lapply(rlist, raster)
snow.multitemp <- stack(list)
plot(snow.multitemp, col=cl)

# plot snow2000 e snow2020
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,200))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,200))

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r

cl2 <- colorRampPalette(c("blue", "white", "red"))(100)
dev.off()
plot(difsnow, col=cl2)

## prediction = download prediction.R from IOL

source("prediction.r")

# too much time plot "predicted.snow.2025.norm.tif"
pred.snow2025 <- raster("predicted.snow.2025.norm.tif")
plot(pred.snow2025, col=cl)

###############################################################
################################################################
#################################################################

### 10. r code patches

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
d1c.for <- reclassify(d1c, cbind(1,NA))
d2c.for <- reclassify(d2c, cbind(1,NA))

# plot dc1 dc2 forest
dev.off()
par(mfrow=c(1,2))
plot(d1c.for,col=cl)
plot(d2c.for, col=cl)

### CLUMP FUNCTION TO CREATE PATCHES
install.packages("igraph")
library(igraph)
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

dev.off()
par(mfrow=c(1,2))
cl <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) #
plot(d1c.for.patches,col=cl)
plot(d2c.for.patches, col=cl)

# SALVARE UN FILE ED ESPORTARLO
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

### MAX PATCHES d1 = 301
### MAX PATCHES d2 = 1212
time <- c("Bf deforest", "Af deforest")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

#ggplot
library(ggplot2)
dev.off()
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

##############################################################
################################################################
#################################################################

###



