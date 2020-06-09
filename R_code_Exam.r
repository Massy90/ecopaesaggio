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
#11. R_code_crop.r

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

# PLOTTARE OBJECTIVES OF A TABLE ($)
plot(meuse$cadmium, meuse$copper)

# HELPING THE DIRECT ACCESS TO DATA.FRAME COLOUMS 
attach(meuse)

# PRODUCE A GRAPHIC ON THE RELATION BETWEEN 2 QUANTITATIVE VARIABLES
plot(cadmium,copper)

# INSIDE THE FUNCTION YOU CAN CHANGES PARAMETERS TO CUSTOMIZE YOR GRAPH
> plot(cadmium,copper, pch=17, col="blue", main="primo plot")
> plot(cadmium,copper, pch=17, col="blue", main="primo plot", xlab="cadmio", ylab="rame")
#####################################################################
#####################################################################
#####################################################################

### R code Spatial
# R SPATIAL FUNCTIONS

# INSTALL AND RECALL sp PACKAGES 
install.packages("sp")
library(sp) # A PACKAGE THAT PROVIDE CLASSES AND METHODS FOR SPATIAL DATA: POINT, LINES, POLYGONS AND GRID

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
par(mfrow=c(1,2)) # 1 ROW, 2 COLUMNS
plot(cadmium, lead, col="pink", pch=2, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2)

# CAMBIO POSIZIONE GRAFICI NEL MULTIFRAME, 2 RIGHE, 1 COLONNA 
par(mfrow=c(2,1)) # 2 ROWS, 1 COLUMN
plot(cadmium, lead, col="blue", pch=17, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2)

# AUTOMATIC MULTIFRAME
# "GGALLY" EXTENDS "GGPLOT2" BY ADDING SEVERAL FUNCTIONS TO FACILITATE THE COMBINING OF GEOMETRIC OBJECTS
# WITH TRANSFORMED DATA
install.packages("GGally")

# ggplot2 GENERALIZED PAIRS PLOT
ggpairs(meuse[,3:6])

# SPATIAL PLOT
head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

# FUNZIONE SPPLOT PER PLOTTARE I DATI SPAZIALI
spplot(meuse, "zinc") # GRID PLOT METHODS FOR SPATIAL DATA WITH ATTRIBUTES

#########################################################################
#########################################################################
#########################################################################

### r code Spatial.2
####### R spatial 2

library(sp)

# dati da usare
data(meuse)
head(meuse)

#SET SPATIAL COORDINATES TO CREATE A SPATIAL OBJECT OR RECALL SPATIAL COORDINATES FROM A SPATIAL OBJECT
coordinates(meuse)=~x+y 

#spplot dei dati di zinco
spplot(meuse, "zinc")

# exercise: spplot dei dati di rame
spplot(meuse, "copper")

# bubble CREA UN BUBBLE PLOT DI DATI SPAZIALI, CON GRANDEZZA DEI BUBBLE PARI ALLA DENSITà DEL DATO SPAZIALE
bubble(meuse,"zinc")

# bubble IN RED FOR COPPER
bubble(meuse,"copper", col="red")

# foraminiferi (Sofia), carbon capture(Marco)

# CREARE UN ARRAY (VETTORE)
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

# DIRE AD R DI INSERIRE I "TITOLI": HEADERS
covid <- read.table("covid_agg.csv", head=TRUE)

########################################################################
#########################################################################
#########################################################################

### R code Point Pattern

# Codice per analisi dei point Patterns

#innstallare ggplot2
install.packages("ggplot2")
library(ggplot2)
install.packages("spatstat")
library(spatstat)


# preparare la work directory (Mac)
setwd("/Users/massimilianoapruzzese/Documents/lab")

# IMPORT DATASET (EX. covid)
covid <- read.table("covid_agg.csv", head=TRUE)
head(covid)

# PLOT ($ = TO LINK A COLUMN WE WANT, TO THE DATASET 
plot(covid$country, covid$case)
plot(covid$country, covid$case, las=0) #las= label della x= parallele
plot(covid$country, covid$case, las=1) #las= label della x= orizzontali
plot(covid$country, covid$case, las=2) #las= label della x= perpendicolari
plot(covid$country, covid$case, las=3) #las= label della x= verticali

# ARGOMENTO PER INGRANDIRE/DIMINUIRE DIMENSIONE DEI PUNTI (cex)
plot(covid$country, covid$case, las=3, cex.lab=0.5) #non proprio funzionante

# IN QUESTO CASO cex.axis=
plot(covid$country, covid$case, las=3, cex.axis=0.5)

# ggplot2 TO PLOT COOL GRAPHS!!!

# ggplot2
data(mpg)
head(mpg)

# data
# aest AESTHETICS OF GRAPH
# geom_ GEOMETRY TYPE
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_line() # tipo per l'analisi dei trend
ggplot(mpg, aes(x=displ, y=hwy)) + geom_poligon()

# ggplot di covid
gglpot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

# density (LE AREE CON LA DENSITà PIù ALTA RISPETTO ALLA QUANTITà DI DATI)

# CREATE dataset for spatstat and PLOT
attach(covid)

# CREATS AN OBJECT OF CLASS "ppp" REPRESENTING A POINT PATTERN DATASET IN THE TWO DIMENSIONAL PLANE
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90)) #(c= range dei vettori) 
d <- density(covids)
plot(d)
points(covids, pch=19)

### RECUPERO DATI ALL'INTERNO DELLA CARTELLA "lab"
setwd("/Users/massimilianoapruzzese/Documents/lab")

# CARICARE .RDATA
load("spatstat_31_03_2020.RData")

# ls() TO SEE FILES
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
coastlines <- readOGR("ne_10m_coastline.shp") #THE FUNCTION READS AN OGR DATA SOURCE AND LAYER INTO A SUITABLE SPATIAL VECTOR OBJECT

# PRIMA INSTALLARE E POI RICHIAMARE LIBRERIA rgdal
install.packages("rgdal")
library(rgdal) # PROVIDES BINDINGS TO THE GEOSPATIAL DATA ABSTRACTION LIBRARY (GDAL)
               # AND ACCESS TO PROJECTION AND TRANSFORMATION OPERATIONS FROM THE PROJ.4 LIBRARY

# plottare COASTLINE
plot(coastlines, add=TRUE)

# Esercizio: plot della mappa di densità con nuova colorazione e aggiunta delle coastlines

# CAMBIARE COLORE alle coastline
plot(coastlines, add=T, col= "grey")

# INTERPOLAZIONE
attach(covid)
marks(covids) <- covid$cases
s <- Smooth(covids) #  FUNCTIONS IMPLEMENTING SINGLE SOURCE OF ERROR STATE SPACE MODELS
                    # FOR PURPOSES OF TIME SERIES ANALYSIS AND FORECASTING

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

# ESTRAE O MODIFICA I MARKS ASSOCIATI A UN PLOT DI SET DI DATI
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

########################################################################
#########################################################################
#########################################################################

### 5. r code Telerilevamento

# codice R per analisi di immagini satellitari

#set work space
setwd("/Users/massimilianoapruzzese/Documents/lab")

# RECALL PACKAGES
library(raster)
library(ggplot2)
library(RStoolbox)
library(rgdal)

# FILE TO DOWNLOAD: p224r63_2011_masked.grd
# BRICK CREATE A RASTER BRICK OBJECT FROM A MULTI-LAYER (BAND) FILE. 
p224r63<- brick("p224r63_2011_masked.grd")
p224r63_2011 <- brick("p224r63_2011_masked.grd") # ci piace di +

# plottare
plot(p224r63_2011)

##### HOW TO SAVE R.data

# setwd
setwd("/Users/massimilianoapruzzese/Documents/lab")
# Load file
load(name_file.Rdata)
#####

plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# SET GENERAL COLOR RAMPE PALETTE
cl <- colorRampPalette(c('black','grey','light grey'))(100)

plot(p224r63_2011, col=cl)

# SET COLOR PALETTE FOR BLUE BAND (B1)
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

## attach(data frame) DOES NOT WORK WITH RASTER PACKAGES
## USE $ TO PLOT A SPECIFIC BAND

plot(p224r63_2011$B1_sre, col=clb)

# Exercise : PLOT NIR BAND
# color ramp palette che varia dal red, orange, yellow

clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_2011$B4_sre, col=clIR)

# PLOT ALL 4 BANDS (multiframe)
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

# CLEAR PLOT SPACE
dev.off()

# NATURAL COLOURS
# R G B= 3 componenti
# plotRGB(p224r63, r=3, g=2, b=1)
# B1: blue
# B2: green
# B3: red

plotRGB(p224r63_2011, r=3, g=2, b=1)

# IF DARK IMMAGE -> USE "STRETCH" ARGOUMENTS
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")

# USE NIR TO SEE VEGETATION
plotRGB(p224r63, r=4, g=3, b=2, stretch="Lin")

# SAVE .pdf FILE
pdf("primgraf.pdf")
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")
dev.off

# MULTIFRAME OF ORIGINAL RGB AND NIR BAND IN RED COMPONENT
par(mfrow=c(1,2))
plotRGB(p224r63, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")

# Exerc: NIR IN GREEN COMPONENT
plotRGB(p224r63, r=3, g=4, b=1, stretch="Lin")

# NIR IN BLUE COMPONENT
plotRGB(p224r63, r=3, g=2, b=4, stretch="Lin")

# PLOT p224r63_1988
p224r63_1988 <- brick("p224r63_1988_masked.grd")
plot(p224r63_1988)

# 4 BAND PLOT (MULTIFRAME)
par(mfrow=c(2,2))

# BLUE
plot(p224r63_1988$B1_sre, col=clb)

# GREEN
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg)

# RED
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre, col=clr)

# NIR
clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_1988$B4_sre, col=clIR)

# CLEAN PLOT
dev.off()

# natural colors
# R G B= 3 componenti
# plotRGB(p224r63, r=3, g=2, b=1)
# B1: blue
# B2: green
# B3: red

plotRGB(p224r63_1988, r=3, g=2, b=1)

# L'IMMAGINE è SCURA ->  "STRETCH" (ARGOUMENT)
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# USE NIR (NEAR INFRARED MEASUREMENT) per vedere la vegetazione
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# salvare pdf.
pdf("primgraf.pdf")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
dev.off

# MULTIFRAME 
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

### Exercise: plot image -> NIR

# MULTIFRAME PLOTS 1988 e 2011
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

# SPECTRAL INDICES
# div1988 = nir1988-red1988

dvi1988 <- (p224r63_1988$B4_sre - p224r63_1988$B3_sre)

# EXERCISE: div 2011 VEGETATION DIFFERENCE
dvi2011 <- (p224r63_2011$B4_sre - p224r63_2011$B3_sre)

# MULTITEMPORAL ANALYSIS OF VEGETATION DIFFERENCES
difdvi <- dvi2011 - dvi1988
plot(difdvi)
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)

# VISUALAZE THE UOTPUT WITH MULTIFRAME 1988rgb, 2011rgb, difdiv
par(mfrow=c(3, 1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# CHANGING THE GRAIN (RESOLUTION)
p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # SPATIAL AGGREGATION OF THEMATIC INFORMATION IN SPATIAL OBJECTS
par(mfrow=c(2, 1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# LOWER RESOLUTION
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

# MULTIFRAME RESOLUTION
par(mfrow=c(3, 1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") # 30m RESOLUTION
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") # 300m RESOLUTION
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin") # 1500m RESOLUTION

# VEGETATION DIFFERENCE AT LOWER RESOLUTION 1500m 
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50,col=cldifdvi)

# FINAL MULTIFRAME
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)

###################################################################
###################################################################
###################################################################

### 6. R code Multitemp

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

#ggplot FINALE
library(ggplot2)
dev.off()
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

##############################################################
################################################################
#################################################################

### 11. R code crop - exam simulation

# SETWD
setwd("/Users/massimilianoapruzzese/Documents/lab")

# exercise: upload the whole snow set
rlist <- list.files(pattern=".tif", full.names = T)
list <- lapply(rlist, raster)
snow.multitemp <- stack(list)

# PLOTTARE TUTTE LE IMMAGINI
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(snow.multitemp, col=clb)

# PLOT SOLO 1 IMMAGINE E CAMBIARE ESTENZIONE O FARE ZOOM (SU ITALIA)
plot(snow.multitemp$snow2010r, col=clb)
extension <- c(6, 20, 35, 50)
plot(snow.multitemp$snow2010r, col=clb, ext= extension)
zoom(snow.multitemp$snow2010r, col=clb, ext= drawExtent())

# PLOTTARE TUTTA LA SERIE MULTITEMPORALE CON ESTENSIONE SOLO SULL'ITALIA
ita <- crop(snow.multitemp, col=clb, extension)
plot(ita)

# METTERE LE LEGENDE TUTTE UGUALI
plot(ita, col= clb, zlim= c(20,200))

# BOXPLOT PER CONFRONTARE LA COPERTURA
boxplot(ita, horizontal=T,outline=F)

##########################################################################
##########################################################################
##########################################################################

########## SDM package - Babak Naimi
######## sdm IS AN OBJECT-ORIENTED, REPRODUCIBLE AND EXTENSIBLE, PLATFORM FOR SPECIES DITRIBUTION MODELLING
##################################
library(sdm)
library(raster)
library(rgdal)

install.packages("sdm")

# Check for species
file <- system.file("external/species.shp", package="sdm") # GET THE LOCATION OF THE SPECIES
species <- shapefile(file) #  READ THE SHAPFILE

species

species$Occurrence

plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# model
path <- system.file("external", package="sdm") # PATH TO THE FOLDER CONTAINS THE DATA

#predictors
lst <- list.files(path=path,pattern='asc$',full.names = T) # LIST THE NAME OF THE RASTER FILES

# STACK IS A FUNCTION IN THE RASTER PACKAGE, TO READ/CREATE A MULTI-LAYERES RASTER DATASET
preds <- stack(lst) # MACKING A RASTER OBJECT

# PLOT THE RASTER OBJECT
plot(preds)
plot(preds[[4]]) # JUST PLOT THE 4th LAYER
# plot(species,add=T)
points(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)


# MODEL
# d <- sdmData(formula=Occurrence~., train=species, predictors=preds)
# d

d <- sdmData(train=species, predictors=preds)
# You may also want to take part of variables:

# d <- sdmData(formula=Occurrence~precipitation+temperature, train=species, predictors=preds)

# FIT THE MODEL m1
m1 <- sdm(Occurrence~elevation+precipitation+temperature+vegetation,data=d,methods=c('glm'))

#  MAKE A RASTER (OR MATRIX) WITH PREDICTIONS FROM 1 OR SEVERAL MODELS IN sdmMODEL
p1 <- predict(m1,newdata=preds)
# ,filename='p1.img') 

# SET A PALETTE OF COLORS FOR THE PLOTTED IMAGE
clp <- colorRampPalette(c('darkblue','orange','red','yellow'))(100) # specifying a color scheme
plot(p1,col=clp)

# SPECIES PRESENT 1/ ABSENCE 0 AS DIFFERENT COLORED POINT ON MAP
points(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# FIT THE SECOND MODEL m2
m2 <- sdm(Occurrence~elevation+precipitation+temperature+vegetation,data=d,methods=c('gam'))

# SECOND PREDICTION
p2 <- predict(m2,newdata=preds) 
plot(p2,col=clp)

# STACK 2 PREDICTION BASED ON 2 DIFFERENT MODELS AND PLOT TOGETHER
stackpred <- stack(p1,p2)
plot(stackpred, col=clp)

##########################################################################
##########################################################################
##########################################################################

# THE END

