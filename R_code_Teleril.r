# codice R per analisi di immagini satellitari

#set work space
setwd("/Users/massimilianoapruzzese/Documents/lab")

# installare pacchetti o richiamare
library(raster)
library(ggplot2)
library(RStoolbox)
library(rgdal)

# file da caricare: p224r63_2011_masked.grd
p224r63<- brick("p224r63_2011_masked.grd")
p224r63_2011 <- brick("p224r63_2011_masked.grd") # ci piace di +

# plottare
plot(p224r63_2011)

# salvare R.data

# sewd
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

# color rampe palette
cl <- colorRampPalette(c('black','grey','light grey'))(100)

# cambiare col a plot
plot(p224r63_2011, col=cl)

# solo banda 1 col blu
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

# attach(data frame) non funziona con pacchetto raster
# simbolo che lega la colonna (es. la banda) al dataset (immagine satellitare)

plot(p224r63_2011$B1_sre, col=clb)

# Exercise : plottare banda dell'infrared
# color ramp palette che varia dal red, orange, yellow

clIR<- colorRampPalette(c("red", "orange", "yellow"))(100)
plot(p224r63_2011$B4_sre, col=clIR)

# 4 bands plot (multiframe)
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

# scura -> si utilizza stretch! pe strecciare i colori (argomento)
plotRGB(p224r63, r=3, g=2, b=1, stretch="Lin")

# utilizziamo NIR per vedere la vegetazione
plotRGB(p224r63, r=4, g=3, b=2, stretch="Lin")

# salvare pdf.
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

# p224r63_1988 <- brick("p224r63_1988_masked.grd") # ci piace di +
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

# multiframe 1988_2011
par(mfrow=c(1,2))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

# spetral indices
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

# changing the grain (resol.)
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)
par(mfrow=c(2, 1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolut.
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

#original 30m
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
