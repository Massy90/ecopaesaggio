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
