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
