# R code land cover

setwd("/Users/massimilianoapruzzese/Documents/lab")
library(raster)
library(sp)
library(RStoolbox)

p224r63_2011 <- brick("p224r63_2011_masked.grd")

# plotRGB
# landsat bands: 1b, 2g, 3r, 4nir;

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#UnsuperClass
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses = 4)
p224r63_2011c

plot(p224r63_2011c$map)

clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# in funzione del n° di classi aumenta l'incertezza dell'algoritmo automatico di classificazione
# riportando classi leggermente differenti

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses = 4)
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses = 2)
