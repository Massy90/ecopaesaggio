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


