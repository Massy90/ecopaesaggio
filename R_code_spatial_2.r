####### R spatial 2

libreria sp
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

# bubble
bubble(meuse,"zinc")

# bubble in rosso x rame
bubble(meuse,"copper", col="red")

# foraminiferi (Sofia), carbon capture(Marco)

# array (vettore)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

#plot dei dati per vedere se sono relazionati (esempio)
plot(foram, carbon, col="green", cex=2, pch=19)

# Dati dall'esterno: covid-19
# Specificare al softwear R che cartella prendere per i dati: Mac: documenti/lab
setwd("Documents/lab")
# Per me col Mac: setwd("/Users/massimilianoapruzzese/Documents/lab") 

#leggere la tabella
read.table("covid_agg.csv")

# dire a R i titoli: header
covid <- read.table("covid_agg.csv", head=TRUE)
