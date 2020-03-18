# PRIMO SVILUPPO DI CODICE
> install.packages("sp")
trying URL 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.5/sp_1.4-1.tgz'
Content type 'application/x-gzip' length 1875963 bytes (1.8 MB)
==================================================
downloaded 1.8 MB


The downloaded binary packages are in
	/var/folders/cm/mv9zgz696z19z9swwkvphg8r0000gn/T//RtmpJpsErY/downloaded_packages
> 
> install.packages(sp)
Error in install.packages : object 'sp' not found
> library("sp", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
Warning message:
package ‘sp’ was built under R version 3.5.2 
> library(sp)
> data("meuse")
> meuse
> summery
Error: object 'summery' not found
> 
> summery(meuse)
> names(meuse)
 [1] "x"       "y"       "cadmium" "copper"  "lead"    "zinc"   
 [7] "elev"    "dist"    "om"      "ffreq"   "soil"    "lime"   
[13] "landuse" "dist.m" 
Error in summery(meuse) : could not find function "summery"
> install.packages(summery)
Error in install.packages : object 'summery' not found
> pairs(meuse)
> pairs(~ cadmium + copper + lead , data = meuse)
# Exercise: cadmium copper zinc
> pairs(~ cadmium + copper + + lead + zinc , data = meuse)
 
> pairs(meuse[3:6])

> pairs(meuse[,3:6], col="green")

> pairs(meuse[,3:6], col="green", pch=19, cex=0.3, main="first pairs")

> names(meuse)
 [1] "x"       "y"       "cadmium" "copper"  "lead"    "zinc"   
 [7] "elev"    "dist"    "om"      "ffreq"   "soil"    "lime"   
[13] "landuse" "dist.m" 
> pairs(meuse[,3:7], col="green", pch=19, cex=0.3, main="first pairs")

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

########### funz. plot

plot(meuse$cadmium, meuse$copper)

attach(meuse)

plot(cadmium,copper)

> plot(cadmium,copper, pch=17, col="blue", main="primo plot")
> plot(cadmium,copper, pch=17, col="blue", main="primo plot", xlab="cadmio", ylab="rame")
