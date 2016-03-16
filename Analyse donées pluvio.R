

### typologie pluie


#vider la mémoire des travaux précédents
rm(list=ls())

## définition de l'opération "ni" (l'inverse de "in")
`%ni%` <- Negate(`%in%`)

#install.packages("openxlsx")
#install.packages("ggthemes")
#install.packages("scales")


### chargement packages
require("ggplot2")   ## pour faire des jolis graphs
library("cowplot")   ## pour  afficher plusieurs ggplot
library("fortunes")
library("lattice")
library("ggthemes")
library("grid")
library("openxlsx")
library("plotly")
library("xts")
library("openair")
library("Cairo" ) ## pour exporter correctement les graphs
library("reshape2")
require("scales")

#theme_set(theme_gray())

my.theme = theme_grey() + theme(text=element_text(family='Roboto', size=30))

fortune(15)


## import des mesure

setwd("C:/Users/XZ4062/Desktop/LyRE/Monitoring/Sirenes/2_BDX_M/Valorisation des donnees/2_data/2015/pluvio")
getwd()

colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)

## lecture du fichier excel(packages openxlsx)
#thil
pluies<- read.xlsx("Synthèse.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)


pluies$periode_retour <- as.factor(pluies$periode_retour)
head(pluies)
str(pluies)





radius <- sqrt( pluies$cumul)



p <- ggplot(pluies, aes(iMax,temps_sec,size=radius, label=Id, colour=periode_retour))+
  geom_point() +
  scale_size(range = c(0, 20))+
  xlab("Intensité max") + 
  ylab("Période de temps sec") +
  scale_y_log10() +
  scale_x_log10()+
  scale_colour_manual(values=rev(heat.colors(7,alpha=0.9)))+
  geom_text(size = 3, colour ="black" )


x11()
p


x11()
symbols(pluies$iMax, pluies$temps_sec, circles= radius,
        bg=pluies$periode_retour,fg="white",log = "xy")

text(pluies$iMax, pluies$temps_sec, pluies$Id , cex=0.5)

palette("default") 

ylim=c(0,5000), 
xlim=c(0,50))

