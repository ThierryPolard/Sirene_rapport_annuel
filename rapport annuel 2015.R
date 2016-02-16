##### Edition rapport annuel 2015#####
######################################
######################################

#vider la mémoire des travaux précédents
rm(list=ls())

## définition de l'opération "ni" (l'inverse de "in")
`%ni%` <- Negate(`%in%`)

#install.packages("openxlsx")
#install.packages("ggthemes")


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


#theme_set(theme_gray())

my.theme = theme_grey() + theme(text=element_text(family='Roboto', size=30))

fortune(15)

########

## import des mesure

setwd("C:/Users/XZ4062/Desktop/LyRE/Monitoring/Sirenes/2_BDX_M/Valorisation des donnees/1_données brutes/Rapport annuel 2015")
getwd()

## lecture du fichier excel(packages openxlsx)
#thil
thil_2014<- read.xlsx("thil_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                    rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
thil_2015<- read.xlsx("thil_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_2014<- read.xlsx("reserve_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
reserve_2015<- read.xlsx("reserve_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)


#fusion données 2014 et 2015 #####a voi si c'est une bonne idée..
#reserve <-rbind(reserve_2014,reserve_2015)
#thil <-rbind(thil_2014, thil_2015)


#sites = c("thil", "reserve")
#annee= c("2014","2015")
#liste = c("thil_2014","thil_2015","reserve_2014","reserve_2015") ## a automatiser
#???n <-length(liste)

# conversion des date en format "temps" ------- ## a automatiser------------------------
n<-ncol(thil_2014)
seq1<-seq(from = 1, to = n, by=2)
for (i in seq1)
  ifelse(is.na(thil_2014[[i]]),print("NA"),
         thil_2014[[i]] <- as.POSIXct(thil_2014[[i]], format="%d/%m/%Y %H:%M",tz="GMT"))
 
n<-ncol(thil_2015)
seq1<-seq(from = 1, to = n, by=2)
for (i in seq1)
  ifelse(is.na(thil_2015[[i]]),print("NA"),
  thil_2015[[i]] <- as.POSIXct(thil_2015[[i]], format="%d/%m/%Y %H:%M",tz="GMT"))

n<-ncol(reserve_2014)
seq1<-seq(from = 1, to = n, by=2)
for (i in  seq1)
  ifelse(is.na(reserve_2014[[i]]),print("NA"),
  reserve_2014[[i]] <-as.POSIXct(reserve_2014[[i]], format="%d/%m/%Y %H:%M",tz="GMT"))

n<-ncol(reserve_2015)
seq1<-seq(from = 1, to = n, by=2)
for (i in  seq1)
  ifelse(is.na(reserve_2015[[i]]),print("NA"),
  reserve_2015[[i]] <- as.POSIXct(reserve_2015[[i]], format="%d/%m/%Y %H:%M",tz="GMT"))

## Création de la matrice temporelle de référence -------

#Création du vecteur "temps"
debut_2014 <- as.POSIXct(strptime("01/01/2014 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin_2014 <- as.POSIXct(strptime("31/12/2014 23:45:00",  format="%d/%m/%Y %H:%M",tz="GMT"))

debut_2015 <- as.POSIXct(strptime("01/01/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin_2015 <- as.POSIXct(strptime("31/12/2015 23:45:00",  format="%d/%m/%Y %H:%M",tz="GMT"))

date<-seq(debut_2014,fin_2015,by=15*60)
date_2014<-seq(debut_2014,fin_2014,by=15*60)
date_2015<-seq(debut_2015,fin_2015,by=15*60)

date[[1]] <- as.POSIXct (date[[1]], format="%d/%m/%Y %H:%M",tz="GMT")
date_2014[[1]] <- as.POSIXct (date_2014[[1]], format="%d/%m/%Y %H:%M",tz="GMT")
date_2015[[1]] <- as.POSIXct (date_2015[[1]], format="%d/%m/%Y %H:%M",tz="GMT")

#Création du data.frame "temps"
date<- data.frame( date, c(1:length(date)))
names(date)<-c("date","indice")

date_2014<- data.frame( date_2014, c(1:length(date_2014)))
names(date_2014)<-c("date","indice")

date_2015<- data.frame( date_2015, c(1:length(date_2015)))
names(date_2015)<-c("date","indice")





###
# décomposition en autant de tableaux que de paramètres  --------
param <- c("conductivite","haut_eau","oxygene","haut_eau_abs","oxygene_pc","pH","redox","temp_eau","temp_air","turbidite")
n<-length(param)

#reserve
for (i in 1:n){
  eval(parse(text=paste0("reserve_2015_",param[i]," <- data.frame(reserve_2015$Date_",param[i],",reserve_2015$",param[i],")")))
  eval(parse(text=paste0("names(reserve_2015_",param[i],")<-c(\"date\",\"",param[i],"\")")))
  #eval(parse(text=paste0("reserve_2015_",param[i]," <-xts(reserve_2015_",param[i],"$",param[i],", order.by = reserve_2015_",param[i],"$date)")))
  #eval(parse(text=paste0("names(reserve_2015_",param[i],")<-c(\"",param[i],"\")")))
}

for (i in 1:n){
  eval(parse(text=paste0("reserve_2014_",param[i]," <- data.frame(reserve_2014$Date_",param[i],",reserve_2014$",param[i],")")))
  eval(parse(text=paste0("names(reserve_2014_",param[i],")<-c(\"date\",\"",param[i],"\")")))
  #eval(parse(text=paste0("reserve_2014_",param[i]," <-xts(reserve_2014_",param[i],"$",param[i],", order.by = reserve_2014_",param[i],"$date)")))
  #eval(parse(text=paste0("names(reserve_2014_",param[i],")<-c(\"",param[i],"\")")))
}

#thil
for (i in 1:n){
  eval(parse(text=paste0("thil_2015_",param[i]," <- data.frame(thil_2015$Date_",param[i],",thil_2015$",param[i],")")))
  eval(parse(text=paste0("names(thil_2015_",param[i],")<-c(\"date\",\"",param[i],"\")")))
  #eval(parse(text=paste0("thil_2015_",param[i]," <-xts(thil_2015_",param[i],"$",param[i],", order.by = thil_2015_",param[i],"$date)")))
  #eval(parse(text=paste0("names(thil_2015_",param[i],")<-c(\"",param[i],"\")")))
}

for (i in 1:n){
  eval(parse(text=paste0("thil_2014_",param[i]," <- data.frame(thil_2014$Date_",param[i],",thil_2014$",param[i],")")))
  eval(parse(text=paste0("names(thil_2014_",param[i],")<-c(\"date\",\"",param[i],"\")")))
  #eval(parse(text=paste0("thil_2014_",param[i]," <-xts(thil_2014_",param[i],"$",param[i],", order.by = thil_2014_",param[i],"$date)")))
  #eval(parse(text=paste0("names(thil_2014_",param[i],")<-c(\"",param[i],"\")")))
}

#  fusion de chaque sous-tableau avec la matrice de réference(pourles caler sur le mm pas de temps) 

##Thil
n<-length(param)
for (i in 1:n){
  eval(parse(text=paste0("thil_2014_",param[i]," <- merge(date_2014, thil_2014_",param[i],", by=\"date\", all.x=T)")))
  eval(parse(text=paste0("thil_2014_",param[i]," <- thil_2014_",param[i],"[!is.na(thil_2014_",param[i],"$",param[i],"),]")))
}

n<-length(param)
for (i in 1:n){
  eval(parse(text=paste0("thil_2015_",param[i]," <- merge(date_2015, thil_2015_",param[i],", by=\"date\", all.x=T)")))
  eval(parse(text=paste0("thil_2015_",param[i]," <- thil_2015_",param[i],"[!is.na(thil_2015_",param[i],"$",param[i],"),]")))
}


##reserve
n<-length(param)
for (i in 1:n){
  eval(parse(text=paste0("reserve_2014_",param[i]," <- merge(date_2014, reserve_2014_",param[i],", by=\"date\", all.x=T)")))
  eval(parse(text=paste0("reserve_2014_",param[i]," <- reserve_2014_",param[i],"[!is.na(reserve_2014_",param[i],"$",param[i],"),]")))
}

n<-length(param)
for (i in 1:n){
  eval(parse(text=paste0("reserve_2015_",param[i]," <- merge(date_2015, reserve_2015_",param[i],", by=\"date\", all.x=T)")))
  eval(parse(text=paste0("reserve_2015_",param[i]," <- reserve_2015_",param[i],"[!is.na(reserve_2015_",param[i],"$",param[i],"),]")))
}

### application de filtre de base
conductivite_max <- 2000
conductivite_min <- 0

oxygene_max <- 20
oxygene_min <- 0.1

oxygene_pc_max <-200
oxygene_pc_min <-1

haut_eau_min <- -500000
haut_eau_max <- 999999999999999

haut_eau_abs_min <- -500000
haut_eau_abs_max <- 999999999999999

turbidite_min <- 0
turbidite_max <- 1000


pH_max <- 9
pH_min <-5

redox_max <- 800
redox_min <- 1

temp_eau_max <- 30
temp_eau_min <- -20

temp_air_max <- 30
temp_air_min <- -20
  
n <-length(param)
for (i in 1:n){
  eval(parse(text=paste0("thil_2014_",param[i]," <-subset (thil_2014_",param[i],",thil_2014_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("thil_2014_",param[i]," <-subset (thil_2014_",param[i]," ,thil_2014_",param[i],"$",param[i]," > ",param[i],"_min)")))
  
}

for (i in 1:n){
  eval(parse(text=paste0("thil_2015_",param[i]," <-subset (thil_2015_",param[i],",thil_2015_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("thil_2015_",param[i]," <-subset (thil_2015_",param[i],",thil_2015_",param[i],"$",param[i]," > ",param[i],"_min)")))
}

for (i in 1:n){
  eval(parse(text=paste0("reserve_2015_",param[i]," <-subset (reserve_2015_",param[i],",reserve_2015_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("reserve_2015_",param[i]," <-subset (reserve_2015_",param[i]," ,reserve_2015_",param[i],"$",param[i]," > ",param[i],"_min)")))
}

for (i in 1:n){
  eval(parse(text=paste0("reserve_2014_",param[i]," <-subset (reserve_2014_",param[i],",reserve_2014_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("reserve_2014_",param[i]," <-subset (reserve_2014_",param[i]," ,reserve_2014_",param[i],"$",param[i]," > ",param[i],"_min)")))
}



###traitement spécial pour supprimmer eles valeur s0 de l'oxygene
reserve_2015_temp_eau  <-subset (reserve_2015_temp_eau,reserve_2015_temp_eau$temp_eau != 0)
reserve_2014_temp_eau  <-subset (reserve_2014_temp_eau,reserve_2014_temp_eau$temp_eau != 0)
thil_2014_temp_eau  <-subset (thil_2014_temp_eau,thil_2014_temp_eau$temp_eau != 0)
thil_2014_temp_eau  <-subset (thil_2014_temp_eau,thil_2014_temp_eau$temp_eau != 0)
reserve_2014_temp_air  <-subset (reserve_2014_temp_air ,reserve_2014_temp_air $temp_air != 0)
reserve_2015_temp_air  <-subset (reserve_2015_temp_air ,reserve_2015_temp_air $temp_air != 0)


##fusion de tous les paramètres en 1 taleau


##reserve
rm(reserve_2015_vf) # suppression d'ancienne version
reserve_2015_vf<- merge( subset(date_2015, select = - c(indice)), subset(reserve_2015_conductivite, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_pH, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_haut_eau, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_oxygene, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_oxygene_pc, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_redox, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_temp_air, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_temp_eau, select = - c(indice)), by="date", all.x=T)
reserve_2015_vf<- merge(reserve_2015_vf, subset(reserve_2015_turbidite, select = - c(indice)), by="date", all.x=T)




rm(reserve_2014_vf) # suppression d'ancienne version
reserve_2014_vf<- merge( subset(date_2014, select = - c(indice)), subset(reserve_2014_conductivite, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_pH, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_haut_eau, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_oxygene, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_oxygene_pc, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_redox, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_temp_air, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_temp_eau, select = - c(indice)), by="date", all.x=T)
reserve_2014_vf<- merge(reserve_2014_vf, subset(reserve_2014_turbidite, select = - c(indice)), by="date", all.x=T)


rm(thil_2015_vf) # suppression d'ancienne version
thil_2015_vf<- merge( subset(date_2015, select = - c(indice)), subset(thil_2015_conductivite, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_pH, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_haut_eau, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_oxygene, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_oxygene_pc, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_redox, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_temp_air, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_temp_eau, select = - c(indice)), by="date", all.x=T)
thil_2015_vf<- merge(thil_2015_vf, subset(thil_2015_turbidite, select = - c(indice)), by="date", all.x=T)

rm(thil_2014_vf) # suppression d'ancienne version
thil_2014_vf<- merge( subset(date_2014, select = - c(indice)), subset(thil_2014_conductivite, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_pH, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_haut_eau, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_oxygene, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_oxygene_pc, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_redox, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_temp_air, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_temp_eau, select = - c(indice)), by="date", all.x=T)
thil_2014_vf<- merge(thil_2014_vf, subset(thil_2014_turbidite, select = - c(indice)), by="date", all.x=T)

##agrégation des deux années si besoin
reserve_vf <- rbind(reserve_2014_vf,reserve_2015_vf)
thil_vf <- rbind(thil_2014_vf,thil_2015_vf)


thil_2014_xts <-xts(thil_2014_vf[,-c(1)], order.by =thil_2014_vf$date)
thil_2015_xts <-xts(thil_2015_vf[,-c(1)], order.by =thil_2015_vf$date)
reserve_2014_xts <-xts(thil_2014_vf[,-c(1)], order.by = reserve_2014_vf$date)
reserve_2015_xts <-xts(thil_2015_vf[,-c(1)], order.by = reserve_2015_vf$date)




###################################
######## Vision globale


#### synthèse métrologique


### vision globale des donnés acquises
############p-e découper le jeu de données avant le traitement pour plus de visibilité
#x11()
#summaryPlot(reserve_vf,
#            col.mis = "gray",
#            col.data ="darkolivegreen2",
#            col.hist = "black",
#            type = "density",
#            period = "years")

#x11()
#summaryPlot(thil_2015_vf,
#            col.mis = "gray",
#            col.data ="darkolivegreen2",
#            col.hist = "black",
#            type = "density",
#            period = "years")



#variaton de paramètres


### plus de sens si toutes les années sont aggrégées


## param physiques
 ####pas super pertinent

#x11()
#timeVariation(thil_vf, 
#              pollutant = c("temp_eau", "temp_air","haut_eau"),
#              normalise = F
#)


## pression phy
#x11()
#timeVariation(reserve_vf, 
#              pollutant = c("conductivite"),
#              normalise = F
#)


#x11()
#timeVariation(thil_vf, 
#              pollutant = c("conductivite"),
#              normalise = F
#)

## fonction bio
#x11()
#timeVariation(reserve_vf, 
#              pollutant = c("oxygene", "oxygene_pc","pH"),
#              normalise = F
#)


x11()
timeVariation(thil_vf, 
              pollutant = c("conductivite"),
              normalise = F
)


x11()
scatterPlot(reserve_vf, x = "temp_eau", y = "oxygene", method = "density", 
            col= "jet") 





x11()
scatterPlot(thil_vf, x = "temp_eau", y = "oxygene", z = "temp_air", 
            type = c("season"))


x11()
scatterPlot(test, x = "temp_eau", y = "oxygene", z = "temp_air", 
            type = c("season"))


test <-rbind(thil_vf,reserve_vf)


#calendrar plot
x11()
calendarPlot(reserve_2015_vf, 
             pollutant = c("oxygene"), 
             year = 2015, 
             cols = c("red", " orange", "blue"), 
             statistic="mean", ###par défaut : moyenne, mais possibilité de changer
             annotate="value",
             breaks = c(0, 3, 5, 100),###valeurs indicatives, à modifie/argmenter
             labels = c("Médiocre", "Moyen","Bon")
) 



x11()
trendLevel(reserve_vf, y = "hour", pollutant = "oxygene")
x11()
trendLevel(thil_vf, y = "hour", pollutant = "oxygene")





trendLevel(thil_vf, x = "season", y = "hour", pollutant = "oxygene",
           cols = "increment")

