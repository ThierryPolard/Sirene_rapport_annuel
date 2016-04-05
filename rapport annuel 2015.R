##### Edition rapport annuel 2015#####
######################################
######################################

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

########


col_thil    <-"#208CFF"
col_reserve <- "#0DB219"
col_cantinolle <-"#048B9A"


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

cantinolle_2015<- read.xlsx("cantinolle_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                         rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                         rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)




### import des données externes (pluviométrie et déversements)
pluieEtDeversements<- read.xlsx("pluieEtDeversements.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                         rowNames = FALSE, detectDates = T, skipEmptyRows = FALSE,
                         rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

#bricolga foireux pour avoir mes dates au bon format... a corriger
#Création du vecteur "temps"
debut <- as.POSIXct(strptime("01/01/2014 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin <- as.POSIXct(strptime("30/12/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
date<-seq(debut,fin,by=24*60*60)

pluieEtDeversements <-cbind(date,pluieEtDeversements[-1])



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

n<-ncol(cantinolle_2015)
seq1<-seq(from = 1, to = n, by=2)
for (i in  seq1)
  ifelse(is.na(cantinolle_2015[[i]]),print("NA"),
         cantinolle_2015[[i]] <- as.POSIXct(cantinolle_2015[[i]], format="%d/%m/%Y %H:%M",tz="GMT"))



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


#cantinolle 

for (i in 1:n){
  eval(parse(text=paste0("cantinolle_2015_",param[i]," <- data.frame(cantinolle_2015$Date_",param[i],",cantinolle_2015$",param[i],")")))
  eval(parse(text=paste0("names(cantinolle_2015_",param[i],")<-c(\"date\",\"",param[i],"\")")))
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


## cantinolle
n<-length(param)
for (i in 1:n){
  eval(parse(text=paste0("cantinolle_2015_",param[i]," <- merge(date_2015, cantinolle_2015_",param[i],", by=\"date\", all.x=T)")))
  eval(parse(text=paste0("cantinolle_2015_",param[i]," <- cantinolle_2015_",param[i],"[!is.na(cantinolle_2015_",param[i],"$",param[i],"),]")))
}


### application de filtre de base
conductivite_max <- 2000
conductivite_min <- 10

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
  eval(parse(text=paste0("reserve_2015_",param[i]," <-subset (reserve_2015_",param[i],",reserve_2015_",param[i],"$",param[i]," > ",param[i],"_min)")))
}

for (i in 1:n){
  eval(parse(text=paste0("reserve_2014_",param[i]," <-subset (reserve_2014_",param[i],",reserve_2014_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("reserve_2014_",param[i]," <-subset (reserve_2014_",param[i],",reserve_2014_",param[i],"$",param[i]," > ",param[i],"_min)")))
}


for (i in 1:n){
  eval(parse(text=paste0("cantinolle_2015_",param[i]," <-subset (cantinolle_2015_",param[i],",cantinolle_2015_",param[i],"$",param[i]," < ",param[i],"_max)")))
  eval(parse(text=paste0("cantinolle_2015_",param[i]," <-subset (cantinolle_2015_",param[i],",cantinolle_2015_",param[i],"$",param[i]," > ",param[i],"_min)")))
}




###traitement spécial pour supprimmer eles valeur s0
reserve_2015_temp_eau  <-subset (reserve_2015_temp_eau,reserve_2015_temp_eau$temp_eau != 0)
reserve_2014_temp_eau  <-subset (reserve_2014_temp_eau,reserve_2014_temp_eau$temp_eau != 0)
thil_2014_temp_eau  <-subset (thil_2014_temp_eau,thil_2014_temp_eau$temp_eau != 0)
thil_2014_temp_eau  <-subset (thil_2014_temp_eau,thil_2014_temp_eau$temp_eau != 0)
reserve_2014_temp_air  <-subset (reserve_2014_temp_air ,reserve_2014_temp_air$temp_air != 0)
reserve_2015_temp_air  <-subset (reserve_2015_temp_air ,reserve_2015_temp_air$temp_air != 0)
cantinolle_2015_temp_air  <-subset (cantinolle_2015_temp_air, cantinolle_2015_temp_air$temp_air != 0)
cantinolle_2015_temp_eau  <-subset (cantinolle_2015_temp_eau, cantinolle_2015_temp_eau$temp_eau != 0)
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


rm(cantinolle_2015_vf) # suppression d'ancienne version
cantinolle_2015_vf<- merge( subset(date_2015, select = - c(indice)), subset(cantinolle_2015_conductivite, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_pH, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_haut_eau, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_oxygene, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_oxygene_pc, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_redox, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_temp_air, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_temp_eau, select = - c(indice)), by="date", all.x=T)
cantinolle_2015_vf<- merge(cantinolle_2015_vf, subset(cantinolle_2015_turbidite, select = - c(indice)), by="date", all.x=T)


##agrégation des deux années si besoin
reserve_vf <- rbind(reserve_2014_vf,reserve_2015_vf)
thil_vf <- rbind(thil_2014_vf,thil_2015_vf)
cantinolle_vf <- rbind(cantinolle_2015_vf)


thil_2014_xts <-xts(thil_2014_vf[,-c(1)], order.by =thil_2014_vf$date)
thil_2015_xts <-xts(thil_2015_vf[,-c(1)], order.by =thil_2015_vf$date)

reserve_2014_xts <-xts(thil_2014_vf[,-c(1)], order.by = reserve_2014_vf$date)
reserve_2015_xts <-xts(thil_2015_vf[,-c(1)], order.by = reserve_2015_vf$date)

cantinolle_2015_xts <-xts(thil_2015_vf[,-c(1)], order.by = cantinolle_2015_vf$date)


#
#######################################################################################################################
################################################## synthèse hydrologique###############################################
    ### plus de sens si toutes les années sont aggrégées

## représentation de la pluvio sur la période considéré (calage sur le calendrier des sirene)

## choix de la période de représention 

# tout ce qui est disponibles
n <- length(thil_vf$date)
debut <-thil_vf$date[1]
fin <-thil_vf$date[n]

# fenetre temporelle définie
debut <- as.POSIXct(strptime("01/08/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin   <- as.POSIXct(strptime("01/12/2015 23:45:00",  format="%d/%m/%Y %H:%M",tz="GMT"))

lims  <- c(debut, fin)


P_pluie<- ggplot() +
  geom_bar(data=pluieEtDeversements, aes(date, Pluie), stat="identity", colour = "blue", fill="blue")+ 
  xlab(NULL) + ylab("Pluviométrie")+
  scale_y_reverse() +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b%y"), limits=lims) +
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         axis.text.x=element_blank(),
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank())

## représentation des déversement aux DO (empilé)
##modification du format du tableaui
meltdf <- melt(pluieEtDeversements[-c(2,3,4,11, 12,13, 15, 16)],id="date")
P_dev <- ggplot(data = meltdf, aes(x = date, y = value, colour=variable, fill = variable)) + 
  geom_bar(stat='identity')+ 
  xlab(NULL) + ylab("Déversements")+
  scale_x_datetime(  breaks=date_breaks("1 month"),labels=date_format("%b%y"),limits=lims) +
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         legend.title=element_blank(), 
         legend.position = c(.95, .6),
         axis.text.x=element_blank(),
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank())


## représentation des déversement à la STEP (by-pass + STEP)
meltdf <- melt(pluieEtDeversements[c(1, 15)],id="date")
#meltdf <- melt(pluieEtDeversements[c(1, 15, 16)],id="date")    # si on veut représenter le rejet de la STEP aussi

P_STEP <- ggplot(data = meltdf, aes(x = date, y = value, colour=variable, fill = variable)) + 
  geom_bar(stat='identity')+ 
  xlab(NULL) + ylab("By-pass STEP")+
  scale_x_datetime(breaks=date_breaks("1 month"),labels=date_format("%b%y"),limits=lims) +
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         legend.title=element_blank(), 
         legend.position = c(.9, .4),
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank(),
         axis.text.x = element_text(size=14))

#x11()
#plot_grid(P_pluie, 
#          P_dev,
#          P_STEP,
#          nrow=3, align = "v")


## param physiques

## conductivité
##timePlot(thil_vf, pollutant = "conductivite") ##moche

thil_cond<- ggplot(thil_vf,aes(x=date, y=conductivite)) +
               geom_point(colour ="#208CFF",size = 1, alpha=0.5) +
               ylim(0, 750)+  
               xlab(NULL) + ylab("Thil")+
               scale_x_datetime(breaks=date_breaks("1 month"),labels=date_format("%b%y"),limits=lims) +
               theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                     panel.grid.major.y = element_blank(),
                     axis.text.x=element_blank())

cantinolle_cond<- ggplot(cantinolle_vf, aes(x=date, y=conductivite)) +
                    geom_point(colour ="#048B9A",    size = 1, alpha =0.5)       +
                    ylim(200, 750)+  
                    xlab(NULL) + ylab("Cantinolle")+
                    scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b%y"),limits=lims) +
                    theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                          panel.grid.major.y = element_blank(),
                          axis.title.x = element_blank(),
                          axis.text.x=element_blank())

reserve_cond<- ggplot(reserve_vf, aes(x=date, y=conductivite)) +
                 geom_point(colour ="#0DB219", size = 1, alpha =0.5)       +
                 ylim(0, 750)+  
                 xlab(NULL) + ylab("Reserve")+
                 scale_x_datetime(breaks=date_breaks("1 month"),labels=date_format("%b%y"),limits=lims) +
                 theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                       panel.grid.major.y = element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size=14))
x11() 
plot_grid(P_pluie, 
          P_dev,
          P_STEP,
          thil_cond,
          cantinolle_cond,
          reserve_cond,
          nrow=6, align = "v")


x11()
smoothTrend(thil_vf, ### pas le plus pertinent pour ce paramètre (on veut montrer des variation brutales)
            deseason =TRUE,
            pollutant = "conductivite",
            ylab = "conductivité",
            xlab = "",
            auto.text = F,
            par.settings=list(fontsize=list(text=16)),
            cols = "#208CFF",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")

x11()
smoothTrend(cantinolle_vf, ### pas le plus pertinent pour ce paramètre (on veut montrer des variation brutales)
            deseason =TRUE,
            pollutant = "conductivite",
            ylab = "conductivité",
            xlab = "",
            cols = "#048B9A",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            shade = "white", 
            date.breaks = 10,
            main = "")


x11()
smoothTrend(reserve_vf, ### pas le plus pertinent pour ce paramètre (on veut montrer des variation brutales)
            deseason =TRUE,
            pollutant = "conductivite",
            ylab = "conductivité",
            xlab = "",
            auto.text = F,
            par.settings=list(fontsize=list(text=16)),
            cols = "#0DB219",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")


x11()
timeVariation(thil_vf, 
              pollutant = c("conductivite"),
              cols =  "#208CFF",
              normalise = F)

x11()
timeVariation(cantinolle_vf, 
              pollutant = c("conductivite"),
              cols =  "#048B9A",
              normalise = F)

x11()
timeVariation(reserve_vf, 
              pollutant = c("conductivite"),
              cols =  "#0DB219",
              normalise = F)




#plot_grid(a,b, nrow=2, align = "v")


### turbidité


thil_turbi <- ggplot(thil_vf, aes(x=date, y=turbidite)) +
                geom_point(colour ="#208CFF", size = 1, alpha= 0.5)   +
                ylim(0, 500)+
                xlab(NULL) + ylab("Thil")+
                scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                      panel.grid.major.y = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.x=element_blank())


cantinolle_turbi <-ggplot(cantinolle_vf, aes(x=date, y=turbidite)) +
                     geom_point(colour ="#048B9A", size = 1, alpha= 0.5)  +
                     ylim(0, 500)+  
                     xlab(NULL) + ylab("Cantinolle")+
                     scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                     theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                           panel.grid.major.y = element_blank(),
                           axis.title.x = element_blank(),
                           axis.text.x=element_blank())

reserve_turbi <-ggplot(reserve_vf, aes(x=date, y=turbidite)) +
                geom_point(colour ="#0DB219", size = 1, alpha= 0.5)  +
                ylim(0, 500)+
                xlab(NULL) + ylab("Reserve")+
                scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                      panel.grid.major.y = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.x = element_text(size=14))


x11()
plot_grid(P_pluie, 
          P_dev,
          P_STEP,
          thil_turbi,
          cantinolle_turbi,
          reserve_turbi,
          nrow=6, align = "v")

##smoothtrend et time variation n'ont pas bcp de sens pour la turbitdité
x11()
timeVariation(thil_vf, 
              pollutant = c("turbidite"),
              cols =  "#208CFF",
              normalise = F)

x11()
timeVariation(cantinolle_vf, 
              pollutant = c("turbidite"),
              cols =  "#048B9A",
              normalise = F)

x11()
timeVariation(reserve_vf, 
              pollutant = c("turbidite"),
              cols =  "#0DB219",
              normalise = F)




## potentiel rédox


thil_redox <- ggplot(thil_vf, aes(x=date, y=redox)) +
                geom_point(colour ="#208CFF", size = 1, alpha=0.5)  +
                ylim(0, 600)+  
                xlab(NULL) + ylab("Thil")+
                scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                      panel.grid.major.y = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.x=element_blank())

cantinolle_redox <- ggplot(cantinolle_vf, aes(x=date, y=redox)) +
                      geom_point(colour ="#048B9A",size = 1, alpha=0.5)   +
                      ylim(0, 600)+
                      xlab(NULL) + ylab("Cantinolle")+
                      scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                      theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                            panel.grid.major.y = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x=element_blank())

reserve_redox <- ggplot(reserve_vf, aes(x=date, y=redox)) +
                  geom_point(colour ="#0DB219",size = 1, alpha=0.5)   +
                  ylim(0, 600)+ 
                  xlab(NULL) + ylab("Reserve")+
                  scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                  theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                        panel.grid.major.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(size=14))
 
x11()
plot_grid(P_pluie, 
          P_dev,
          P_STEP,
          thil_redox,
          cantinolle_redox, 
          reserve_redox,
          nrow=6, align = "v")

x11()
smoothTrend(thil_vf, 
            deseason =TRUE,
            pollutant = "redox",
            ylab = "redox",
            xlab = "",
            auto.text = F,
            par.settings=list(fontsize=list(text=16)),
            cols = "#208CFF",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")

x11()
smoothTrend(reserve_vf, 
            deseason =TRUE,
            pollutant = "redox",
            ylab = "redox",
            xlab = "",
            auto.text = F,
            par.settings=list(fontsize=list(text=16)),
            cols = "#0DB219",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")


### pH
##timePlot(thil_vf, pollutant = "turbidite") ##moche

thil_pH <- ggplot(thil_vf, aes(x=date, y=pH)) +
            geom_point(colour ="#208CFF",alpha = 0.5, size = 1)      +
            ylim(6.5,9)+ 
            xlab(NULL) + ylab("Thil")+
            scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
            theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                  panel.grid.major.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x=element_blank())

cantinolle_pH <- ggplot(cantinolle_vf, aes(x=date, y=pH)) +
                  geom_point(colour ="#048B9A",alpha = 0.5, size = 1)      +
                  ylim(6.5,8)+ 
                  xlab(NULL) + ylab("Cantinolle")+
                  scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                  theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                        panel.grid.major.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x=element_blank())  

reserve_pH <- ggplot(reserve_vf, aes(x=date, y=pH)) +
                  geom_point(colour ="#0DB219",alpha = 0.5, size = 1)      +  
                  ylim(6,8)+  
                  xlab(NULL) + ylab("Reserve")+
                  scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                  theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                        panel.grid.major.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(size=14))


x11()
plot_grid(P_pluie, 
          P_dev,
          P_STEP,
          thil_pH,
          cantinolle_pH,
          reserve_pH,
          nrow=6, align = "v")


x11()
smoothTrend(thil_vf, 
            deseason =TRUE,
            pollutant = "pH",
            ylab = "pH",
            xlab = "",
            cols = "#208CFF",
            par.settings=list(fontsize=list(text=16)),
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")
x11()
smoothTrend(reserve_vf, 
            deseason =TRUE,
            pollutant = "pH",
            ylab = "pH",
            xlab = "",
            par.settings=list(fontsize=list(text=16)),
            cols = "#0DB219",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")



x11()
timeVariation(thil_vf, 
              pollutant = c("pH"),
              cols =  "#208CFF",
              normalise = F)

x11()
timeVariation(cantinolle_vf, 
              pollutant = c("pH"),
              cols =  "#048B9A",
              normalise = F)

x11()
timeVariation(reserve_vf, 
              pollutant = c("pH"),
              cols =  "#0DB219",
              normalise = F)


#### oxygene


thil_oxy <-ggplot(thil_vf, aes(x=date, y=oxygene)) +
            geom_point(colour ="#208CFF",alpha = 0.5, size = 1)    +
            scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
            xlab(NULL) + ylab("Thil")+
            theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                  panel.grid.major.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x=element_blank())

cantinolle_oxy <-ggplot(cantinolle_vf, aes(x=date, y=oxygene)) +
                  geom_point(colour ="#048B9A",alpha = 0.5, size = 1)    +
                  scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                  xlab(NULL) + ylab("Cantinolle")+
                  theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                        panel.grid.major.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x=element_blank())

reserve_oxy <-ggplot(reserve_vf,  aes(x=date, y=oxygene)) +
                  geom_point(colour ="#0DB219",alpha = 0.5, size = 1)    +
                  scale_x_datetime( breaks=date_breaks("1 month"),labels=date_format("%b %y"),limits=lims) +
                  xlab(NULL) + ylab("Reserve")+
                  theme(panel.grid.major = element_line(color = "gray91", size = 0.4),
                        panel.grid.major.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.x = element_text(size=14))

x11()
plot_grid(P_pluie, 
          P_dev,
          P_STEP,
          thil_oxy,
          cantinolle_oxy ,
          reserve_oxy,
          nrow=6, align = "v")



x11()
smoothTrend(thil_vf, 
            deseason =TRUE,
            pollutant = "oxygene",
            ylab = "oxygene",
            xlab = "",
            par.settings=list(fontsize=list(text=16)),
            cols = "#208CFF",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")


x11()
smoothTrend(reserve_vf, 
            deseason =TRUE,
            pollutant = "oxygene",
            ylab = "oxygene",
            xlab = "",
            par.settings=list(fontsize=list(text=16)),
            cols = "#0DB219",
            ref.y = list(h = c(180, 120,60), lty = c(1,1,1), col = c("blue","green" ,"yellow")),
            ci = TRUE,
            date.breaks = 20,
            main = "")

x11()
timeVariation(thil_vf, 
              pollutant = c("oxygene"),
              cols =  "#208CFF",
              normalise = F)

x11()
timeVariation(cantinolle_vf, 
              pollutant = c("oxygene"),
              cols =  "#048B9A",
              normalise = F)


x11()
timeVariation(reserve_vf, 
              pollutant = c("oxygene"),
              cols =  "#0DB219",
              normalise = F)


x11()
timeVariation(thil_vf, 
              pollutant = c("oxygene", "pH"),
              normalise = T
)


x11()
timeVariation(cantinolle_vf, 
              pollutant = c("oxygene", "pH"),
              normalise = T
)


x11()
timeVariation(reserve_vf, 
              pollutant = c("oxygene", "pH"),
              normalise = T
)



x11()
scatterPlot(thil_2014_vf, x = "temp_eau", y = "oxygene", z = "temp_air", 
            type = c("season"))

x11()
scatterPlot(reserve_vf, x = "temp_eau", y = "oxygene", z = "temp_air", 
            type = c("season"))




palette1 <- openColours(c("#FFDAB9", #peachpuff     8
                         "#B0E0E6", #powderblue    9 
                         "#87CEFF", # skyblue1     10
                         ), 20)


x11()
trendLevel(thil_vf, y = "hour", pollutant = "oxygene",
           par.settings=list(fontsize=list(text=18)),
           cols = palette1 )

palette2 <- openColours(c("#CD3700", #orangered3    4
                          "#FFDAB9", #peachpuff     8
                          "#B0E0E6", #powderblue    9 
                          "#87CEFF", # skyblue1     10
                          "#436EEE"  # royalblue2  >12
), 20)
x11()
trendLevel(reserve_vf, y = "hour", pollutant = "oxygene",
           par.settings=list(fontsize=list(text=18)),
           cols = palette2 )



#calendrar plot


xx <- timeAverage(thil_2015_vf, avg.time = "day", data.thresh = 0, statistic = "percentile",
            type = "default", percentile = 90, start.date = NA, end.date = NA,
            interval = NA, vector.ws = FALSE, fill = FALSE)

x11()
calendarPlot(xx, 
             pollutant = c("oxygene"), 
             year = 2015, 
             cols = c("red", "orange", "yellow", "green","blue"), 
             statistic="max",
             annotate="date",
             par.settings=list(fontsize=list(text=18)),
             breaks = c(0, 3, 4, 6, 8,100),###valeurs indicatives, à modifie/argmenter
             labels = c("Mauvais", "Médiocre", "Moyen","Bon","Très bon"),
             key.position  = "bottom"
) 


x11()
calendarPlot(xx, 
             pollutant = c("oxygene"), 
             year = 2015, 
             cols = c("red", "orange", "yellow", "green","blue"), 
             statistic="min",
             annotate="date",
             breaks = c(0, 3, 4, 6, 8,100),###valeurs indicatives, à modifie/argmenter
             labels = c("Mauvais", "Médiocre", "Moyen","Bon","Très bon")
) 

calendarPlot(reserve_2015_vf, 
             pollutant = c("oxygene"), 
             year = 2015, month = 1:12,
             type = "default", annotate = "value", statistic = "percentile",
             cols = "heat", lim = NULL, 
             col.lim = c("grey30", "black"), font.lim = c(1, 2), 
             cex.lim = c(0.6, 1), digits = 0,
             data.thresh = 0, labels = NA, breaks = NA, 
             key.header = "", key.footer = "", key.position = "right",
             key = TRUE, auto.text = TRUE)




timeAverage(reserve_2015_vf, avg.time = "day", data.thresh = 0, statistic = "percentile",
            type = "default", percentile = 90, start.date = NA, end.date = NA,
            interval = NA, vector.ws = FALSE, fill = FALSE)
