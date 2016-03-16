##### Edition rapport annuel 2015#####
######################################


## section Synthèse evenementielle####
######################################

#vider la mémoire des travaux précédents
rm(list=ls())

## définition de l'opération "ni" (l'inverse de "in")
`%ni%` <- Negate(`%in%`)

### chargement packages
ma_liste_de_packages <- c("ggplot2", ## pour faire des jolis graphs
        "cowplot", ## pour  afficher plusieurs ggplot
        "fortunes", 
        "lattice", 
        "ggthemes", 
        "openxlsx", ## pour ouvrir directement les fichier excel
        "plotly", ## pour des graph interactifs
        "xts", 
        "openair",
        "Cairo" , ## pour exporter correctement les graphs
        "reshape2" ) 

lapply(ma_liste_de_packages, require, character.only = TRUE) ## require sur l'ensemble des packages



#theme_set(theme_gray())

my.theme = theme_grey() + theme(text=element_text(family='Roboto', size=30))

fortune(15)

#####################


col_thil    <-"#208CFF"
col_reserve <- "#0DB219"



##### import des mesures

setwd("C:/Users/XZ4062/Desktop/LyRE/Monitoring/Sirenes/2_BDX_M/Valorisation des donnees/1_données brutes/Rapport annuel 2015/evenement")
getwd()

## lecture des fichiers excel(packages openxlsx)
## oxygene
#2014
thil_2014_event_ox_full<- read.xlsx("Outil_oxygene_thil_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                    rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_2014_event_ox_full<- read.xlsx("Outil_oxygene_reserve_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                       rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                       rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)


thil_2015_event_ox_full<- read.xlsx("Outil_oxygene_thil_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                    rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_2015_event_ox_full<- read.xlsx("Outil_oxygene_reserve_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                    rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
## conductivite
thil_2014_event_cond_full<- read.xlsx("Outil_conductivite_thil_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                    rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                    rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

thil_2015_event_cond_full<- read.xlsx("Outil_conductivite_thil_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_2014_event_cond_full<- read.xlsx("Outil_conductivite_reserve_2014.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_2015_event_cond_full<- read.xlsx("Outil_conductivite_reserve_2015.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                      rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                                      rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

##formatage des données

sites <- c("thil","reserve")
annees <- c(2014,2015)


## format date

n <- length(sites)
m <- length(annees)
for (i in 1:n){
  for (j in 1:m){
   eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_ox_full$date <- convertToDateTime(",sites[i],"_",annees[j],"_event_ox_full$date)"))) ###??? fonction de openxlsx
   eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_ox_full$date <- round(",sites[i],"_",annees[j],"_event_ox_full$date,\"mins\")"))) ## besoin d'arrondir les date à la minute pres, car pb d'arrondi avec la conversion précédente
   
   eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_cond_full$date <- convertToDateTime(",sites[i],"_",annees[j],"_event_cond_full$date)"))) ###??? fonction de openxlsx
   eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_cond_full$date <- round(",sites[i],"_",annees[j],"_event_cond_full$date,\"mins\")"))) ## besoin d'arrondir les date à la minute pres, car pb d'arrondi avec la conversion précédente
         }
}

## format num # pour les paramètres qui ne sont pas chargés correctement
n <- length(sites)
for (i in 1:n){
  eval(parse(text=paste0("",sites[i],"_2015_event_ox_full$filtre <- as.numeric(",sites[i],"_2015_event_ox_full$filtre)"))) 
  eval(parse(text=paste0("",sites[i],"_2014_event_ox_full$Signale_niveau <- as.numeric(",sites[i],"_2014_event_ox_full$Signale_niveau)"))) 
}


## allegement de la table à manipuler (bcp de colonne inutiles) 

#???oxygene
n <- length(sites)
m <- length(annees)
for (i in 1:n){
  for (j in 1:m){
  eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_ox <- ",sites[i],"_",annees[j],"_event_ox_full[,c(1,4, ## données de base
                                                   13,14,15, ## signaux intensité
                                                   19,20,21)]")))
  eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_ox[,5] <- as.factor(",sites[i],"_",annees[j],"_event_ox[,5])"))) 
  eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_ox[,8] <- as.factor(",sites[i],"_",annees[j],"_event_ox[,8])"))) 
  }
}

##conductivité
for (i in 1:n){
  for (j in 1:m){
    eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_cond <- ",sites[i],"_",annees[j],"_event_cond_full[,c(1,4, ## données de base
                           10,11,12, ## signaux 
                           13,14,15)]")))
    eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_cond[,7] <- as.factor(",sites[i],"_",annees[j],"_event_cond[,7])"))) 
    eval(parse(text=paste0("",sites[i],"_",annees[j],"_event_cond[,8] <- as.factor(",sites[i],"_",annees[j],"_event_cond[,8])"))) 
  }
}


################# import des données d'évenement et création des layers ggplot associés

## Import des données  d'évenement
thil_event<- read.xlsx("evenement.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                            rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                            rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

reserve_event<- read.xlsx("evenement.xlsx", sheet = 2, startRow = 1, colNames = TRUE, 
                            rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                            rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

## conversion des information de début et fin d'évenement au bon format date
n <- length(sites)
for (i in 1:n){
    eval(parse(text=paste0("",sites[i],"_event[,2] <-as.POSIXct( round(convertToDateTime(",sites[i],"_event[,2]),\"mins\"),format=\"%d/%m/%Y %H:%M\",tz=\"GMT\")")))
    eval(parse(text=paste0("",sites[i],"_event[,3] <-as.POSIXct( round(convertToDateTime(",sites[i],"_event[,3]),\"mins\"),format=\"%d/%m/%Y %H:%M\",tz=\"GMT\")")))
}

## création de la charte visuelle pour matérialiser les évenement (création d'instance layer)
#### compliqué d'optimiser car besoin d'appeller le nom du site 
n <- length(thil_event$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_thil",i," <- thil_event[",i,",2]")))
  eval(parse(text=paste0("b_thil",i," <- thil_event[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_thil",i,", 
                         xmax = b_thil",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'dodger blue', alpha = 0.2, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_thil",i,sep="_"),data,.GlobalEnv)
}
liste_temp_thil <- c()
liste_temp_thil <- paste0("G_thil_",paste0(1:n))
viz_event_thil  <- c()
viz_event_thil  <- paste(liste_temp_thil,collapse=" + ")



###
n <- length(reserve_event$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_reserve",i," <- reserve_event[",i,",2]")))
  eval(parse(text=paste0("b_reserve",i," <- reserve_event[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_reserve",i,", 
                         xmax = b_reserve",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'dodger blue', alpha = 0.2, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_reserve",i,sep="_"),data,.GlobalEnv)
}

liste_temp_reserve <- c()
liste_temp_reserve <- paste0("G_reserve_",paste0(1:n))
viz_event_reserve  <- c()
viz_event_reserve  <- paste(liste_temp_reserve,collapse=" + ")



###########création des fonction pour géréner les graphiques



###  cadrage temporel (on veut un graph par trimestre) ### a améliorer mais je ne vois pas trop comment
debut_T1_2015 <- as.POSIXct("01/01/2015 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T1_2015   <- as.POSIXct("31/03/2015 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T2_2015 <- as.POSIXct("01/04/2015 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T2_2015   <- as.POSIXct("30/06/2015 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T3_2015 <- as.POSIXct("01/07/2015 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T3_2015   <- as.POSIXct("30/09/2015 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T4_2015 <- as.POSIXct("01/10/2015 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T4_2015   <- as.POSIXct("31/12/2015 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

daterange_2015<- list()
daterange_2015<- list(T1=c(debut_T1_2015,fin_T1_2015),
                  T2=c(debut_T2_2015,fin_T2_2015),
                  T3=c(debut_T3_2015,fin_T3_2015),
                  T4=c(debut_T4_2015,fin_T4_2015)
                  )

debut_T1_2014 <- as.POSIXct("01/01/2014 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T1_2014   <- as.POSIXct("31/03/2014 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T2_2014 <- as.POSIXct("01/04/2014 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T2_2014   <- as.POSIXct("30/06/2014 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T3_2014 <- as.POSIXct("01/07/2014 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T3_2014   <- as.POSIXct("30/09/2014 23:45", format="%d/%m/%Y %H:%M",tz="GMT")

debut_T4_2014 <- as.POSIXct("01/10/2014 00:00", format="%d/%m/%Y %H:%M",tz="GMT")
fin_T4_2014   <- as.POSIXct("31/12/2014 23:45", format="%d/%m/%Y %H:%M",tz="GMT")



daterange_2014<- list()
daterange_2014<- list(T1=c(debut_T1_2014,fin_T1_2014),
                      T2=c(debut_T2_2014,fin_T2_2014),
                      T3=c(debut_T3_2014,fin_T3_2014),
                      T4=c(debut_T4_2014,fin_T4_2014)
)



#################### confrontation avec les données d'évenement idendfiées dans les rapports


####### oxygene (création des graphs)

## fonction graph Amplitude ##??? avec intensité de valeur
graph_T_amp <- function(site,annee,param){
    n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                                      aes(x = date, y = filtre),
                                      colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,",
                                      aes(x = date, y =Signale_amplitude,colour=Intensite_signal_amplitude),size=3)+
                           scale_color_gradient(name =\"baisse de\nl'amplitude journaliere\",low=\"red\", high=\"green\") +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
                           ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
}


## fonction graph Amplitude ##??? avec classe de valeur
graph_T_amp <- function(site,annee,param){
  n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                           aes(x = date, y = filtre),
                           colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,",
                           aes(x = date, y =Signale_amplitude,colour=classes_alerte_amplitude),size=3)+
                           scale_colour_manual(name =\"baisse de\nl'amplitude journaliere\", values = c(\"orange\",\"red\")) +
                           scale_alpha_manual(values=c(0.1, 0.5,),guide=F) +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
                           ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
  }



## exemple
graph_T_amp("reserve", "2015", "ox")
graph_T_amp("thil", "2015", "ox")


## fonction graph niveau Max ## graph avec intensité de valeur
graph_T_max <- function(site,annee,param){
  n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                                      aes(x = date, y = filtre),
                                      colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,",
                                      aes(x = date, y =Signale_niveau,colour=Intensite_signal_niveau),
                                      size=3)+
                           scale_color_gradient(name =\"baisse du\nmaximum journalier\", low=\"red\", high=\"green\") +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
    ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
}


########### ## graph avec classes de valeur
graph_T_max <- function(site,annee,param){
  n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                           aes(x = date, y = filtre),
                           colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,",
                           aes(x = date, y =Signale_niveau,colour=classes_alerte_niveau),
                           size=3)+
                           scale_colour_manual(name =\"baisse du\nmaximum journalier\", values = c(\"orange\",\"red\")) +
                           scale_alpha_manual(values=c(0.1, 0.5,),guide=F) +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
                           ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
  }


###########


## exemple
graph_T_max("thil", "2015", "ox")
graph_T_max("reserve", "2015", "ox")




## fonction graph conductivité (création des graphs)
#######.
graph_T_cond <- function(site,annee,param){
  n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                                      aes(x = date, y = non_signalees),
                                      colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,", aes(x = date, y =signal_precoce), size=2)+
                           geom_point(data = ",site,"_",annee,"_event_",param,", aes(x = date, y =signal_avere,colour=intensite_du_signal), size=3)+
                           scale_color_gradient(name =\"baisse de la\nconductivité\",low=\"green\", high=\"red\") +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
                           ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
  }

###??? 
#######. graph par classe
graph_T_cond <- function(site,annee,param){
  n <- 4 ## nombr de trimestre dans chaque année
  x11()
  for (i in 1:n){
    event <- eval(parse(text=paste0("viz_event_",site,"")))
    eval(parse(text=paste0("p_T",i," <- ggplot() + 
                           geom_line (data = ",site,"_",annee,"_event_",param,", 
                           aes(x = date, y = non_signalees),
                           colour=col_",site,",size = 1, alpha =0.5) +
                           ",event," +
                           geom_point(data = ",site,"_",annee,"_event_",param,", aes(x = date, y =signal_precoce), size=2)+
                           geom_point(data = ",site,"_",annee,"_event_",param,", aes(x = date, y =signal_avere,colour=classes_alerte_precoce), size=3)+
                           scale_colour_manual(name =\"baisse de la\nconductivité\", values = c(\"yellow\",\"orange\",\"red\")) +
                           scale_alpha_manual(values=c(0.1, 0.5,),guide=F) +
                           theme(axis.title.x = element_blank()) + 
                           ylab(\"\")  +
                           xlim(daterange_",annee,"$T",i,") ")
                           ))
  }
  liste_temp <- paste0("p_T",paste0(1:n))
  liste_graph <-paste(liste_temp,collapse=",")
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
  }

## exemple
graph_T_cond("thil", "2015", "cond")
graph_T_cond("reserve", "2014", "cond")
graph_T_cond("thil", "2014", "cond")








