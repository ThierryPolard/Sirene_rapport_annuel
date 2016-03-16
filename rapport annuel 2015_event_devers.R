

#################### confrontation avec les données hydrologiques (pluie et déversements)
require("scales")

## représentation de la pluvio sur la période considéré (calage sur le calendrier des sirene)
#Création du vecteur "temps"
debut <- as.POSIXct(strptime("01/01/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin   <- as.POSIXct(strptime("31/12/2015 23:45:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
lims  <- c(debut, fin)




### import des données externes (pluviométrie et déversements)
setwd("C:/Users/XZ4062/Desktop/LyRE/Monitoring/Sirenes/2_BDX_M/Valorisation des donnees/1_données brutes/Rapport annuel 2015")
getwd()

pluieEtDeversements<- read.xlsx("pluieEtDeversements.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                                rowNames = FALSE, detectDates = T, skipEmptyRows = FALSE,
                                rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

#bricolga foireux pour avoir mes dates au bon format... a corriger
#Création du vecteur "temps"
debut <- as.POSIXct(strptime("01/01/2014 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin <- as.POSIXct(strptime("30/12/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
date<-seq(debut,fin,by=24*60*60)

pluieEtDeversements <-cbind(date,pluieEtDeversements[-1])



P_pluie<- ggplot() +
  geom_bar(data=pluieEtDeversements, aes(date, pluie_bruges), stat="identity", colour = "blue", fill="blue") +
  xlab(NULL) + ylab("")    +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         axis.text.x=element_blank(),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank())+
  scale_y_reverse()

##??? représentation des déversements aux DO
meltdf <- melt(pluieEtDeversements[-c(2,3,4,11, 12,13, 15, 16)],id="date")

P_dev <- ggplot(data = meltdf, aes(x = date, y = value, colour=variable, fill = variable)) + 
  geom_bar(stat='identity')+ 
  xlab(NULL) + ylab("")+
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims)+
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         legend.title=element_blank(), 
         legend.position = c(.9, .4),
         axis.text.x=element_blank(),
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank())
  

##??? représentation des déversements au by-pass de la STEP (ne pas représenter pour Thil)
#meltdf <- melt(pluieEtDeversements[c(1, 15, 16)],id="date")
meltdf <- melt(pluieEtDeversements[c(1, 15)],id="date")

P_STEP <- ggplot(data = meltdf, aes(x = date, y = value, colour=variable, fill = variable)) + 
  geom_bar(stat='identity')+ 
  xlab(NULL) + ylab("")+
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims)+
  theme( plot.margin=unit(c(-0.5,1,1,1), "cm"),
         panel.background = element_rect(fill = "white",colour = "white", linetype = "solid"),
         legend.title=element_blank(), 
         legend.position = c(.9, .4), 
         panel.grid.major = element_line(color = "gray91", size = 0.4),
         panel.grid.major.y = element_blank())

x11()

## représentation de l'impact sur l'amplitude journalère de la concentration en oxygène
x <- ggplot  ()+
  geom_line(data=thil_2015_event_ox,
            aes(x = date, y = filtre),
            colour =col_thil,size = 1)+
  geom_point(data = thil_2015_event_ox,
             aes(x = date, y =Signale_amplitude,colour=classes_alerte_amplitude),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse de \nl'amplitude journaliere", values = c("orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .9),
        axis.text.x=element_blank(),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())


## représentation de l'impact sur le niveau maximum journalier de la concentration en oxygène
y <- ggplot  ()+
  geom_line(data = thil_2015_event_ox,
            aes(x = date, y = filtre),
            colour =col_thil,size = 1)+
  geom_point(data = thil_2015_event_ox,
             aes(x = date, y =Signale_niveau,colour=classes_alerte_niveau),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse du \nmaximum journalier", values = c("orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
    theme(legend.position = c(.05, .9),
          axis.text.x=element_blank(),
          panel.grid.major = element_line(color = "gray91", size = 0.4),
          panel.grid.major.y = element_blank())

## représentation de l'impact sur la conductivité (impact avéré uniquement)
zz <- ggplot  ()+
  geom_line(data=reserve_2015_event_cond,
            aes(x = date, y = non_signalees),
            colour =col_reserve,size = 1)+
  geom_point(data = reserve_2015_event_cond,
             aes(x = date, y =signal_avere,colour=intensite_du_signal),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_gradient(name ="baisse de \nla conductivité", low="orange", high="red") +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .8),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())


z <- ggplot  ()+
  geom_line(data=thil_2015_event_cond,
            aes(x = date, y = non_signalees),
            colour =col_thil,size = 1)+
  geom_point(data = thil_2015_event_cond,
             aes(x = date, y =signal_avere,colour=global),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse de \nla conductivité", values = c("yellow","orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .8),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())



x11() 
plot_grid(P_pluie, 
          P_dev,
          P_STEP,  
          x,
          y,
          z,
          nrow=6, align = "v")



################# visualisation des pluies 
################ import des données d'évenement et création des layers ggplot associés

setwd("C:/Users/XZ4062/Desktop/LyRE/Monitoring/Sirenes/2_BDX_M/Valorisation des donnees/1_données brutes/Rapport annuel 2015/pluvio")
getwd()


## Import des données  d'évenement
bruges_pluies<- read.xlsx("pluvio_bruges.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                       rowNames = FALSE, detectDates = T, skipEmptyRows = FALSE,
                       rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)


cantinolle_pluies<- read.xlsx("pluvio_cantinolle.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
                        rowNames = FALSE, detectDates = F, skipEmptyRows = FALSE,
                        rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)


## conversion des information de début et fin d'évenement au bon format date

pluviometres <- c("bruges", "cantinolle")

cantinolle_pluies$debut <- as.POSIXct(strptime(cantinolle_pluies$debut  ,  format="%d/%m/%Y %H:%M",tz="GMT"))
cantinolle_pluies$fin   <- as.POSIXct(strptime(cantinolle_pluies$fin,  format="%d/%m/%Y %H:%M",tz="GMT"))
bruges_pluies$debut     <- as.POSIXct(strptime(bruges_pluies$debut,  format="%d/%m/%Y %H:%M",tz="GMT"))
bruges_pluies$fin       <- as.POSIXct(strptime(bruges_pluies$fin ,  format="%d/%m/%Y %H:%M",tz="GMT"))

cantinolle_pluies$classe <- as.factor(cantinolle_pluies$classe)
bruges_pluies$classe <- as.factor(bruges_pluies$classe)


######### subset en fonction de la classe de pluie
bruges_pluies_0  <- subset(bruges_pluies, classe== "0")
bruges_pluies_1  <- subset(bruges_pluies, classe== "1")
bruges_pluies_2  <- subset(bruges_pluies, classe== "2")
bruges_pluies_4  <- subset(bruges_pluies, classe== "4")
bruges_pluies_12 <- subset(bruges_pluies, classe== "12")

cantinolle_pluies_0  <- subset(cantinolle_pluies, classe== "0")
cantinolle_pluies_1  <- subset(cantinolle_pluies, classe== "1")
cantinolle_pluies_2  <- subset(cantinolle_pluies, classe== "2")
cantinolle_pluies_4  <- subset(cantinolle_pluies, classe== "4")
cantinolle_pluies_12 <- subset(cantinolle_pluies, classe== "12")
cantinolle_pluies_24 <- subset(cantinolle_pluies, classe== "24")
cantinolle_pluies_5200 <- subset(cantinolle_pluies, classe== "5200")


#0    -> vert transparent
#1    -> jaune transparent
#2    -> jaune
#4    -> orange
#12   -> rouge
#24   -> bordeaux
#5200 -> noir



## création de la charte visuelle pour matérialiser les évenement (création d'instance layer)
#### compliqué d'optimiser car besoin d'appeller le nom du site 
# création de la charte visuelle pour matérialiser les évenement (création d'instance layer)
#### compliqué d'optimiser car besoin d'appeller le nom du site 


###??? formule générale à utiliser normalement
#n <- length(cantinolle_pluies$Id)
#for (i in 1:n){
#  eval(parse(text=paste0("a_cantinolle",i," <- cantinolle_pluies[",i,",2]")))
#  eval(parse(text=paste0("b_cantinolle",i," <- cantinolle_pluies[",i,",3]")))
#  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle",i,", 
#                         xmax = b_cantinolle",i,",
#                         ymin = -Inf, ymax = +Inf,
#                         colour = NULL),
#                         fill = 'red', alpha = 0.5, show.legend = FALSE)")))
#  print("ok") 
#  assign(paste("G_cantinolle",i,sep="_"),data,.GlobalEnv)
#}
#liste_temp_cantinolle <- c()
#liste_temp_cantinolle <- paste0("G_cantinolle_",paste0(1:n))
#viz_pluies_cantinolle  <- c()
#viz_pluies_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")



########## Approche déguelasse en découpant par type de pluies ####### a automatiser plus tard !!
##??? cantinolle


#mini pluies
n <- length(cantinolle_pluies_0$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_0",i," <- cantinolle_pluies_0[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_0",i," <- cantinolle_pluies_0[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_0",i,", 
                         xmax = b_cantinolle_0",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'green', alpha = 0.4, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_0",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_0_",paste0(1:n))
viz_pluies_0_cantinolle  <- c()
viz_pluies_0_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")


#petites pluies
n <- length(cantinolle_pluies_1$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_1",i," <- cantinolle_pluies_1[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_1",i," <- cantinolle_pluies_1[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_1",i,", 
                         xmax = b_cantinolle_1",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'yellow', alpha = 0.4, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_1",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_1_",paste0(1:n))
viz_pluies_1_cantinolle  <- c()
viz_pluies_1_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")

# pluies un peu plus grosse
n <- length(cantinolle_pluies_2$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_2",i," <- cantinolle_pluies_2[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_2",i," <- cantinolle_pluies_2[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_2",i,", 
                         xmax = b_cantinolle_2",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'yellow', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_2",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_2_",paste0(1:n))
viz_pluies_2_cantinolle  <- c()
viz_pluies_2_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")

# pluies moyennes
n <- length(cantinolle_pluies_4$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_4",i," <- cantinolle_pluies_4[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_4",i," <- cantinolle_pluies_4[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_4",i,", 
                         xmax = b_cantinolle_4",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'orange', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_4",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_4_",paste0(1:n))
viz_pluies_4_cantinolle  <- c()
viz_pluies_4_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")


# pluies grosses
n <- length(cantinolle_pluies_12$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_12",i," <- cantinolle_pluies_12[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_12",i," <- cantinolle_pluies_12[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_12",i,", 
                         xmax = b_cantinolle_12",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'red', alpha = 0.5, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_12",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_12_",paste0(1:n))
viz_pluies_12_cantinolle  <- c()
viz_pluies_12_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")



# pluies tres  grosses
n <- length(cantinolle_pluies_24$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_24",i," <- cantinolle_pluies_24[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_24",i," <- cantinolle_pluies_24[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_24",i,", 
                         xmax = b_cantinolle_24",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'darkred', alpha = 1, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_24",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_24_",paste0(1:n))
viz_pluies_24_cantinolle  <- c()
viz_pluies_24_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")


# pluies vraiment tres  grosses
n <- length(cantinolle_pluies_5200$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_cantinolle_5200",i," <- cantinolle_pluies_5200[",i,",2]")))
  eval(parse(text=paste0("b_cantinolle_5200",i," <- cantinolle_pluies_5200[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_cantinolle_5200",i,", 
                         xmax = b_cantinolle_5200",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'black', alpha = 1, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_cantinolle_5200",i,sep="_"),data,.GlobalEnv)
}
liste_temp_cantinolle <- c()
liste_temp_cantinolle <- paste0("G_cantinolle_5200_",paste0(1:n))
viz_pluies_5200_cantinolle  <- c()
viz_pluies_5200_cantinolle  <- paste(liste_temp_cantinolle,collapse=" + ")



eval(parse(text=paste0("x2 <- x+",viz_pluies_0_cantinolle,"+
                               ",viz_pluies_1_cantinolle,"+
                               ",viz_pluies_2_cantinolle,"+
                               ",viz_pluies_4_cantinolle,"+
                               ",viz_pluies_12_cantinolle,"+
                               ",viz_pluies_24_cantinolle,"+
                               ",viz_pluies_5200_cantinolle," ")))
eval(parse(text=paste0("y2 <- y+",viz_pluies_0_cantinolle,"+
                               ",viz_pluies_1_cantinolle,"+
                       ",viz_pluies_2_cantinolle,"+
                       ",viz_pluies_4_cantinolle,"+
                       ",viz_pluies_12_cantinolle,"+
                       ",viz_pluies_24_cantinolle,"+
                       ",viz_pluies_5200_cantinolle," ")))
eval(parse(text=paste0("z2 <- z +",viz_pluies_0_cantinolle,"+
                               ",viz_pluies_1_cantinolle,"+
                       ",viz_pluies_2_cantinolle,"+
                       ",viz_pluies_4_cantinolle,"+
                       ",viz_pluies_12_cantinolle,"+
                       ",viz_pluies_24_cantinolle,"+
                       ",viz_pluies_5200_cantinolle," ")))


x11() 
plot_grid(P_pluie, 
          P_dev,
          P_STEP,  
          x2,
          y2,
          z2,
          nrow=6, align = "v")



### reserve de Bruges


# représentation de l'impact sur l'amplitude journalère de la concentration en oxygène
x <- ggplot  ()+
  geom_line(data=reserve_2015_event_ox,
            aes(x = date, y = filtre),
            colour =col_reserve,size = 1)+
  geom_point(data = reserve_2015_event_ox,
             aes(x = date, y =Signale_amplitude,colour=classes_alerte_amplitude),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse de \nl'amplitude journaliere", values = c("orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .9),
        axis.text.x=element_blank(),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())


## représentation de l'impact sur le niveau maximum journalier de la concentration en oxygène
y <- ggplot  ()+
  geom_line(data = reserve_2015_event_ox,
            aes(x = date, y = filtre),
            colour =col_reserve,size = 1)+
  geom_point(data = reserve_2015_event_ox,
             aes(x = date, y =Signale_niveau,colour=classes_alerte_niveau),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse du \nmaximum journalier", values = c("orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .9),
        axis.text.x=element_blank(),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())

## représentation de l'impact sur la conductivité (impact avéré uniquement)

z <- ggplot  ()+
  geom_line(data=reserve_2015_event_cond,
            aes(x = date, y = non_signalees),
            colour =col_reserve,size = 1)+
  geom_point(data = reserve_2015_event_cond,
             aes(x = date, y =signal_avere,colour=global),size=3)+
  xlab(NULL) + ylab("")    +
  scale_colour_manual(name ="baisse de \nla conductivité", values = c("yellow","orange","red")) +
  scale_x_datetime( breaks=date_breaks("1 month"), labels=date_format("%b %Y"), limits=lims) +
  theme(legend.position = c(.05, .8),
        panel.grid.major = element_line(color = "gray91", size = 0.4),
        panel.grid.major.y = element_blank())




#mini pluies
n <- length(bruges_pluies_0$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_0",i," <- bruges_pluies_0[",i,",2]")))
  eval(parse(text=paste0("b_bruges_0",i," <- bruges_pluies_0[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_0",i,", 
                         xmax = b_bruges_0",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'green', alpha = 0.4, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_0",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_0_",paste0(1:n))
viz_pluies_0_bruges  <- c()
viz_pluies_0_bruges  <- paste(liste_temp_bruges,collapse=" + ")


#petites pluies
n <- length(bruges_pluies_1$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_1",i," <- bruges_pluies_1[",i,",2]")))
  eval(parse(text=paste0("b_bruges_1",i," <- bruges_pluies_1[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_1",i,", 
                         xmax = b_bruges_1",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'yellow', alpha = 0.4, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_1",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_1_",paste0(1:n))
viz_pluies_1_bruges  <- c()
viz_pluies_1_bruges  <- paste(liste_temp_bruges,collapse=" + ")

# pluies un peu plus grosse
n <- length(bruges_pluies_2$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_2",i," <- bruges_pluies_2[",i,",2]")))
  eval(parse(text=paste0("b_bruges_2",i," <- bruges_pluies_2[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_2",i,", 
                         xmax = b_bruges_2",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'yellow', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_2",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_2_",paste0(1:n))
viz_pluies_2_bruges  <- c()
viz_pluies_2_bruges  <- paste(liste_temp_bruges,collapse=" + ")

# pluies moyennes
n <- length(bruges_pluies_4$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_4",i," <- bruges_pluies_4[",i,",2]")))
  eval(parse(text=paste0("b_bruges_4",i," <- bruges_pluies_4[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_4",i,", 
                         xmax = b_bruges_4",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'orange', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_4",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_4_",paste0(1:n))
viz_pluies_4_bruges  <- c()
viz_pluies_4_bruges  <- paste(liste_temp_bruges,collapse=" + ")


# pluies grosses
n <- length(bruges_pluies_12$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_12",i," <- bruges_pluies_12[",i,",2]")))
  eval(parse(text=paste0("b_bruges_12",i," <- bruges_pluies_12[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_12",i,", 
                         xmax = b_bruges_12",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'red', alpha = 0.5, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_12",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_12_",paste0(1:n))
viz_pluies_12_bruges  <- c()
viz_pluies_12_bruges  <- paste(liste_temp_bruges,collapse=" + ")







eval(parse(text=paste0("x2 <- x+",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))
eval(parse(text=paste0("y2 <- y+",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))
eval(parse(text=paste0("z2 <- z +",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))


x11() 
plot_grid(P_pluie, 
          P_dev,
          P_STEP,  
          x2,
          y2,
          z2,
          nrow=6, align = "v")



################traitement de la durée de temps sec

######### subset en fonction de la durée de temps sec précedent


cantinolle_pluies$classe <- as.factor(cantinolle_pluies$classe_temps_sec)
bruges_pluies$classe <- as.factor(bruges_pluies$classe_temps_sec)


bruges_pluies_ts1  <- subset(bruges_pluies, classe_temps_sec== "1")
bruges_pluies_ts2  <- subset(bruges_pluies, classe_temps_sec== "2")
bruges_pluies_ts3  <- subset(bruges_pluies, classe_temps_sec== "3")

cantinolle_pluies_ts1  <- subset(cantinolle_pluies, classe_temps_sec== "1")
cantinolle_pluies_ts2  <- subset(cantinolle_pluies, classe_temps_sec== "2")
cantinolle_pluies_ts3  <- subset(cantinolle_pluies, classe_temps_sec== "3")



#temps court
n <- length(bruges_pluies_1$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_ts1",i," <- bruges_pluies_ts1[",i,",2]")))
  eval(parse(text=paste0("b_bruges_ts1",i," <- bruges_pluies_ts1[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_ts1",i,", 
                         xmax = b_bruges_ts1",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'yellow', alpha = 0.4, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_ts1",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_ts1_",paste0(1:n))
viz_pluies_ts1_bruges  <- c()
viz_pluies_ts1_bruges  <- paste(liste_temp_bruges,collapse=" + ")

# temps moyen
n <- length(bruges_pluies_ts2$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_ts2",i," <- bruges_pluies_ts2[",i,",2]")))
  eval(parse(text=paste0("b_bruges_ts2",i," <- bruges_pluies_ts2[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_ts2",i,", 
                         xmax = b_bruges_ts2",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'orange', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_ts2",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_ts2_",paste0(1:n))
viz_pluies_ts2_bruges  <- c()
viz_pluies_ts2_bruges  <- paste(liste_temp_bruges,collapse=" + ")

# temps longs
n <- length(bruges_pluies_ts3$Id)
for (i in 1:n){
  eval(parse(text=paste0("a_bruges_ts3",i," <- bruges_pluies_ts3[",i,",2]")))
  eval(parse(text=paste0("b_bruges_ts3",i," <- bruges_pluies_ts3[",i,",3]")))
  eval(parse(text=paste0("data<- geom_rect(aes(xmin = a_bruges_ts3",i,", 
                         xmax = b_bruges_ts3",i,",
                         ymin = -Inf, ymax = +Inf,
                         colour = NULL),
                         fill = 'red', alpha = 0.7, show.legend = FALSE)")))
  print("ok") 
  assign(paste("G_bruges_ts3",i,sep="_"),data,.GlobalEnv)
}
liste_temp_bruges <- c()
liste_temp_bruges <- paste0("G_bruges_ts3_",paste0(1:n))
viz_pluies_ts3_bruges  <- c()
viz_pluies_ts3_bruges  <- paste(liste_temp_bruges,collapse=" + ")


eval(parse(text=paste0("x3 <- x+",viz_pluies_ts1_bruges,"+
                       ",viz_pluies_ts2_bruges,"+
                       ",viz_pluies_ts3_bruges,"")))

eval(parse(text=paste0("y3 <- y+",viz_pluies_ts1_bruges,"+
                       ",viz_pluies_ts2_bruges,"+
                       ",viz_pluies_ts3_bruges,"")))

eval(parse(text=paste0("z3 <- z +",viz_pluies_ts1_bruges,"+
                       ",viz_pluies_ts2_bruges,"+
                       ",viz_pluies_ts3_bruges,"")))


x11() 
plot_grid(P_pluie, 
          P_dev,
          P_STEP,  
          x3,
          y3,
          z3,
          nrow=6, align = "v")




