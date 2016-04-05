###################figure sur 6 mois ## a réintégrer correctement dans le code
#Création du vecteur "temps"
debut <- as.POSIXct(strptime("01/07/2015 00:00:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
fin   <- as.POSIXct(strptime("3/12/2015 23:45:00",  format="%d/%m/%Y %H:%M",tz="GMT"))
lims  <- c(debut, fin)


#  réprésentation par rapport à l'intenité de la pluie



##??? cantinolle

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



eval(parse(text=paste0("x2_cant <- x+",viz_pluies_0_cantinolle,"+
                       ",viz_pluies_1_cantinolle,"+
                       ",viz_pluies_2_cantinolle,"+
                       ",viz_pluies_4_cantinolle,"+
                       ",viz_pluies_12_cantinolle,"+
                       ",viz_pluies_24_cantinolle,"+
                       ",viz_pluies_5200_cantinolle," ")))
eval(parse(text=paste0("y2_cant <- y+",viz_pluies_0_cantinolle,"+
                       ",viz_pluies_1_cantinolle,"+
                       ",viz_pluies_2_cantinolle,"+
                       ",viz_pluies_4_cantinolle,"+
                       ",viz_pluies_12_cantinolle,"+
                       ",viz_pluies_24_cantinolle,"+
                       ",viz_pluies_5200_cantinolle," ")))
eval(parse(text=paste0("z2_cant <- z +",viz_pluies_0_cantinolle,"+
                       ",viz_pluies_1_cantinolle,"+
                       ",viz_pluies_2_cantinolle,"+
                       ",viz_pluies_4_cantinolle,"+
                       ",viz_pluies_12_cantinolle,"+
                       ",viz_pluies_24_cantinolle,"+
                       ",viz_pluies_5200_cantinolle," ")))


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







eval(parse(text=paste0("x2_bruges <- x+",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))
eval(parse(text=paste0("y2_bruges <- y+",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))
eval(parse(text=paste0("z2_bruges <- z +",viz_pluies_0_bruges,"+
                       ",viz_pluies_1_bruges,"+
                       ",viz_pluies_2_bruges,"+
                       ",viz_pluies_4_bruges,"+
                       ",viz_pluies_12_bruges," ")))



x11() 
plot_grid(x2_cant,
          y2_cant,
          z2_cant,  
          x2_bruges,
          y2_bruges,
          z2_bruges,
          nrow=6, align = "v")




