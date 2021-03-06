### Rapport annuel ## volet m�trologie
##################################
######## Vision globale
#### synth�se m�trologique


### vision globale des donn�s acquises
x11()
summaryPlot(cantinolle_2015_vf,
            col.mis = "gray",
            col.data ="darkolivegreen2",
            col.hist = "black",
            type = "density",
            period = "years")


############d�coupage du jeu de donn�es avant le traitement pour plus de visibilit�
## Thil

n<- length(param)
for (i in 1:n){
  x11()
  eval(parse(text=paste0("summaryPlot(thil_2015_",param[i],"[,-c(2)],
                         col.mis = \"gray\",
                         col.trend =\"#208CFF\",
                         col.data =\"#208CFF\",
                         col.hist=\"#208CFF\",
                         ylab = c(\"Thil_",param[i],"\", \"Pourcentage du total\"), 
                         xlab = c(\"Date\", \"r�partition des valeurs\"), 
                         lwd=3,
                         cex=12
                         )")))
}

## Reserve

n<- length(param)
for (i in 1:n){
  x11()
  eval(parse(text=paste0("summaryPlot(reserve_2015_",param[i],"[,-c(2)],
                         col.mis = \"gray\",
                         col.trend =\"#0DB219\",
                         col.data =\"#0DB219\",
                         col.hist =\"#0DB219\",
                         ylab = c(\"Reserve_",param[i],"\", \"Pourcentage du total\"), 
                         xlab = c(\"Date\", \"r�partition des valeurs\"), 
                         lwd=3,
                         cex=12
                         )")))
}




##Cantinolle

n<- length(param)
for (i in 1:n){
  x11()
  eval(parse(text=paste0("summaryPlot(cantinolle_2015_",param[i],"[,-c(2)],
                         col.mis = \"gray\",
                         col.trend =\"#0DB219\",
                         col.data =\"#0DB219\",
                         col.hist =\"#0DB219\",
                         ylab = c(\"Reserve_",param[i],"\", \"Pourcentage du total\"), 
                         xlab = c(\"Date\", \"r�partition des valeurs\"), 
                         lwd=3,
                         cex=12
                         )")))
}





