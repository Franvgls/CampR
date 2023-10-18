## ---- echo=FALSE, results='hide',include=FALSE--------------------------------
library(CampR)

## ----SP-NORT, fig.width=7.2,fig.height=3.8------------------------------------
MapNort()

## ----SP-NORT results, fig.width=7.2,fig.height=3.8----------------------------
armap.camp("N07","Cant",ti=T,es=T)

## ----SP-PORC, fig.width=7.2,fig.height=3.8------------------------------------
mapporco()

## ----Porcupine, fig.width=6,fig.height=4.7------------------------------------
par(mfrow=c(1,2))
armap.camp("P07","Porc",ti=T)
armap.camp("P07","Porc",ti=F,CTD=T,lans=F)

## ----SP-ARSA, fig.width=7.2,fig.height=3.8------------------------------------
MapArsa()

## ----Arsa resultaos, fig.width=6.2,fig.height=5.1-----------------------------
armap.camp("207","Arsa",ti=T,es=T)

## ----Medits, fig.width=7.2,fig.height=6.4-------------------------------------
MapMedit()

## ----Medits-result, fig.width=6.4,fig.height=6.4------------------------------
armap.camp("M07","Medi",ti=T,es=T)

## ----IbAtlFUs, fig.width=7.2,fig.height=6.4-----------------------------------
MapIberia(xlims = c(-10.5, -1.1), ylims = c(35.95, 44.52),places = T,pais=F,
          FU=c("FU31","FU25","FU26","FU27","FU39","FU30"),ColFU = "white")
text(-9.5,42.5,"FU26",font=2,col=1,cex=1)
text(-9.5,40.5,"FU27",font=2,col=1,cex=1)
text(-5,44,"FU31",font=2,col=1,cex=1)
text(-9.5,43.5,"FU25",font=2,col=1,cex=1)
text(-6.8,36.5,"FU30",font=2,col=1,cex=1)

