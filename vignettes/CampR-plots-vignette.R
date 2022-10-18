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

