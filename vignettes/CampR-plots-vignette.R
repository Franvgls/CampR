## ---- echo=FALSE---------------------------------------------------------
library(CampR)

## ---- fig.width=7.2,fig.height=4.1---------------------------------------
armap.camp("N07","Cant",ti=T,es=T)

## ---- fig.width=7.2,fig.height=5.1---------------------------------------
par(mfrow=c(1,2))
armap.camp("P07","Porc",ti=T)
armap.camp("P07","Porc",ti=F,CTD=T,lans=F)

## ---- fig.width=7.2,fig.height=6.1---------------------------------------
armap.camp("207","Arsa",ti=T,es=T)

## ---- fig.width=6.2,fig.height=6.5---------------------------------------
armap.camp("M07","Medi",ti=T,es=T)

## ---- fig.width=6.2,fig.height=6.5---------------------------------------
maphist(2,19,"P15","Porc",ti=T)

