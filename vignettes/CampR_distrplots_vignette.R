## ---- echo=FALSE, results='hide',include=FALSE--------------------------------
library(CampR)

## ----maphistal, fig.height=9, fig.width=9, include=TRUE, results='hide'-------
maphistal(gr=1,esp=50,camp=Nsh[31:33],dns="Cant",tmin = 0,tmax=23,es=F,ceros=F,layout=c(1,3),bw=T)

## ----maphistage, fig.height=5, fig.width=9, include=TRUE, results='hide'------
maphistage(gr=1,esp=43,camp=Psh[14:16],dns="Porc",age = 1,es=F,ceros=F,layout=c(3,1),bw=T)

## ----mapecol, fig.height=7.1, fig.width=9, include=TRUE, results='hide'-------
MapEcol.camp(1,999,Nsh[25:30],"Cant",ind="n",bw=TRUE,indec="simp",out.dat=TRUE,layout=c(2,3))

