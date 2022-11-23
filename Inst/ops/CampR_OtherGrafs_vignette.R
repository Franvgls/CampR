## ---- echo=FALSE, results='hide',include=FALSE--------------------------------
library(CampR)

## ----tallas, fig.width=6.5,fig.height=4---------------------------------------
dtall.camp(2,19,Psh[15:16],"Porc",es=F,layout=c(2,1))

## ----talla edad, fig.width=6.5,fig.height=4,results='hide'--------------------
grafedtal.camp(1,43,"P09","Porc",es=FALSE,out.dat=TRUE)

## ----SE+bootstrap, fig.width=6.5,fig.height=4,results='hide'------------------
grafhistbox(1,79,Psh[1:17],"Porc",ci.lev = .8,DLS = F,tline = F,SE=T,cex.leg = .8)

## ----DLS, fig.width=6.5,fig.height=4,results='hide'---------------------------
grafhistbox(1,79,Psh[1:17],"Porc",ci.lev = 0,DLS = T,tline = F,SE=F,cex.leg = .8)

