## ---- echo=FALSE, results='hide', include=FALSE-------------------------------
library(CampR)

## ----armap.camp,fig.width=18,fig.height=8,fig.align='center'------------------
armap.camp("N12","Cant") 

## ----qcdistlan.camp-----------------------------------------------------------
qcdistlan.camp("N12","Cant",todos=FALSE,pc.error=2)

## ----qcLW.camp, echo=FALSE----------------------------------------------------
qcLW.camp(1,51,"N12","Cant",margerr=14)

## ----qcdtall.camp, echo=FALSE-------------------------------------------------
qcdtall.camp(1,51,"N12","Cant",107)

## ----qcTalPez.lan, echo=TRUE--------------------------------------------------
qcTalPez.lan("P18",dns="Porc",16,gr=1)

## ----grafedtal.camp, echo=FALSE-----------------------------------------------
grafedtal.camp(1,42,"N12","Cant",plus=6)

