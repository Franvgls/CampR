## ----message=FALSE,results='hide',include=FALSE-------------------------------
library(ggplot2)
library(mapdata)
library(CampR)
library(knitr)
library(kableExtra)
options(knitr.table.format = "markdown") 

## ----Survey design, fig.height=7, fig.width=7---------------------------------
armap.tot("P16","Porc",es=FALSE,leg = TRUE,bw=FALSE)

## ----Hake abundance,warning=FALSE,message=FALSE,tidy=TRUE---------------------
library(dplyr)
hake<-CampR::datab(1,50,"P16","Porc")
kableExtra::kable(hake,digits=2,caption="Hake abundance Porcupine 2016 Survey") %>%
  kableExtra::kable_styling(bootstrap_options="condensed",full_width=T,position="center")

## ----Hake ts. abundance-------------------------------------------------------
CampR::grafhistbox.comp(1,50,Psh,"Porc",es=F)

## ----Total abundance map, results="hide"--------------------------------------
Porc<-map_data(Porc.map)
hake<-maphist(1,50,"P16","Porc",out.dat=T,plot=F)
ggplot(hake)+
  geom_polygon(aes(long,lat,group=group),data=Porc,fill="white",color="black")+
  geom_point(aes(x=long,y=lat,size=sqrt(numero)),color="blue")+scale_size_continuous(name="No. inds.")+coord_fixed(1.3)

## -----------------------------------------------------------------------------
dtall.camp(1,50,"P16","Porc",es=F)

## -----------------------------------------------------------------------------
maphistal(1,50,"P16","Porc",1,20,ti=list(label="Merluccius merluccius\n < 20 cm",font=4),ceros=F)

## ----Recruitment strength time series, echo=FALSE, message=FALSE, warning=FALSE, results='markup',tidy=TRUE----
recr.ts<-CampR::dattal.camps(1,50,Psh,"Porc",1,20,graf=T)
kable(t(recr.ts),digits=2,col.names = camptoyear(Psh),caption="Hake recruitment strength") %>%
  kable_styling(full_width=TRUE,position="center")

