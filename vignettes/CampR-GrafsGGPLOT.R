## ----load library, message=FALSE, warning=FALSE, include=FALSE----------------
library(ggplot2)
library(CampR)
library(plotly)
# library(RColorBrewer)
# yor_col<- brewer.pal(7, "Greens")

## ----Data---------------------------------------------------------------------
Porc<-ggplot2::map_data(Porc.map) 
head(Porc)
hake<-CampR::maphist(1,50,"P16","Porc",out.dat=T,plot=F)

## ----Graf Porcupine-----------------------------------------------------------
p<-ggplot2::ggplot(hake)+
  geom_polygon(aes(long,lat,group=group),data=Porc,fill="white",color="darkgrey")+
  geom_point(aes(x=long,y=lat,size=sqrt(numero),text=lan),color="blue")+
  scale_size_continuous(name="No. ind.")+coord_fixed(1.3)
ggplotly(p,tooltip=c("text","lance"),width=800,height=500)

## ----resultados tabla---------------------------------------------------------
library(knitr)
library(kableExtra)
options(knitr.table.format = "markdown") 
kable(databICES(1,50,"N16","Cant"),digits=2,caption="Merluza en 2016 Cantábrico y Galicia") %>%
  kable_styling(bootstrap_options="condensed",full_width=F,position="center")

## ----Demersales datos---------------------------------------------------------
Nort<-ggplot2::map_data(Nort.map)
head(Nort)

## ----mapas Demersales---------------------------------------------------------
ggplot2::ggplot(data=Nort)+geom_polygon(aes(long,lat,fill=region,group=group),col="white")+
  coord_fixed(1.3)

