#' Crea datos en formato SEFOS a partir de Camp, dns
#'
#' Function to complete HH with ICESrect and area
#' @param gr Grupo de organismos, 1 peces 2 crustaceos, 3 moluscos, 4 equinodermos, 5 inv..
#' @param esp Código de especie camp
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param plus Cual es la edad plus
#' @param cor.time Corrección del tiempo de arrastre al calcular las abundancias
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param AltAlk ALK alternativa tomada de un fichero de edad del camp edadxyy.dbf sin ruta ni extensión
#' @return A data.table con el formato de datos de SEFOS para la evaluación de caballa
#' @examples # sefos.camp(1,74,"N23","Cant",plus=3)
#' @export
sefos.camp<-function(gr,esp,camp,dns="Cant",plus=8,cor.time=TRUE,AltAlk=NA,incl2=FALSE) {
  datesp<-maphistage(gr,esp,camp,dns,0,plus,cor.time=cor.time,AltAlk=AltAlk,incl2=incl2,plot=FALSE,out.dat=T)
  datlan<-getICESarea(camp,dns,incl2=incl2)
  DB<-datlan[,c("lance")] #c("camp","lance","prof")
  DB$Country<-rep("SP",nrow(datlan))
  DB$CruiseCode<-rep(camp,nrow(datlan))
  DB$DateDay<-lubridate::day(datlan$fecha)
  DB$DateMth<- lubridate::month(datlan$fecha)
  DB$DateYr<-as.numeric(substr(lubridate::year(datlan$fecha),3,4))
  DB$HaulNb<- datlan$lance
  DB$ICESrect<- paste(substr(datlan$StatRec,1,2),substr(datlan$StatRec,3,4))
  DB$LatDgr<- trunc(datlan$lat)
  DB$LatMin<- trunc((datlan$lat-trunc(datlan$lat))*60)
  DB$LonDgr<- abs(trunc(datlan$long))
  DB$LonMin<- trunc((abs(datlan$long)-trunc(abs(datlan$long)))*60)
  DB$EW<-ifelse(datlan$long>0,"E","W")
  DB$time<-datlan$hora_l
  DB$Depth<-datlan$prof
  DB$Temp<-datlan$temp
  DB$Sal<-datlan$sali
  DB<-cbind(DB,datesp[,5:ncol(datesp)])
  DB$Total<-rowSums(datesp[,5:ncol(datesp)])
  return(DB[,2:ncol(DB)])
  }
