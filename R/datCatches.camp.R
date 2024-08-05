#' Crea datos de capturas en formato para evaluaciones tipo Capros o modelos bayesianos (completar con datTalCatch.camp)
#'
#' Function para geographical - bayesian models
#' @param gr Grupo, 1 peces, 2 crustaceos...
#' @param esp Código de especie
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param cor.time Corrección del tiempo de arrastre al calcular las abundancias (mantener en T, *da datos por media hora de lance*)
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @return A data.table con el formato de datos para otras especies con formato de geográfico por lances
#' @examples # datCatches.camp(1,18,"N23","Cant")
#' @export
datCatches.camp<-function(gr,esp,camp,dns="Cant",cor.time=TRUE,incl2=FALSE) {
  datesp<-maphist(gr,esp,camp,dns,cor.time=cor.time,incl2=incl2,plot=FALSE,out.dat=T)
  datlan<-getICESarea(camp,dns,incl2=incl2)
  DB<-datlan[,c("lance")] #c("camp","lance","prof")
  DB$Survey<-rep(camp,nrow(datlan))
  DB$DateYr<-datlan$year
  DB$Quarter<- datlan$quarter
  DB$HaulNb<- datlan$lance
  DB$latdec<-datlan$lat
  DB$longdec<-datlan$long
  DB$SubDiv<-datlan$icesArea
  DB$ICESrect<- paste(substr(datlan$StatRec,1,2),substr(datlan$StatRec,3,4))
  DB$Depth<-datlan$prof
  DB$N30<-datesp$numero
  DB$Kg30<-datesp$peso.gr/1000
  return(DB[,2:ncol(DB)])
  }
