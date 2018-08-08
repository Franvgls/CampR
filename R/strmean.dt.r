#' Medias intraestrato y estratificadas por estratos geográfico y batimétrico
#' 
#' Función interna para cálculos y aplicación de bootstrap
#' @param x Es el vector de abundancias en peso o número
#' @param sector Es la asignación al sector de cada muestra igual que en strmean {\link{strmean}} pero ha de tener dos caracteres, primero el sector geográfico y segundo el estrato batimétrico
#' @param area El área del sector correspondiente
#' @param Nas Permite obtener desviación estandar cuando sólo existe un dato, dando una desviación standard de 0. Es util para obtener una estimacion de la variabilidad aunque sólo haya un lance en un estrato
#' @return Devuelve la media estratificadda ponderada al área dentro de cada estrato y subestrato y la total del área
#' @export
# Medias intraestrato y estratificadas por estrato geogr?fico y sector detalladas con su asignaci?n
# a cada estrato y sector junto con sus SE y sus CV param?tricos.
# Las variables son las mismas que en el caso anterior. El campo Nas sirve para calcular a pesar de 
# estratos sin varianza por tener un sólo lance.  area<-as.numeric(as.character(area))
strmean.dt<- function(x,sector,area,Nas=FALSE) {
  area<-tapply(area,sector,mean)
  avgloc<-tapply(x,sector,mean)
  SEloc<-sqrt(tapply(x,sector,var))/sqrt(tapply(x,sector,length))
  if (Nas) {SEloc[is.na(SEloc)]<-0}
  CVloc<-(tapply(x,sector,sd)*100/tapply(x,sector,mean))
  sumsect<-tapply(avgloc*area,as.factor(substr(names(area),1,1)),sum)
  avgsect<-sumsect/tapply(area,as.factor(substr(names(area),1,1)),sum)
  var.st<-tapply(x,sector,var)*((area)^2)/tapply(x,sector,length)
  sect2<-tapply(area,as.factor(substr(names(area),1,1)),sum)
  SEsect<-sqrt(tapply(var.st,as.factor(substr(names(area),1,1)),sum,na.rm=Nas)/(sect2^2))
  CVsect<-(SEsect*100/avgsect)
  sect<-rbind(avgsect,SEsect,CVsect)
  sumestr<-tapply(avgloc*area,as.factor(substr(names(area),2,2)),sum)
  avgestr<-sumestr/tapply(area,as.factor(substr(names(area),2,2)),sum)
  estr2<-tapply(area,as.factor(substr(names(area),2,2)),sum)
  SEestr<-sqrt(tapply(var.st,as.factor(substr(names(area),2,2)),sum,na.rm=Nas)/(estr2^2))
  CVestr<-(SEestr*100/avgestr)
  estr<-rbind(avgestr,SEestr,CVestr)
  avg<-weighted.mean(avgloc,area)
  SE<-sqrt(sum(var.st,na.rm=Nas)/sum(area)^2)
  CV<-(SE*100/avg)
  avg<-rbind(avg,SE,CV)
  dummy<-rbind(avgloc,SEloc,CVloc)
  dummy<-list(locales=dummy,estratos=estr,sectores=sect,total=avg);
  dummy
}
