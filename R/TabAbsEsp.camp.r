#'Tabla resumen abundancias estratificadas por estrato y sector 
#'
#'Salida de datos para informe de campaña del Cantábrico y Galicia (con 5 estratos y 3 sectores).
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año concreto (XX): Demersales "NXX"
#' @param dns Elige el origen de las bases de datos: sólo sirve para el Cantábrico/Galicia "Cant
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea válido cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve un objeto en formato list con las abundancias y errores estándar de la especie elegida en la campaña elegida.
#' @export
TabAbsEsp.camp<-function(gr,esp,camp,dns="Cant",cor.time=T,Nas=F) {
  esp<-format(esp,width=3,justify="r")
  if (substr(dns,1,4)!="Cant") {stop("Función sólo disponible para Demersales Costa Norte divisiones IXa, VIIIc Este y VIIIc Oeste")}
  dumbw<-CV.camp(gr,esp,camp,dns,cor.time=cor.time,Nas=Nas)
  dumbn<-CV.camp(gr,esp,camp,dns,cor.time=cor.time,Nas=Nas,ind="n")
  dumbwloc<-rbind(sapply(dumbw$locales[1:2,c(1,4,7,10,13)],t),sapply(dumbw$locales[1:2,c(2,5,8,11,14)],t),sapply(dumbw$locales[1:2,c(3,6,9,12,15)],t))
  dumbwsect<-sapply(dumbw$sectores[1:2,],t)
  dumbwestr<-sapply(dumbw$estratos[1:2,],t)
  dumbwtot<-t(dumbw$total[1:2,])
  resw<-cbind(rbind(dumbwloc,dumbwsect),rbind(t(dumbw$estratos[1:2,]),dumbwtot))
  dumbnloc<-rbind(sapply(dumbn$locales[1:2,c(1,4,7,10,13)],t),sapply(dumbn$locales[1:2,c(2,5,8,11,14)],t),sapply(dumbn$locales[1:2,c(3,6,9,12,15)],t))
  dumbnsect<-sapply(dumbn$sectores[1:2,],t)
  dumbnestr<-sapply(dumbn$estratos[1:2,],t)
  dumbntot<-t(dumbn$total[1:2,])
  resn<-cbind(rbind(dumbnloc,dumbnsect),rbind(t(dumbn$estratos[1:2,]),dumbntot))
  names.sect<-paste(c("Avg","SE"),rep(colnames(dumbw$sectores),each=2),sep="_")
  names.estr<-t(colnames(dumbw$estratos))
  names.tot<-paste(rep("Total",2),rownames(dumbw$total)[1:2],sep="_")
  colnames(resw)<-c(names.sect,names.tot)
  rownames(resw)<-c(names.estr,"Total")
  colnames(resn)<-c(names.sect,names.tot)
  rownames(resn)<-c(names.estr,"Total")
  list(absw=resw,absn=resn)
}
#TabAbsEsp.camp("1"," 36","N08","Cant")
#TabAbsEsp.Camp(1,36,"P10","Pnew")
