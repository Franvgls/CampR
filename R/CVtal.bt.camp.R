#' Abundancias, SE y coeficientes de variación paramétricos calculados con bootstrap
#'
#' Función de resultados a partir de bootstrap con Rbt iteraciones conservando el diseño de muestreo
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que sacar datos año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX", Mediterráneo "MXX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant" o "Cnew", Golfo de Cádiz "Arsa" y Mediterráneo "Medi"
#' @param tmin Talla mínima
#' @param tmax Talla máxima
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Rbt número de iteraciones bootstrap para calcular los intervalos de confianza
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @return Devuelve el valor medio por bootstrap de la media, el error estándar y el CV
#' @seealso {\link{grafhistbox}}
#' @export
CVtal.bt.camp<- function(gr,esp,camp,dns,tmin,tmax,cor.time=TRUE,Rbt=1000,excl.sect=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  mm<-datos.camp(gr,esp,camp,dns,cor.time=cor.time,kg=F)
  mm1<-dattalgr.camp(gr,esp,camp,dns,tmin,tmax,incl2=FALSE)
  mm<-merge(mm[,c("sector","lance","arsect")],mm1[,c("lan","numero")],by.x="lance",by.y="lan")
  if (any(!is.na(excl.sect))) {
    for (i in 1:length(excl.sect)) {mm<-mm[-grep(excl.sect[i],as.character(mm$sector)),]}
    mm$sector<-factor(as.character(mm$sector))
  }
  dumb<-boot::boot(mm$numero,strmean,Rbt,stype="f",strata=mm$sector,sector=mm$sector,area=mm$arsect)
  CV.camp<-list(avg.sv=dumb$t0,sd.sv=sd(dumb$t),CV.sv=sd(dumb$t)*100/mean(dumb$t))
  print(CV.camp)
}
