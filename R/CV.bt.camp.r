#' Abundancias, SE y coeficientes de variación paramétricos calculados con bootstrap
#'
#' Función de resultados a partir de bootstrap con Rbt iteraciones conservando el diseño de muestreo
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña de la que sacar datos año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa"
#' @param ind El dato a procesar, si (p)eso o (n)umero
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T saca los resultados en Kg, si F en gramos
#' @param Rbt número de iteraciones bootstrap para calcular los intervalos de confianza
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @return Devuelve el valor medio por bootstrap de la media, el error estándar y el CV
#' @seealso grafhistbox {\link{grafhistbox.r}}
#' @export
CV.bt.camp<- function(gr,esp,camp,dns,ind="p",cor.time=T,kg=T,Rbt=1000,excl.sect=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  mm<-datos.camp(gr,esp,camp,dns,cor.time=cor.time,kg=kg)
  if (any(!is.na(excl.sect))) {
    for (i in 1:length(excl.sect)) {mm<-mm[-grep(excl.sect[i],as.character(mm$sector)),]}
    mm$sector<-factor(as.character(mm$sector))
  }
  require(boot)
  if (ind=="p") {dumb<-boot(mm$peso,strmean,Rbt,stype="f",strata=mm$sector,sector=mm$sector,area=mm$arsect)}
  else {dumb<-boot(mm$numero,strmean,Rbt,stype="f",strata=mm$sector,sector=mm$sector,area=mm$arsect)}
  CV.camp<-list(avg.sv=dumb$t0,sd.sv=sd(dumb$t),CV.sv=sd(dumb$t)*100/mean(dumb$t))
  print(CV.camp)
}
