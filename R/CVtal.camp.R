#' Abundancias, SE y coeficientes de variación paramétricos por subestrato, estrato y total para rango tallas
#'
#' Función de resultados
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que sacar los dato un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param tmin Talla mínima
#' @param tmax Talla máxima
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Si F el error estándar de los subestratos y estratos con un solo lance son NA. Si T utiliza el valor del estrato y evita los NAs
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @return Devuelve una lista con valorees de media, SE y CV para los subestratos, estratos y total de la campaña
#' @seealso {\link{CV.camp}}, {\link{CVtal.bt.camp}}
#' @export
CVtal.camp<- function(gr,esp,camp,dns,tmin,tmax,cor.time=TRUE,Nas=FALSE,excl.sect=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de m?s de una")}
  esp<-format(esp,width=3,justify="r")
  mm<-datos.camp(gr,esp,camp,dns,cor.time=cor.time)
  mm1<-dattalgr.camp(gr,esp,camp,dns,tmin,tmax,incl2=FALSE)
  mm<-merge(mm[,c("sector","lance","arsect")],mm1[,c("lan","numero")],by.x="lance",by.y="lan")
  if (any(!is.na(excl.sect))) {
    for (i in 1:length(excl.sect)) {mm<-mm[-grep(excl.sect[i],as.character(mm$sector)),]}
    mm$sector<-factor(as.character(mm$sector))
  }
  dumb<-strmean.dt(mm$numero,mm$sector,mm$arsect,Nas=Nas)
  CV.camp<-dumb
  dumb
}
