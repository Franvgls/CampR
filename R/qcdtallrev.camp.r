#' Revisión de los histogramas de tallas de todas las especies de una campaña
#'
#' Revisa la distribución de tallas estratificada de todas las especies de peces que hayan aparecido en una campaña.Se puede salir de la función cerrando el gráfico y se puede obtener una función para la campaña y usarla en cada lance. Ver documentación qcdtall.camp
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "POrc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param nlans Permite no mostrar la relación talla-peso de una especie si el número de lances en que ha aparecido es menor de nlans (por defecto nlans=2) 
#' @seealso {\link{qcdtall.camp}}
#' @examples qcdtallrev.camp(camp="N10",dns="Cant",nlans=2)
#' @family Control de calidad
#' @export
qcdtallrev.camp<- function(camp="P12",dns="Porc",nlans=2) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dumblist<-ListFauna.camp(gr=1,camp,dns=dns)
  dumblist<-dumblist[order(dumblist$nlan,decreasing=TRUE),]
  if (!devAskNewPage()) devAskNewPage(T)
  for (i in 1:nrow(dumblist)) {
    dtall.camp(1,dumblist$esp[i],camp=camp,dns=dns,out.dat=FALSE,ti=TRUE)
    if (dumblist$nlan[i]==nlans) break
  }
  devAskNewPage(F)
}