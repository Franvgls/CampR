#' Lances nulos en una campaña
#'
#' Busca y devuelve los lances nulos de una campaña 
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @export
lan0.camp<-function(camp,dns) {
  if (length(camp)>1) stop("Sólo se puede seleccionar una campaña, proces individualmente")
  datlan.camp(camp,dns,incl0=T)[datlan.camp(camp,dns,incl0=T)[,3]==0,"lance"]
}
# lan0.camp("P01","Pnew")