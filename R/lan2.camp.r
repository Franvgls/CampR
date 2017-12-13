#' Lances especiales de una campaña
#'
#' Busca y devuelve los lances especiales de una campaña 
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @examples lan2.camp("P01","Porc")
#' @export
lan2.camp<-function(camp,dns) {
  if (length(camp)>1) stop("Sólo se puede seleccionar una campaña, proces individualmente")
  datlan.camp(camp,dns)[datlan.camp(camp,dns)[,3]==2,"lance"]
}