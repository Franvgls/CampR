#' Peso de la distribución de tallas de esp en los lances solicitados
#'
#' Da el peso de la distribución de tallas de las capturas de la especie en los lances solicitados 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña para representar el histograma de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lances Número de lance o lances de los que se extrae el dato de peso de la especie solicitada. Si lances se deja en NA, da todos los lances de la campaña. 
#' @param sex por defecto (T) da valores por sexos si los hay, si F suma todos, si todos son indeterminados no afecta.
#' @param areg Para cambiar el coeficiente "a" de la regresión si es distinto al archivado en el CAMP.
#' @param breg Para cambiar el coeficiente "b" de la regresión si es distinto al archivado en el CAMP.
#' @family Distribuciones de tallas
#' @examples dtallan.peso(gr=1,esp=9,camp="N02",dns="Cant",lance=50)
#' @export
dtallan.peso<-function(gr,esp,camp,dns="Cant",lances=NA,sex=TRUE,areg=NA,breg=NA) {
  if (length(esp)>1 | length(camp)>1) {
    stop("la función sólo sirve para una campaña y una especie")
  }
  esp<-format(esp,width=3,justify="r")
  dumb<-dtallan.camp(gr,esp,camp,dns,lances=lances,sex=sex)
  if (is.na(areg) & is.na(breg)) abdumb<-talpes.camp(gr,esp)
  else abdumb<-c(areg,breg)
  if (ncol(dumb)==2) {
    result<-(abdumb[1]*(dumb$talla+.5)^abdumb[2])*dumb$indet
  }
  else result<-(abdumb[1]*(dumb$talla+.5)^abdumb[2])*rowSums(dumb[2:ncol(dumb)])
  sum(result)
}
