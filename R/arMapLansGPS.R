#' Combinación en dos mapas de armap.camp con etiquetas de los lance y mapa con los lances en segmentos
#'
#' Muestra un mapa con el número de lance en el punto del lance y debajo o al lado mapa de los lances en segmentos
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa"
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña
#' @param col Define el color de los segmentos
#' @param lwd Define el ancho de los segmentos
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param Nlans se T incluye los números de lance por encima de los segmentos
#' @param rumbo si T incluye un punto al final del lance, en el punto de la virada, sólo en el segundo mapa
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un mapa, pero no objetos de datos.
#' @seealso {\link{datlan.camp}}, {\link{qcdistlan.camp}}
#' @examples arMapLansGPS("N20","Cant",xlims=c(-9.8,-7.8),ylims=c(43.5,44.2),layout=c(1,2),rumbo=T)
#' @family mapas
#' @family PescaWin
#' @export
arMapLansGPS<-function(camp,dns="Porc",incl0=FALSE,xlims=NA,ylims=NA,col=2,lwd=2,places=TRUE,Nlans=FALSE,
                       rumbo=F,es=T,bw=FALSE,layout=NA) {
  opar<-par(no.readonly=TRUE)
  if (dns=="Cant" & any(is.na(layout))) par(mfrow=c(2,1))
  if (c(dns=="Porc" | dns=="Arsa") & any(is.na(layout))) par(mfrow=c(1,2))
  # if (any(!is.na(layout))) par(mfrow=layout)
  armap.camp(camp=camp,dns=dns,xlims=xlims,ylims=ylims,col=col,lwd=lwd,places=places,Nlans=TRUE,es=es,bw=bw)
  MapLansGPS(camp=camp,dns=dns,incl0=incl0,xlims=xlims,ylims=ylims,col=col,lwd=lwd,places=places,Nlans = F,es=es,bw=bw,rumbo=rumbo)
  par(opar)
}
