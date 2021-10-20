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
#' @param cexlab Tamaño de los números de lance utilizados en el gráfico de armap.lan parte del gráfico
#' @param rumbo si T incluye un punto al final del lance, en el punto de la virada, sólo en el segundo mapa
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un mapa, pero no objetos de datos.
#' @seealso {\link{datlan.camp}}, {\link{qcdistlan.camp}}
#' @examples arMapLansGPS("N20","Cant",xlims=c(-9.8,-7.8),ylims=c(43.5,44.2))
#' @family mapas
#' @family PescaWin
#' @export
arMapLansGPS<-function(camp,dns="Porc",incl0=FALSE,xlims=NA,ylims=NA,col=2,lwd=2,cexlab=.6,places=TRUE,Nlans=FALSE,
                       es=T,bw=FALSE,layout=NA) {
  opar<-par(no.readonly=TRUE)
  datlan<-datlan.camp(camp=camp,dns=dns,incl2=T,incl0=incl0)
  if (dns=="Cant") {
    if (any(is.na(xlims))) xlims=c(-10.25,-1.4);ylims=c(41.82,44.48)
    # MapNort(places=places,xlims=xlims,ylims = ylims,bw=bw)
    # segments(datlan$longitud_l,datlan$latitud_l,datlan$longitud_v,datlan$latitud_v,col=col)
    # text(latitud_l~longitud_l,datlan,label=lance,cex=cexlab,font=2,pos=1,offset=0)
    if (any(!is.na(layout))) par(mfrow=layout)
    else par(mfrow=c(2,1))
    armap.camp(camp=camp,dns=dns,xlims=xlims,ylims=ylims,col=col,lwd=lwd,places=places,Nlans=TRUE,es=es,bw=bw,cexlab=cexlab)
    MapLansGPS(camp=camp,dns=dns,incl0=incl0,xlims=xlims,ylims=ylims,col=col,lwd=lwd,places=places,Nlans = FALSE,es=es,bw=bw,rumbo=FALSE)
  }
  if (c(dns=="Porc" | dns=="Arsa")) {
  par(mfrow=c(1,2)) # if (any(!is.na(layout))) par(mfrow=layout)
  armap.camp(camp=camp,dns=dns,xlims=xlims,ylims=ylims,cexlab=cexlab,col=col,lwd=lwd,places=places,Nlans=TRUE,es=es,bw=bw)
  MapLansGPS(camp=camp,dns=dns,incl0=incl0,xlims=xlims,ylims=ylims,col=col,lwd=lwd,places=places,Nlans = FALSE,es=es,bw=bw,rumbo=FALSE)
  }
  par(opar)
}
