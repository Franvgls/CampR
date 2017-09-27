#' Gráfico con los lances en segmentos
#'
#' gráfica todos los lances de una campaña con las distancias recorridas en cada uno de ellos
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" 
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña 
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña  
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param ax Si T saca los ejes x e y
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux
#' @seealso {\link{datlan.camp}}, {\link{qcdistlan.camp}}
#' @examples MapLansGPS("12C","Cant")
#' @family mapas
#' @family PescaWin
#' @export
MapLansGPS<-function(camp,dns="Pnew",incl0=FALSE,xlims=NA,ylims=NA,places=TRUE,es=T,bw=FALSE,ax=T) {
  #if (!all(any(is.na(xlims)),any(is.na(ylims))))  stop("Si se especifica limite de coordenadas debe hacerlo en latitud y longitud")
  lan<-datlan.camp(camp,dns,redux=FALSE,incl2=TRUE,incl0=TRUE)
  lannul<-lan[lan$validez==0,c("longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  lan<-lan[lan$validez!=0,c("longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    if (any(!is.na(xlims))) {mapporco(xlims=xlims,ylims=ylims,ax=ax)} else mapporco()
    }
  if (substr(dns,1,4)=="Cant" | dns=="Cnew" ) {
    if (any(!is.na(xlims))) {MapNort(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapNort()
  }
  if (dns=="Arsa") {
    if (any(!is.na(xlims))) {MapArsa(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapArsa()
  }
  if (dns=="Medi") {
    if (any(!is.na(xlims))) {MapMedit(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapMedit()
  }
  segments(lan$longitud_l,lan$latitud_l,lan$longitud_v,lan$latitud_v,col=1,lwd=2)
  if (incl0) segments(lannul$longitud_l,lannul$latitud_l,lannul$longitud_v,lannul$latitud_l,col=2,lwd=2)
}