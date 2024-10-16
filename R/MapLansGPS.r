#' Mapa con los lances en segmentos
#'
#' gráfica todos los lances de una campaña con las distancias recorridas en cada uno de ellos
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa"
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña
#' @param ti si TRUE inclye el nombre de la campaña en el gráfico
#' @param col Define el color de los segmentos
#' @param lwd Define el ancho de los segmentos
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param incl0 si T incluye los lances nulos con color negro
#' @param Nlans si T incluye los números de lance por encima de los segmentos
#' @param cexlans tamaño de las etiquetas de los lances si se incluyen
#' @param rumbo si T incluye
#' @param ax Si T saca los ejes x e y
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @param ErrLans NA por defecto, pero si se da un vector con varios lances con errores que sale de qcdistlan.camp()$lances solo saca los lances con error
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux
#' @seealso {\link{datlan.camp}}, {\link{qcdistlan.camp}}
#' @examples MapLansGPS("12C","Cant",ti=TRUE,xlims=c(-10.4,-7.4),ylims=c(41.8,44.6))
#' @examples MapLansGPS(123,"Arsa",ti=TRUE,ErrLans=c(1,7,40),Nlans=T,graf="Maplans123")
#' @family mapas
#' @family PescaWin
#' @export
MapLansGPS<-function(camp,dns="Porc",leg=F,incl0=FALSE,xlims=NA,ylims=NA,ti=FALSE,col=2,lwd=2,places=TRUE,Nlans=FALSE,cexlans=.8,rumbo=FALSE,
                     es=TRUE,bw=FALSE,ax=TRUE,ErrLans=NA,ICESrect=FALSE,ICESlab=FALSE,ICESlabcex=.8,graf=FALSE,xpng=1200,ypng=800,ppng=15) {
  #if (!all(any(is.na(xlims)),any(is.na(ylims))))  stop("Si especifica limite de coordenadas debe hacerlo tanto en latitud y longitud")
  lan<-datlan.camp(camp,dns,redux=FALSE,incl2=TRUE,incl0=TRUE)
  if (any(!is.na(ErrLans))) lan<-filter(lan,lance %in% ErrLans)
  lannul<-lan[lan$validez==0,c("lance","longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  lan<-lan[lan$validez!=0,c("lance","longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  camp.name<-DBI::dbReadTable(ch1, paste0("CAMP",camp[1]))$IDENT
  DBI::dbDisconnect(ch1)
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    if (any(!is.na(xlims) | !is.na(ylims))) {mapporco(ICESrect=ICESrect,ICESlab=ICESlab,xlims=xlims,ylims=ylims,bw=bw,ax=ax)} else mapporco(ICESrect=ICESrect,ICESlab=ICESlab,ICESlabcex = ICESlabcex)}
  if (substr(dns,1,4)=="Cant" | dns=="Cnew" ) {
    if (any(!is.na(xlims)|!is.na(ylims))) {MapNort(ICESrect=ICESrect,ICESlab=ICESlab,xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapNort(ICESrect=ICESrect,ICESlab=ICESlab,ICESlabcex = ICESlabcex)
  }
  if (dns=="Arsa") {
    if (any(!is.na(xlims))) {MapArsa(ICESrect=ICESrect,ICESlab=ICESlab,xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapArsa(ICESrect=ICESrect,ICESlab=ICESlab,ICESlabcex = ICESlabcex)
    if (leg) {legend("topright",c("Valid","Null"),lty=1,col = c(2,1),inset=.02,bg="white")}
  }
  if (dns=="Medi") {
    if (any(!is.na(xlims))) {MapMedit(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapMedit()
  }
  if (dns=="Other") {
    longrank<-range(lan$longitud_l,lan$longitud_v,na.rm=T)
    if (max(lan$longitud_l,lan$longitud_v)<c(-10)) longrank<-c(longrank[1],c(-10))
    longrank<-c(floor(longrank[1]),ceiling(longrank[2]))
    latrank<-range(lan$latitud_l,lan$latitud_v,na.rm=T)
    latrank<-c(floor(latrank[1]),ceiling(latrank[2]))
    maps::map("worldHires",xlim=longrank,ylim=latrank,fill=T,col="grey")
  }
  box()
  if (ti) {title(camp.name,line=2)}
  segments(lan$longitud_l,lan$latitud_l,lan$longitud_v,lan$latitud_v,col=col,lwd=lwd)
  if (rumbo) points(lan$longitud_v,lan$latitud_v,pch=21,bg=2,col=1,cex=.8)
  if (incl0) segments(lannul$longitud_l,lannul$latitud_l,lannul$longitud_v,lannul$latitud_l,col=1,lwd=2)
  if (Nlans) text(latitud_v~longitud_v,lan,label=lan$lance,pos=1,cex=cexlans,font=2,offset=.05)
  if (Nlans & incl0) text(latitud_v~longitud_v,lannul,label=lannul$lance,pos=1,cex=cexlans,col=2,font=2)
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
}

