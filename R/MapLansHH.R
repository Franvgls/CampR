#' Gráfico con los lances en segmentos
#'
#' gráfica todos los lances de una campaña con las distancias recorridas en cada uno de ellos
#' @param fic Nombre del fichero HH con los datos a comprobar en formato HH de DATRAS, con su path o sin el si se está en el mismo directorio
#' @param dns Elige el supuesto origen de los datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" para otras campañas IBTS "Other"
#' @param nrows Número de filas del fichero que se quieren obtener, por defecto NA y coge todo el fichero
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña
#' @param col Define el color de los segmentos
#' @param lwd Define el ancho de los segmentos
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param Nlans si T incluye los números de lance por encima de los segmentos
#' @param Bearing si T marca el rumbo hacia el punto final con un punto en el final del lance.
#' @param ax Si T saca los ejes x e y
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux
#' @seealso {\link{MapLansGPS}}, {\link{armap.camp}}
#' @examples MapLansHH(fic=icesDatras::getDATRAS("HH","SP-NORTH",2001,4),dns="Other")
#' @examples MapLansHH(fic=icesDatras::getDATRAS("HH","IE-IGFS",2019,4),dns="Other")
#' @family mapas
#' @family PescaWin
#' @export
MapLansHH<-function(fic,dns="Cant",nurows=NA,incl0=FALSE,incl2=TRUE,xlims=NA,ylims=NA,col=2,lwd=2,places=TRUE,Nlans=FALSE,bearing=FALSE,es=T,bw=FALSE,ax=T) {
  namesHH<-c("RecordType","Quarter","Country","Ship","Gear","SweepLngt","GearExp","DoorType","StNo","HaulNo","Year","Month","Day",
             "TimeShot","Stratum","HaulDur","DayNight","ShootLat","ShootLong","HaulLat","HaulLong","StatRec","Depth","HaulVal",
             "HydroStNo","StdSpecRecCode","BycSpecRecCode","DataType","Netopening","Rigging","Tickler","Distance","Warplngt",
             "Warpdia","WarpDen","DoorSurfuce","DoorWgt","DoorSpread","WingSpread","Buoyancy","KiteDim","WgtGroundRope","TowDir",
             "GroundSpeed","SpeedWater","SurCurDir","SurCurSpeed","BotCurDir","BotCurSpeed","WindDir","WindSpeed","SwellDir",
             "SwellHeight","SurTemp","BotTemp","SurSal","BotSal","ThermoCline","ThClineDepth")
  if (!all(any(is.na(xlims)),any(is.na(ylims))))  stop("Si especifica limite de coordenadas debe hacerlo tanto en latitud y longitud")
  if (is.numeric(nurows)) lan<-fread(fic,nrows=nurows)
  else lan<-data.table(fic)
  if (names(lan)[1]=="V1") {names(lan)<-namesHH}
  if (incl0) {lannul<-dplyr::filter(lan,HaulVal=="I")}
  if (incl2) {lanesp<-dplyr::filter(lan,HaulVal=="A")}
  lan<-dplyr::filter(lan,HaulVal!="I" & HaulVal!="A")
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
  if (tolower(dns)=="other") {
    longrank<-range(lan$ShootLong,lan$HaulLong,na.rm=T)
    if (max(lan$ShootLong,lan$HaulLong)<c(-10)) longrank<-c(longrank[1],c(-10))
    longrank<-c(floor(longrank[1]),ceiling(longrank[2]))
    latrank<-range(lan$Shootlat,lan$HaulLat,na.rm=T)
    latrank<-c(floor(latrank[1]),ceiling(latrank[2]))
    NeAtlIBTS::IBTSNeAtl_map(nl=latrank[2]+.5,sl=latrank[1]-.5,xlims = c(longrank[1]-.5,
                  longrank[2]+.5),leg=F,dens=0,load=F,ICESdiv=F)
  }
  segments(lan$ShootLong,lan$ShootLat,lan$HaulLong,lan$HaulLat,col=1,lwd=lwd)
  if (bearing) points(HaulLat~HaulLong,lan,pch=23,bg="grey90",cex=.8)
  if (incl0) segments(lannul$ShootLong,lannul$ShootLat,lannul$HaulLong,lannul$HaulLat,col=2,lwd=lwd)
  if (incl2) segments(lanesp$ShootLong,lanesp$ShootLat,lanesp$HaulLong,lanesp$HaulLat,col=3,lwd=lwd)
  if (any(exists("lannul") |exists("lanesp"))) {legend("bottom", legend = c("Standard","Extra", "Null"), inset=c(0,0.01),
                                                           bty = "n", cex = .8,lty=1,lwd=2,col=c(1,3,2),horiz=T)}
  if (Nlans) text(HaulLat~HaulLong,lan,label=lan$HaulNo,pos=1,cex=.7,font=2)
  if (Nlans & incl0) text(HaulLat~HaulLong,lannul,label=lannul$HaulNo,pos=1,cex=.7,col=2)
  lan
}
