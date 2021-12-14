#' A partir de los HH de DATRAS con CAMPtoHH comprueba que los lances son de día y avisa si son de noche
#'
#' Sirve para control de calidad y asegurarse que los datos de horas y fechas cuando las fechas son muy distintas canta.
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine, "Arsa" para el Golfo de Cádiz y "Medi" para MEDITS
#' @return Devuelve un data.frame con campaña, lance, recorrido, recorrido según la fórmula de Haversine, recorrido según la velocidad x el tiempo, velocidad, tiempo, rumbo, rumbo estimado según posiciones,velocidad calculada a partir de la distancia y el tiempo, y los porcentajes de errores de distancia, velocidad y rumbo.
#' @examples qcdistlan.camp("C14","Cant",pc.error=.01)
#' @examples qcdistlan.camp("216","Arsa",pc.error=.01)
#' @seealso {\link{MapLansGPS}}
#' @references \code{\link[suncalc]{getSunlightTimes}}
#' @family Control de calidad
#' @export
qclandata.camp<-function(camp,dns="Cant") {
  if (length(camp)>1) {
    hhlan<-CAMPtoHH(camp[1],dns,incl2 = T)
    for (i in camp[2:length(camp)]) hhlan<-rbind(hhlan,CAMPtoHH(i,dns,incl2 = T))
  }
  else hhlan<-CAMPtoHH(camp,dns,incl2 = T)
  hhlan$date<-as.Date(paste0(hhlan$Year,"-",hhlan$Month,"-",hhlan$Day))
  hhlan$time<-as.ITime(paste0(substr(hhlan$TimeShot,1,2),":",substr(hhlan$TimeShot,3,4)))
  hhlan$lon<-hhlan$ShootLong
  hhlan$lat<-hhlan$ShootLat
  hhlan<-as.data.frame(cbind(hhlan,suncalc::getSunlightTimes(data = hhlan[,c("date","lat","lon")],tz="GMT",keep=c("sunrise","sunset"))[,c("sunrise","sunset")]))
  hhlan$daynight<-ifelse(as.ITime(hhlan$sunrise)<hhlan$time & hhlan$time<as.ITime(hhlan$sunset),"D","N")
  hhlan$sunrise<-as.ITime(hhlan$sunrise)
  hhlan$sunset<-as.ITime(hhlan$sunset)
  filter(hhlan,daynight=="N")[,c("date","HaulNo","DayNight","daynight","sunrise","time","sunset")]
  }
