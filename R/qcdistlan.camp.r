#' Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP
#'
#' Sirve para control de calidad y asegurarse que los datos de distancias, posiciones, y las horas de largada y virada son correctos y no salen del estándar de largar y virar de día
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine, "Arsa" para el Golfo de Cádiz y "Medi" para MEDITS
#' @param todos Por defecto F. Si T lista todos los lances con valores, si no sólo los que pc.error>error
#' @param pc.error porcentaje de error aceptable para no mostrar los lances como erróneos
#' @param error.rb si F no tiene en cuenta los errores de rumbo que tampoco tiene mucho peso
#' @param plots si T abre una pantalla con tres gráficos de los errores detectados en error.dist, error.vel y error.rumb
#' @return Devuelve un data.frame con campaña, lance, recorrido, recorrido según la fórmula de Haversine, recorrido según la velocidad x el tiempo, velocidad, tiempo, rumbo, rumbo estimado según posiciones,velocidad calculada a partir de la distancia y el tiempo, y los porcentajes de errores de distancia, velocidad y rumbo, horas de amanecer, salida del sol, mediodía, puesta del sol y noche.
#' @examples qcdistlan.camp("C14","Cant",pc.error=.01)
#' @examples qcdistlan.camp("216","Arsa",pc.error=.01)
#' @seealso {\link{MapLansGPS}}
#' @references distHaversine function gives the haversine calculation of distance between two geographic points \code{\link[geosphere]{distHaversine}}
#' @family Control de calidad
#' @export
qcdistlan.camp<-function(camp,dns="Cant",todos=FALSE,pc.error=2,error.rb=TRUE,plots=TRUE) {
  while (!is.null(dev.list()))  dev.off()
  windows()
  dumblan<-datlan.camp(camp,dns,redux=FALSE)
  dumblan$mins<-round(dumblan$haul.mins*dumblan$weight.time,1)
  dumblan$dist.vel<-round(c(dumblan$weight.time*dumblan$haul.mins)/60*dumblan$velocidad*1852,0)
  dumblan$dist.hf<-round(geosphere::distHaversine(dumblan[,c("longitud_l","latitud_l")],dumblan[,c("longitud_v","latitud_v")]))
  dumblan$vel.dist<-round((dumblan$dist.hf/1852)/(dumblan$weight.time*dumblan$haul.mins/60),1)
  dumblan$error.vel<-round((dumblan$dist.vel-dumblan$recorrido)*100/dumblan$recorrido,2)
  dumblan$error.dist<-round((dumblan$dist.hf-dumblan$recorrido)*100/dumblan$recorrido,2)
  dumblan$rumb<-round(geosphere::bearingRhumb(dumblan[,c("longitud_l","latitud_l")],dumblan[c("longitud_v","latitud_v")]),1)
  dumblan$error.rumb<-round(dumblan$rumb-dumblan$rumbo)
  dumblan$date<-as.Date(paste0(dumblan$year,"-",month(dumblan$fecha),"-",lubridate::day(dumblan$fecha)))
  dumblan$time_l<-as.ITime(paste0(substr(dumblan$hora_l,1,2),":",substr(dumblan$hora_l,4,5)))
  dumblan$time_v<-as.ITime(paste0(substr(dumblan$hora_v,1,2),":",substr(dumblan$hora_v,4,5)))
  dumblan$lon<-dumblan$longitud_l
  dumblan$lat<-dumblan$latitud_l
  dumblan<-as.data.frame(cbind(dumblan,suncalc::getSunlightTimes(data = dumblan[,c("date","lat","lon")],tz="GMT",keep=c("dawn","sunrise","solarNoon"))[,c("dawn","sunrise","solarNoon")]))
  dumblan$lon<-dumblan$longitud_v
  dumblan$lat<-dumblan$latitud_v
  dumblan<-as.data.frame(cbind(dumblan,suncalc::getSunlightTimes(data = dumblan[,c("date","lat","lon")],tz="GMT",keep=c("sunset","dusk"))[,c("sunset","dusk")]))
  dumblan$daynight<-ifelse(as.ITime(dumblan$sunrise)<dumblan$time_l & dumblan$time_l<as.ITime(dumblan$sunset),"D","N")
  dumblan$dawn<-as.ITime(dumblan$dawn)
  dumblan$sunrise<-as.ITime(dumblan$sunrise)
  dumblan$solarNoon<-as.ITime(dumblan$solarNoon)
  dumblan$sunset<-as.ITime(dumblan$sunset)
  dumblan$dusk<-as.ITime(dumblan$dusk)
  dumblan$dayhour<-NA
  for (i in 1:nrow(dumblan)) { if (dumblan$time_l[i]<dumblan$dawn[i] | dumblan$time_v[i]>dumblan$dusk[i]) dumblan$dayhour[i]<-"N" }
  for (i in 1:nrow(dumblan)) { if (dumblan$time_l[i]>dumblan$dawn[i] & dumblan$time_l[i]<dumblan$sunrise[i]) dumblan$dayhour[i]<-"S" }
  for (i in 1:nrow(dumblan)) { if (dumblan$time_l[i]>dumblan$sunrise[i] & dumblan$time_l[i]<dumblan$solarNoon[i]) dumblan$dayhour[i]<-"M" }
  for (i in 1:nrow(dumblan)) { if (dumblan$time_l[i]>dumblan$solarNoon[i] & dumblan$time_l[i]<dumblan$sunset[i]) dumblan$dayhour[i]<-"T" }
  for (i in 1:nrow(dumblan)) { if (dumblan$time_l[i]>dumblan$sunset[i] & dumblan$time_v[i]<dumblan$dusk[i]) dumblan$dayhour[i]<-"A" }
  # if (out.dat) return(dumblan)
  # else return(filter(dumblan,daynight=="N")[,c("date","lance","daynight","sunrise","time_l","sunset","time_v")])
  dumblan[abs(dumblan$error.dist)>pc.error | abs(dumblan$error.vel)>pc.error | abs(dumblan$error.rumb)>pc.error,c("camp","lance","recorrido","dist.hf","vel.dist","velocidad","error.dist","error.vel","error.rumb")]
  op<-par(no.readonly = T)
  if (plots) {
    temp<-dumblan[order(dumblan$camp,dumblan$lance),c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","vel.dist","error.dist","error.vel","rumbo","error.rumb")]
    par(mfrow=c(1,3),oma=c(0,0,2,0))
    ylims<-hablar::max_(abs(temp$error.dist))*1.1
    plot(error.dist~lance,temp,cex=sqrt(abs(error.dist)),pch=21,
         bg=if_else(error.dist<0,"red","blue"),type="o",ylim=c(-ylims,ylims))
    mtext(paste("Campaña",camp),outer =T,cex=1.1,font=2)
    abline(h=c(-1,0,1),lty=c(3,2,3),lwd=c(.5,1,.5))
    title(main="Error distancia-puntos")
    ylims<-hablar::max_(abs(temp$error.vel))*1.1
    plot(error.vel~lance,temp,cex=sqrt(abs(error.vel)),pch=21,bg=if_else(error.vel<0,"red","blue"),type="o",ylim=c(-ylims,ylims))
    abline(h=c(-1,0,1),lty=c(3,2,3),lwd=c(.5,1,.5))
    title(main="Error distancia-velocidad")
    ylims<-hablar::max_(abs(temp$error.rumb))*1.1
    plot(error.rumb~lance,temp,cex=sqrt(abs(error.rumb)),pch=21,bg=if_else(error.rumb<0,"red","blue"),type="o",ylim=c(-ylims,ylims))
    abline(h=c(-1,0,1),lty=c(3,2,3),lwd=c(.5,1,.5))
    title(main="Error rumbo puntos")
  }
  par(op)
  if (length(unique(year(dumblan$fecha)))>1) print(paste("Detectados lances en varios años: ",paste(unique(dumblan$year),collapse = ", ")))
  if (todos & error.rb) return(dumblan[order(dumblan$camp,dumblan$lance),c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","vel.dist","error.dist","error.vel","rumbo","error.rumb","sunrise","time_l","sunset","time_v","dusk","daynight")])
  if (!todos & error.rb) {lt<-list(lances=(dumblan[abs(dumblan$error.dist)>pc.error | abs(dumblan$error.vel)>pc.error*3 | abs(dumblan$error.rumb)>pc.error*3,
                       c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","rumbo","rumb","vel.dist","error.dist","error.vel","error.rumb")]),
                          daynight=filter(dumblan,daynight=="N")[,c("date","lance","daynight","sunrise","time_l","sunset","time_v")])
                          return(lt)}
  if (!error.rb) return(dumblan[abs(dumblan$error.dist)>pc.error | abs(dumblan$error.vel)>pc.error*3,
                               c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","vel.dist","error.dist","error.vel")])
  }
