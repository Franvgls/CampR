#' Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP
#'
#' Sirve para control de calidad y asegurarse que los datos de distancias, posiciones, y las horas de largada y virada son correctos y no salen del estándar de largar y virar de día
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine, "Arsa" para el Golfo de Cádiz y "Medi" para MEDITS
#' @param plots si T abre una pantalla con cuatro gráficos de comparacion entre camp y DATRAS
#' @return Devuelve un gráfico con cuatro plots puertas~prof, calones~prof, vertical~prof y lances en área
#' @examples qclansdatr.camp("N23","Cant")
#' @examples qclansdatr.camp("216","Arsa")
#' @seealso {\link{MapLansGPS}}
#' @references 
#' @family Control de calidad
#' @export
qclandatr.camp<-function(camp,dns) {
  par(mfrow=c(2,2))
  datcamp<-datlan.camp(camp,dns,redux=F,incl2=T,incl0 = F)
  if (dns=="Cant") {
    MapNort(places=T)     
    datdatr<-icesDatras::getDATRAS("HH","SP-NORTH",camptoyear(camp),4)
  }
  if (dns=="Porc") {
    mapporco()
    datdatr<-icesDatras::getDATRAS("HH","SP-PORC",camptoyear(camp),3)
  }
    if (dns=="ARSA") {
      MapArsa()
  plot(dista_p~prof_l,datcamp,pch=21,bg="blue",ylim=c(0,hablar::max_(datcamp$dista_p)*1.1))
  points(DoorSpread~Depth,datdatr,pch=21,cex=1.2,lwd=2)
  plot(abert_h~prof_l,datcamp,pch=21,bg="blue",ylim=c(0,hablar::max_(datcamp$abert_h)*1.1))
  points(WingSpread~Depth,datdatr,pch=21,cex=1.2,lwd=2)
  plot(abert_v~prof_l,datcamp,pch=21,bg="blue",ylim=c(0,hablar::max_(datcamp$abert_v)*1.01))
  points(Netopening~Depth,datdatr,pch=21,cex=1.2,lwd=2)
  points(latitud_l~longitud_l,datcamp,pch=21,bg="blue")
  points(ShootLat~ShootLong,datdatr,pch=21,cex=1.2,lwd=2)
}