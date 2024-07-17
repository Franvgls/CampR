#' Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP
#'
#' Sirve para control de calidad y asegurarse que los datos de distancias, posiciones, y las horas de largada y virada son correctos y no salen del estándar de largar y virar de día
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine, "Arsa" para el Golfo de Cádiz y "Medi" para MEDITS
#' @param plots si T abre una pantalla con cuatro gráficos de comparacion entre camp y DATRAS
#' @return Devuelve un gráfico con cuatro plots puertas~prof, calones~prof, vertical~prof y lances en área
#' @examples qclandatr.camp("N23","Cant")
#' @examples qclandatr.camp("216","Arsa")
#' @seealso {\link{MapLansGPS}}
#' @family Control de calidad
#' @export
qclandatr.camp<-function(camp,dns) {
  par(mfrow=c(2,2))
  datcamp<-datlan.camp(camp,dns,redux=F,incl2=T,incl0 = F)
  if (dns=="Cant") {
    MapNort(places=T)
    datdatr<-icesDatras::getDATRAS("HH","SP-NORTH",camptoyear(camp),4)
    legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bg="white")
  }
  if (dns=="Porc") {
    mapporco()
    datdatr<-icesDatras::getDATRAS("HH","SP-PORC",camptoyear(camp),3)
    legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bg="white")
  }
  if (dns=="ARSA") {
    MapArsa()
    datdatr<-icesDatras::getDATRAS("HH","SP-ARSA",camptoyear(camp),ifelse(substr(camp,1,1)==1,2,4))
    legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bg="white")
  }
  points(latitud_l~longitud_l,datcamp,pch=21,bg="yellow")
  points(ShootLat~ShootLong,datdatr,pch=21,cex=1.1,col="red",lwd=2)
  legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bty="n",bg="white")
  plot(dista_p~prof_l,datcamp,pch=21,bg="yellow",ylim=c(0,hablar::max_(datcamp$dista_p)*1.1),ylab="Door spread (m)",xlab=c("Depth (m)"))
  points(DoorSpread~Depth,datdatr,pch=21,col="red",bg=NA,cex=1.2,lwd=2)
  title("Door spread vs. depth")
  legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),bty="n",pt.lwd = c(1,2),inset = c(.02),bg="white")
  plot(abert_h~prof_l,datcamp,pch=21,bg="yellow",ylim=c(0,hablar::max_(datcamp$abert_h)*1.1),ylab=c("Wing spread (m)"),xlab=("Depth (m)"))
  points(WingSpread~Depth,datdatr,pch=21,cex=1.2,col="red",lwd=2)
  legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bg="white",bty="n")
  title("Wing spread vs. depth")
  plot(abert_v~prof_l,datcamp,pch=21,bg="yellow",ylim=c(0,hablar::max_(datcamp$abert_v)*1.01),ylab=c("Vertical opening (m)"),xlab=c("Depth (m)"))
  points(Netopening~Depth,datdatr,pch=21,col="red",cex=1.2,lwd=2)
  legend("bottomright",c("Camp hauls","DATRAS hauls"),pch=21,pt.cex = c(1,1.2),pt.bg=c("yellow",NA),col=c(1,"red"),pt.lwd = c(1,2),inset = c(.02),bg="white",bty="n")
  title("Net opening vs. depth")
}
