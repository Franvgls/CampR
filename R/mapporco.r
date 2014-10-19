#' Mapa del Banco de Porcupine sin referencias en tierra
#' 
#' Función auxiliar para sacar el mapa de la campaña Porcupine
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param label Si T saca las etiquetas de cada una de las cuadriculas numeradas consecutivamente por estratos 1A,1B,2B,3A,3B
#' @param colo Color de las etiquetas, por defecto rojas
#' @param ax Si T saca los ejes x e y
#' @param wmf Si T saca a fichero metafile porconc.emf
#' @param corners Si T coloca dos puntos rojos en los extremos nordeste y suroeste para ajustar mapas al PescaWin con ax=F
#' @return Saca en pantalla el mapa y es utilizada por otras funciones, si wmf=T lo saca a metafile para fondo del pescawin
#' @seealso MapNort {\link{MapNort.r}} MapCant {\link{MapCant.r}}
#' @export
mapporco<-function(lwdl=1,cuadr=F,ICESrect=F,label=F,colo=2,ax=T,wmf=F,corners=F) {
  require(maps)
  data(CampR)
  asp<-diff(c(50.5,54.5))/(diff(range(-15.5,-10.5))*cos(mean(50.5,54.5)*pi/180))
  if (wmf) win.metafile(filename = "porconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3)
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  map(Porc.map,xlim=c(-15.5,-10.5),ylim=c(50.5,54.5),type="n")
  nstrat<-length(which(!is.na(Porc.map$names)))
  nland<-length(Porc.map$names)-nstrat
  if (ax) {axis(2,las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8))
           #rug(51:54,-.01,side=2,pos=-15.53,lwd=2)
           axis(1,at=c(-15:-11),labels=c(15:11),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8))
           #rug(-15:-11,-.01,side=1,pos=50.459,lwd=2)
           axis(3,at=c(-15:-11),labels=c(15:11),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8))
           #rug(-15:-11,-.01,side=3,pos=54.541,lwd=2)
           axis(4,las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8))
           #rug(51:54,-.01,side=4,pos=-10.47,lwd=2)
  }
  if (cuadr) {
    abline(h=seq(50,55,by=1/12),col=gray(.6),lwd=.6)
    abline(v=seq(-18,-10,by=3/23),col=gray(.6),lwd=.6)
  }
  if (ICESrect) {
    abline(h=seq(50,55,by=.5),col=gray(.2),lwd=.6)
    abline(v=seq(-18,-10,by=1),col=gray(.2),lwd=.6)
  }
  map(Porc.map,add=T,fill=T,col=c(rep(NA,nstrat),rep("gray85",nland)),lwd=lwdl)
  box(lwd=lwdl)
  if (label) {
    if (!exists("Porc.grid")) {
      Porc.grid<-sacagrid()
    }
    text(Porc.grid$x,Porc.grid$y,labels=Porc.grid$pt,cex=.3,col=colo)
  }
  if (corners) points(c(-15.5,-10.5),c(50.5,54.5),pch=16,col=2)
  if (wmf) dev.off()
  if (wmf) par(mar=c(5, 4, 4, 2) + 0.1)
}
