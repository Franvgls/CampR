#' Mapa del Golfo de Cádiz
#' 
#' Función auxiliar para sacar mapas de la campaña ARSA
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD) 
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ax Si T saca los ejes x e y
#' @param es Si T saca titulos y ejes en español
#' @param wmf Si T saca a fichero metafile Arsaconc.emf
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @seealso {\link{MapCant}} {\link{mapporco}}
#' @examples MapArsa()
#' @export
MapArsa<-function(lwdl=1,cuadr=F,cuadrMSFD=F,ICESrect=F,ax=T,wmf=F,es=T,places=T) {
  data(CampR)
  require(maps)
  asp<-diff(c(35.95,37.33))/(diff(c(-8,-5.55))*cos(mean(c(35.95,37.29))*pi/180))
  if (wmf) win.metafile(filename = "Arsaconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3)
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  map(Arsa.str,xlim=c(-8,-5.55),ylim=c(35.95,37.33),type="n",yaxs="i",xaxs="i")
  if (cuadr) {
    abline(h=seq(31,45,by=1/12),col=gray(.6),lwd=.6)
    abline(v=seq(-12,0,by=0.089),col=gray(.6),lwd=.6)
  }
  if (ICESrect) {
    abline(h=seq(31,45,by=.5),col=gray(.2),lwd=.6)
    abline(v=seq(-12,0,by=1),col=gray(.2),lwd=.6)
  }
  if (cuadrMSFD) {
    abline(h=seq(31,45,by=1/6),col=gray(.4),lwd=.5)
    abline(v=seq(-12,0,by=0.2174213),col=gray(.4),lwd=.5)
  }
  map(Arsa.map,add=T,fill=T,col=c(rep(NA,5),c("gray85","gray90")),lwd=lwdl)
  if (places) {
    points(c(-6.299667,-6.950833),c(36.53433,37.25833),pch=20)
    text(-6.950833,37.25833,"Huelva",cex=.85,font=2,pos=2)
    text(-6.299667,36.53433,"Cádiz",cex=.85,font=2,pos=3)
    text(-7.9,37.2,"PORTUGAL",cex=1,font=2,pos=4)
  }
  if (ax) {
    axis(2,at=35:38,labels=paste(35:38,"º",sep=""),las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8),tck=F,mgp=c(3,.5,0))
    rug(35:38,.01,side=2,lwd=lwdl,quiet=T)
    rug(seq(35,38,by=.5),.005,side=2,lwd=lwdl,quiet=T)
    axis(1,at=c(-8:-5),labels=paste(c(8:5),"º",sep=""),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8),tck=F,mgp=c(3,.5,0))
    rug(-8:-5,.01,side=1,lwd=lwdl,quiet=T)
    rug(seq(-8,-5,by=.5),.005,side=1,lwd=lwdl,quiet=T)
    axis(3,at=c(-8:-5),labels=paste(c(8:5),"º",sep=""),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8),tck=F,mgp=c(3,.5,0))
    rug(-8:-5,.01,side=3,lwd=lwdl,quiet=T)
    rug(seq(-8,-5,by=.5),.005,side=3,lwd=lwdl,quiet=T)
    axis(3,at=c(-15:-11),labels=paste(c(15:11),"º",sep=""),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8),tck=F,mgp=c(3,.5,0))
    rug(-15:-11,.01,side=3,lwd=1,quiet=T)
    rug(seq(-15,-11,by=.5),.005,side=3,lwd=lwdl,quiet=T)
    axis(4,at=35:38,labels=paste(31:34,"º",sep=""),las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.8),tck=F,mgp=c(3,.5,0))
    rug(35:38,.01,side=4,lwd=lwdl,quiet=T)
    rug(seq(35,38,by=.5),.005,side=4,lwd=lwdl,quiet=T)
  }
  box(lwd=lwdl)
  if (wmf) dev.off()
  if (wmf) par(mar=c(5, 4, 4, 2) + 0.1)
}
