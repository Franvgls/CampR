#' Mapa del Banco de Porcupine sin referencias en tierra
#'
#' Función auxiliar para sacar el mapa de la campaña Porcupine
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param label Si T saca las etiquetas de cada una de las cuadriculas numeradas consecutivamente por estratos 1A,1B,2B,3A,3B
#' @param colo Color de las etiquetas, por defecto rojas
#' @param ax Si T saca los ejes x e y
#' @param wmf Si T saca a fichero metafile porconc.emf
#' @param corners Si T coloca dos puntos rojos en los extremos nordeste y suroeste para ajustar mapas al PescaWin con ax=F
#' @return Saca en pantalla el mapa y es utilizada por otras funciones, si wmf=TRUE lo saca a metafile para fondo del pescawin
#' @seealso {\link{MapNort}}, {\link{MapCant}}
#' @family mapas base
#' @family Porcupine
#' @export
mapporco<-function(xlims=c(-15.5,-10.5),ylims=c(50.5,54.5),lwdl=1,cuadr=FALSE,ICESrect=FALSE,ICESlab=FALSE,ICESlabcex=.7,label=FALSE,colo=2,ax=TRUE,wmf=FALSE,corners=FALSE) {
  asp<-diff(c(50.5,54.5))/(diff(range(-15.5,-10.5))*cos(mean(50.5,54.5)*pi/180))
  if (wmf) win.metafile(filename = "porconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3, mgp=c(2,.5,0))
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  maps::map(Porc.map,xlim=xlims,ylim=ylims,type="n")
  nstrat<-length(which(!is.na(Porc.map$names)))
  nland<-length(Porc.map$names)-nstrat
  if (ax) {
     degs = seq(-15,-11,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(51,54,ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (cuadr) {
    abline(h=seq(50,55,by=1/12),col=gray(.6),lwd=.6)
    abline(v=seq(-18,-10,by=3/23),col=gray(.6),lwd=.6)
  }
  if (ICESrect) {
    abline(h=seq(50,55,by=.5),col=gray(.2),lwd=.6)
    abline(v=seq(-18,-10,by=1),col=gray(.2),lwd=.6)
  }
  if (ICESlab) text(c(stat_y+.215)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
  maps::map(Porc.map,add=TRUE,fill=TRUE,col=c(rep(NA,nstrat),rep("gray85",nland)),lwd=lwdl)
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
