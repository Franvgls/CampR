#' Mapa del Banco de Porcupine sin referencias en tierra
#'
#' Función auxiliar para sacar el mapa de la campaña Porcupine
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param latlonglin si T saca líneas longi y latitudinales muy claritas
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param label Si T saca las etiquetas de cada una de las cuadriculas numeradas consecutivamente por estratos 1A,1B,2B,3A,3B
#' @param colo Color de las etiquetas, por defecto rojas
#' @param es Textos en español, si F en inglés
#' @param leg incluye la leyenda con los colores/patrones de los estratos
#' @param FU Por defecto NA pero si se incluye un vector con la lista de las unidades funcionales las pinta (las disponibles en Porcupine sólo es FU16)
#' @param FUsLab Por defecto F, pero si T incluye una etiqueta con los nombres de las FUs seleccionadas en FUs
#' @param dens si mayor de 0 las superficies de los estratos tienen patrones de líneas
#' @param sectcol si T pone los sectores con color de fondo, en caso contrario lo deja en blanco, bueno para armap.tot
#' @param bw Si T mapa en blanco y negro respecto a tierra y puntos, en caso contrario en color. Para sacar el diseño de estratos de Porcupine se utiliza sectcol=TRUE y leg=TRUE
#' @param ax Si T saca los ejes x e y
#' @param wmf Si T saca a fichero metafile porconc.emf
#' @param corners Si T coloca dos puntos rojos en los extremos nordeste y suroeste para ajustar mapas al PescaWin con ax=F
#' @return Saca en pantalla el mapa y es utilizada por otras funciones, si wmf=TRUE lo saca a metafile para fondo del pescawin
#' @seealso {\link{MapNort}}, {\link{MapCant}}
#' @examples mapporco(xlims = c(-15.3,-8.10),corners = F,leg=T,sectcol=T,dens=10,es=F) para sustituir a maparea
#' @examples mapporco(corners = F,leg=F,sectcol=F,dens=0,es=F) # para sustituir a mapporco
#' @family mapas base
#' @family Porcupine
#' @export
mapporco<-function(xlims=c(-15.5,-10.5),ylims=c(50.5,54.5),lwdl=1,latlonglin=TRUE,cuadr=FALSE,ICESrect=FALSE,ICESlab=FALSE,
                   ICESlabcex=.7,label=FALSE,colo=2,dens=0,bw=F,places=TRUE,es=TRUE,ax=TRUE,wmf=FALSE,corners=FALSE,
                   leg=FALSE,sectcol=FALSE,FU=NA,FUsLab=FALSE) {
  asp<-diff(c(50.5,54.5))/(diff(range(-15.5,-10.5))*cos(mean(50.5,54.5)*pi/180))
  if (any(is.na(xlims))) {xlims<-c(-15.5,-10.5)}
  if (any(is.na(ylims))) {ylims<-c(50.5,54.5)}
  if (wmf) {win.metafile(filename = "porconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)}
  if (!wmf) {par(mar=c(2,2.5,2, 2.5) + 0.3, mgp=c(2,.5,0))}
  if (!ax) {par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))}
  library(mapdata)
  maps::map("worldHires",c("ireland","UK:Northern Ireland"),ylim=c(50.5,54.5),xlim=c(-15.5,-8.2),
            fill=TRUE,col=ifelse(bw,gray(.7),"saddlebrown"),type="n")
  maps::map(Porc.map,xlim=xlims,ylim=ylims,type="n")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=ifelse(bw,"white","lightblue1"))
  nstrat<-length(which(!is.na(Porc.map$names)))
  nland<-length(Porc.map$names)-nstrat
  if (ax) {
     degs = seq(floor(xlims[1]),ceiling(xlims[2]),ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(floor(ylims[1]),ceiling(ylims[2]),ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (latlonglin) abline(v=c(-20:2),h=c(40:60),lty=3,col=gray(.7))
  if (cuadr) {
    abline(h=seq(50,55,by=1/12),col=gray(.6),lwd=.6)
    abline(v=seq(-18,-10,by=3/23),col=gray(.6),lwd=.6)
  }
  if (ICESrect) {
    abline(h=seq(50,55,by=.5),col=gray(.2),lwd=.6)
    abline(v=seq(-18,-10,by=1),col=gray(.2),lwd=.6)
  }
  if (sectcol) colrs=c("Steelblue2","Steelblue2","Steelblue","blue4","green","darkgreen",gray(.7))
  else {
    if (bw) {colrs=c(rep(ifelse(bw,"white","lightblue"),6),gray(.7))} else {colrs=c(rep("lightblue1",6),"antiquewhite")}
  }
  if (ICESlab) text(c(stat_y+.215)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
  maps::map("worldHires",c("ireland","UK:Northern Ireland"),ylim=c(50.5,54.5),xlim=c(-15.5,-8.2),
            fill=TRUE,col=ifelse(bw,gray(.7),"bisque"),add = T)
  maps::map(Porc.map,add=TRUE,fill=TRUE,col=c(rep(NA,nstrat-1),ifelse(bw,"gray85","bisque")),lwd=lwdl)
  if (any(!is.na(FU))) {
    if (any(stringr::str_detect(FU,"FU16"))) {polygon(FU16[,c("long")],FU16[,c("lat")],density = NULL,col=NULL,border="red",lwd=3); if (FUsLab) text(c(lat+.10)~c(long-.55),filter(as.data.frame(FU16),lat==max(FU16[,"lat"])),lab="FU16",cex=.8,font=2,pos=4,col=2)}
  }
  if (places) {
    points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
    text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
    text(-(8.95),(52.2),label=ifelse(es,"IRLANDA","IRELAND"),cex=1.3,font=2)
    }
  box(lwd=lwdl)
  maps::map(Porc.map,add=TRUE,fill=TRUE,col=colrs)
  if (dens>0) {
    polygon(maps::map(Porc.map,"1Aa",plot=FALSE)$x,maps::map(Porc.map,"1Aa",plot=FALSE)$y,density=dens)
    polygon(maps::map(Porc.map,"1Ab",plot=FALSE)$x,maps::map(Porc.map,"1Ab",plot=FALSE)$y,density=dens)
    polygon(maps::map(Porc.map,"1B",plot=FALSE)$x,maps::map(Porc.map,"1B",plot=FALSE)$y,density=dens,angle=0)
    polygon(maps::map(Porc.map,"2B",plot=FALSE)$x,maps::map(Porc.map,"2B",plot=FALSE)$y,density=dens,angle=0)
    polygon(maps::map(Porc.map,"2C",plot=FALSE)$x,maps::map(Porc.map,"2C",plot=FALSE)$y,density=dens,angle=135)
    polygon(maps::map(Porc.map,"1C",plot=FALSE)$x,maps::map(Porc.map,"1C",plot=FALSE)$y,density=dens,angle=135)
  }
  if (any(!is.na(FU))) {
    if (any(stringr::str_detect(FU,"FU16"))) {polygon(FU16[,c("long")],FU16[,c("lat")],density = 20,col="chartreuse",border="red",lwd=3); if (FUsLab) text(c(lat+.10)~c(long-.55),filter(as.data.frame(FU16),lat==max(FU16[,"lat"])),lab="FU16",cex=.8,font=2,pos=4,col=2)}
  }
  if (leg) {
    rect(-13.2,50.7,-10.3,51.3,col="white")
    rect(-13.05,51.05,-12.8,51.2,col=colrs[1])
    rect(-12.8,51.05,-12.55,51.2,col=colrs[3])
    rect(-12.55,51.05,-12.30,51.2,col=colrs[4])
    rect(-12.9,50.8,-12.65,50.95,col=colrs[5])
    rect(-12.65,50.8,-12.4,50.95,col=colrs[6])
    if (dens>0) {
      rect(-13.05,51.05,-12.8,51.2,col=1,density=15)
      rect(-12.8,51.05,-12.55,51.2,col=1,density=15,angle=0)
      rect(-12.55,51.05,-12.30,51.2,col=1,density=15,angle=135)
      rect(-12.9,50.8,-12.65,50.95,col=1,density=15,angle=0)
      rect(-12.65,50.8,-12.4,50.95,col=1,density=15,angle=135)
    }
    text(-12.3,(51.2+51.05)/2,label=ifelse(es,"Sector 1 (norte) E, F y G","Sector 1: E, F & G"),pos=4,cex=.8,font=2)
    text(-12.3,(50.8+50.95)/2,label=ifelse(es,"Sector 2 (sur) F & G","Sector 2: F & G"),pos=4,cex=.8,font=2)
  }
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
