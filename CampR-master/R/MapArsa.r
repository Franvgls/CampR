#' Mapa del Golfo de Cádiz
#'
#' Función auxiliar para sacar mapas de la campaña ARSA
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrMSFD Si T dibuja cuadrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD)
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param FU Por defecto NA pero si se incluye un vector con la lista de las unidades funcionales las pinta (las disponibles en MapNort son FU31, FU25, FU26)
#' @param FUsLab Por defecto F, pero si T incluye una etiqueta con los nombres de las FUs seleccionadas en FUs
#' @param leg si T incluye una legenda con los estratos de profundidad y los estratos se colorean respectivamente
#' @param ax Si T saca los ejes x e y
#' @param bw si T mapa con tierra en gris, si F tierra en color
#' @param es Si T saca titulos y ejes en español
#' @param wmf Si T saca a fichero metafile Arsaconc.emf
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @seealso {\link{MapCant}}, {\link{mapporco}}
#' @examples MapArsa()
#' @family mapas base
#' @family ARSA
#' @export
MapArsa<-function(xlims=c(-8.149,-5.52),ylims=c(35.95,37.3337),lwdl=1,leg=F,cuadr=FALSE,cuadrMSFD=FALSE,ICESrect=FALSE,
                  FU=NA,ColFU="chartreuse",FUsLab=FALSE,ax=TRUE,bw=FALSE,wmf=FALSE,es=TRUE,places=TRUE) {
  asp<-diff(c(35.95,37.33))/(diff(c(-8,-5.55))*cos(mean(c(35.95,37.33))*pi/180))
  if (wmf) win.metafile(filename = "Arsaconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  maps::map(Arsa.str,xlim=xlims,ylim=ylims,type="n",xaxs="i",yaxs="i")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=ifelse(bw,"white","lightblue1"))
  if (cuadr) abline(h=seq(31,45,by=1/12),v=seq(-12,0,by=0.089),col=gray(.6),lwd=.6)
  if (ICESrect) abline(h=seq(36,37,by=.5),v=seq(-8,-5,by=1),col=gray(.2),lwd=.6)
  if (cuadrMSFD) abline(h=seq(31,45,by=1/6),v=seq(-12,0,by=0.2174213),col=gray(.4),lwd=.5)
  if (any(!is.na(FU))) {
    if (any(stringr::str_detect(FU,"FU29"))) {polygon(FU29[,c("long")],FU29[,c("lat")],density = dens,col=ColFU,border="red",lwd=3); if (FUsLab) text(c(lat+.10)~c(long-.10),filter(as.data.frame(FU29),long==min(FU29[,"long"])),lab="FU29",cex=.8,font=2,pos=1,col=2)}
    if (any(stringr::str_detect(FU,"FU30"))) {polygon(FU30[,c("long")],FU30[,c("lat")],density = dens,col=ColFU,border="red",lwd=3); if (FUsLab) text(c(lat+.10)~c(long-.10),filter(as.data.frame(FU30),long==min(FU30[,"long"])),lab="FU30",cex=.8,font=2,pos=1,col=2)}
  }
  maps::map(Arsa.map,add=TRUE,fill=TRUE,col=c(rep(NA,5),ifelse(bw,"light gray","wheat")),lwd=lwdl,xaxs="i",yaxs="i")
  maps::map(Arsa.map,add=TRUE,fill=TRUE,col=c(rep(NA,6),ifelse(bw,"light gray","wheat")),lwd=lwdl,xaxs="i",yaxs="i")
  if (leg) {
    if (!bw) {
      maps::map(Arsa.map,Arsa.map$names[grep("StrA",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue1")
      maps::map(Arsa.map,Arsa.map$names[grep("StrB",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue2")
      maps::map(Arsa.map,Arsa.map$names[grep("StrC",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue3")
      maps::map(Arsa.map,Arsa.map$names[grep("StrD",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="blue")
      maps::map(Arsa.map,Arsa.map$names[grep("StrE",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="darkblue")
      legend("topright",c("A: 15-30 m","B: 30-70 m","C: 71-200 m","D: 201-500 m","E: 501-800 m"),fill=c("lightblue1","lightblue2","lightblue3","blue","darkblue"),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
    }
    else {
      maps::map(Arsa.map,Arsa.map$names[grep("StrA",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.9))
      maps::map(Arsa.map,Arsa.map$names[grep("StrB",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.8))
      maps::map(Arsa.map,Arsa.map$names[grep("StrC",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.6))
      maps::map(Arsa.map,Arsa.map$names[grep("StrD",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.5))
      maps::map(Arsa.map,Arsa.map$names[grep("StrE",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.4))
      legend("topright",c("A: 15-30 m","B: 30-70 m","C: 71-200 m","D: 201-500 m","E: 501-800 m"),fill=c(gray(.9),gray(.8),gray(.6),gray(.5),gray(.4)),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
    }
  }
    if (places) {
    points(c(-6.299667,-6.950833),c(36.53433,37.25833),pch=20)
    text(-6.950833,37.25833,"Huelva",cex=.85,font=2,pos=2)
    text(-6.299667,36.53433,"Cádiz",cex=.85,font=2,pos=3)
    text(-8,37.25,"PORTUGAL",cex=1,font=2,pos=4)
  }
  if (ax) {
     degs = seq(-8,-5,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(35,38,ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  box(lwd=lwdl)
  if (wmf) dev.off()
  if (wmf) par(mar=c(5, 4, 4, 2) + 0.1)
 }
