#' Mapa del Cantábrico y Galicia
#'
#' Función auxiliar para sacar el mapa de la campaña Demersales
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrcol Color para los recuadros de muestreo
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD)
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param icesrectcol Color para los rectángulos ICES
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param leg Si T saca una leyenda con los estratos batimétricos que salen de color o con grises en bw
#' @param bw si T mapa con tierra en gris, si F tierra en color
#' @param dens si mayor de 0 las superficies de los estratos tienen patrones de líneas
#' @param es si T saca textos en español, si F en inglés
#' @param ax Si T saca los ejes x e y
#' @param strat Si marca los sectores geográficos (los batimetricos salen con las líneas correspondientes, y en colores con leg=T)
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @param country si T saca el país
#' @param xlims Define los limites longitudinales del mapa, por defecto -10.25 y -1.4 oeste
#' @param ylims Define los limites longitudinales del mapa, por defecto 41.82 y 44.48 norte
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @seealso {\link{MapCant}}, {\link{mapporco}}
#' @family mapas base
#' @family Galicia Cantabrico
#' @export
MapNort<- function(lwdl=.5,cuadr=FALSE,cuadrcol=gray(.4),cuadrMSFD=FALSE,ICESrect=FALSE,ICESrectcol=gray(.2),ICESlab=FALSE,ICESlabcex=.7,leg=F,bw=TRUE,es=FALSE,ax=TRUE,strat=FALSE,places=FALSE,country=F,xlims=c(-10.25,-1.4),ylims=c(41.82,44.48)) {
  maps::map(Nort.str,xlim=xlims,ylim=ylims,type="n")
  if (ax) {
     degs = seq(-10,-2,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(42,44,ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (cuadr) {
    abline(h=seq(41,45,by=1/12),col=cuadrcol,lwd=.5)
    abline(v=seq(-12,0,by=3/26),col=cuadrcol,lwd=.5)
  }
  if (cuadrMSFD) {
    abline(h=seq(31,45,by=1/6),col=gray(.4),lwd=.5)
    abline(v=seq(-12,0,by=0.2174213),col=gray(.4),lwd=.5)
  }
  if (ICESlab) text(c(stat_y+.22)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
  if (ICESrect) {
    abline(h=seq(41,45,by=.5),col=ICESrectcol,lwd=2)
    abline(v=seq(-12,-1),col=ICESrectcol,lwd=2)
  }
  if (strat) {
    abline(h=43,v=c(-7.66,-6,-3.5),lty=1,col=gray(.0),lwd=2)
    text(c(-10,-9.7,-6.83,-4.75,-2.7),c(42.4,44.3,44.3,44.3,44.3),c("MF","FE","EP","PA","AB"),font=2)
  }
  maps::map(Nort.str,add=TRUE,fill=TRUE,col=c(rep(NA,16),ifelse(bw,"light gray","wheat")),lwd=lwdl)
  maps::map(Nort.map,Nort.map$names[1:16],add=TRUE,col=c("gray"),lwd=lwdl)
  maps::map(Nort.map,Nort.map$names[17],add=TRUE,col=c("black"),lwd=lwdl)
  box(lwd=2)
  # if (cuadr) {
  #   abline(h=seq(41,45,by=1/12),col=gray(.4),lwd=.5)
  #   abline(v=seq(-12,0,by=3/26),col=gray(.4),lwd=.5)
  # }
  # if (cuadrMSFD) {
  #   abline(h=seq(31,45,by=1/6),col=gray(.4),lwd=.5)
  #   abline(v=seq(-12,0,by=0.2174213),col=gray(.4),lwd=.5)
  # }
  if (leg) {
    if (!bw) {
      maps::map(Nort.map,Nort.map$names[grep(".a",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue")
      maps::map(Nort.map,Nort.map$names[grep(".b",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col="blue")
      maps::map(Nort.map,Nort.map$names[grep(".c",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col="darkblue")
      legend("bottomright",c("A: 70-120 m","B: 121-200 m","C: 201-500 m"),fill=c("lightblue","blue","darkblue"),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
    }
    else {
      maps::map(Nort.map,Nort.map$names[grep(".a",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.8))
      maps::map(Nort.map,Nort.map$names[grep(".b",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.6))
      maps::map(Nort.map,Nort.map$names[grep(".c",Nort.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.3))
      legend("bottomright",c("A: 70-120 m","B: 121-200 m","C: 201-500 m"),fill=c(gray(.8),gray(.6),gray(.3)),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
      }
  }
  if (country) {
    legend("bottom",ifelse(es,"España","Spain"),cex=2,inset=.15,bty="n")
  }
  if (places) {
    text(-8.383,43.367,"A Coruña",cex=.85,font=2,pos=1)
    points(-8.383,43.367,pch=15,cex=.9)
    text(-8.7167,42.233,"Vigo",cex=.85,font=2,pos=4)
    points(-8.7167,42.233,pch=15,cex=.9)
    text(-3.81,43.47,"Santander",cex=.85,font=2,pos=1)
    points(-3.81,43.48,pch=15,cex=.9)
    text(-2.934,43.26,"Bilbao",cex=.85,font=2,pos=1)
    points(-2.934,43.268,pch=15,cex=.9)
    text(-1.9884,43.3205,"San Sebastian",cex=.85,font=2,pos=1)
    points(-1.9884,43.321,pch=15,cex=.9)
    text(-5.6619,43.5440,"Gijon",cex=.85,font=2,pos=1)
    points(-5.6619,43.5440,pch=15,cex=.9)
    #points(-1.7915,43.3625,pch=15,cex=.9)
    #text(-1.7915,43.3625,"Fuenterrabía",font=2,pos=1,cex=.85)
    #points(-2.7213888888889,43.413,pch=15,cex=.9)
    #text(-2.7213888888889,43.420833333333,"Bermeo",font=2,pos=1,cex=.85)
  }
}
