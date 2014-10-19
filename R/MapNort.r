#' Mapa del Cantábrico y Galicia
#' 
#' Función auxiliar para sacar el mapa de la campaña Demersales
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD) 
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param bw si T mapa con tierra en gris, si F tierra en color
#' @param es si T saca textos en español, si F en inglés
#' @param ax Si T saca los ejes x e y
#' @param strat Si marca los sectores geográficos (los batimetricos salen con las líneas correspondientes
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @param xlims Define los limites longitudinales del mapa, por defecto -10.25 y -1.4 oeste
#' @param ylims Define los limites longitudinales del mapa, por defecto 41.82 y 44.48 norte
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @seealso MapCant {\link{MapCant}} mapporco {\link{mapporco}}
#' @export
MapNort<- function(lwdl=.5,cuadr=F,cuadrMSFD=F,ICESrect=F,bw=T,es=F,ax=T,strat=F,places=F,xlims=c(-10.25,-1.4),ylims=c(41.82,44.48)) {
  data(CampR)
  require(maps)
  map(Nort.str,xlim=xlims,ylim=ylims,type="n")
  if (ax) {
    axis(2,at=c(42:44),las=1,tick=F,line=-.5,cex.axis=.8)
    #rug(42:44,-.01,side=2,pos=-10.32,lwd=2)
    axis(1,at=c(-10:-2),labels=c(10:2),tick=F,line=-.8,cex.axis=.8)
    #rug(-10:-2,-.01,side=1,pos=41.79,lwd=2)
    axis(3,at=c(-10:-2),labels=c(10:2),tick=F,line=-.5,cex.axis=.8)
    #rug(-10:-2,-.01,side=3,pos=44.33,lwd=2)
    axis(4,at=c(42:44),las=1,tick=F,line=-.5,cex.axis=.8)
    #rug(42:44,-.01,side=4,pos=-1.33,lwd=2)
  }
  if (cuadr) {
    abline(h=seq(41,45,by=1/12),col=gray(.4),lwd=.5)
    abline(v=seq(-12,0,by=3/26),col=gray(.4),lwd=.5)
  }
  if (cuadrMSFD) {
    abline(h=seq(31,45,by=1/6),col=gray(.4),lwd=.5)
    abline(v=seq(-12,0,by=0.2174213),col=gray(.4),lwd=.5)
  }
  if (ICESrect) {
    abline(h=seq(41,45,by=.5),col=gray(.2),lwd=.5)
    abline(v=seq(-12,-1),col=gray(.2),lwd=.5)
  }
  if (strat) {
    abline(h=43,v=c(-7.66,-6,-3.5),lty=1,col=gray(.0),lwd=2)
    text(c(-10,-9.7,-6.83,-4.75,-2.7),c(42.4,44.3,44.3,44.3,44.3),c("MF","FE","EP","PA","AB"),font=2)
  }
  map(Nort.str,add=T,fill=T,col=c(rep(NA,16),ifelse(bw,"light gray","burlywood3")),lwd=lwdl)
  map(Nort.map,Nort.map$names[1:16],add=T,col=c("gray"),lwd=lwdl)
  map(Nort.map,Nort.map$names[17],add=T,col=c("black"),lwd=lwdl)
  box(lwd=2)
  if (places) {
    legend("bottom",ifelse(es,"España","Spain"),cex=2,inset=.15,bty="n")
    text(-8.383,43.367,"A Coruña",cex=.85,font=2,pos=1)
    points(-8.383,43.367,pch=15,cex=.9)
    text(-8.7167,42.233,"Vigo",cex=.85,font=2,pos=4)
    points(-8.7167,42.233,pch=15,cex=.9)
    text(-3.81,43.47,"Santander",cex=.85,font=2,pos=1)
    points(-3.81,43.48,pch=15,cex=.9)
    text(-2.934,43.26,"Bilbao",cex=.85,font=2,pos=1)
    points(-2.934,43.268,pch=15,cex=.9)
    text(-5.663,43.56,"Gijon",cex=.85,font=2,pos=1)
    points(-5.663,43.56,pch=15,cex=.9)
    #points(-1.7915,43.3625,pch=15,cex=.9)
    #text(-1.7915,43.3625,"Fuenterrabía",font=2,pos=1,cex=.85)
    #points(-2.7213888888889,43.413,pch=15,cex=.9) 
    #text(-2.7213888888889,43.420833333333,"Bermeo",font=2,pos=1,cex=.85) 
  }
}
