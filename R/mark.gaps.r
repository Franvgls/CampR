#' Busca huecos en un fichero de lances creado con sorteo()
#' 
#' En un fichero de lances creado por sorteo busca los huecos de cuadrículas agrupadas sin ningun lance alrededor
#' @param x es el fichero de lances a comprobar
#' @param buffer Distancia entre cuadrículas contiguas que es posible elegir. El valor por defecto asegura que no se seleccionan dos cuadrículas contíguas
#' @param srv.dsg Diseño del muestreo. data.frame de dos columnas, strat, stations (estrato y número de lances)
#' @return Devuelve un mapa con los lances y los huecos en rojo además de los puntos sin lances alredador
#' @seealso mapsorteo {\link{mapsorteo.r}} sorteo {\link{sorteo.r}}
#' @export
mark.gaps<- function(x,buffer=.1177,srv.dsg=Porc.dsg){
  distgeogr<- function(a,b){sqrt(sum((abs(a[1]-b[1])*cos(mean(c(a[[2]],b[[2]]))*pi/180))^2,abs(a[2]-b[2])^2))}
  strats<-data.frame(lab=srv.dsg[,1],stat.no=srv.dsg[,2])
  if (!exists("Porc.grid")) {Porc.grid<-sacagrid()}
  gridbase<-Porc.grid
  hauls<-x
  mapporco(cuadr=T,lwdl=2)
  points(gridbase[,1:2])
  points(hauls[,1:2],pch=16,col=2,cex=.8)
  for (nhauls in 1:nrow(hauls)) {
    gridbase<-gridbase[as.vector(apply(gridbase[,1:2],1,distgeogr,b=hauls[nhauls,1:2]))>buffer,]
    if (c(nhauls/5-trunc(nhauls/5))==0) {
      mapporco(cuadr=T,lwdl=2)
      points(gridbase[,1:2])
      points(hauls[,1:2],pch=16,col=2,cex=.8)
    }
  }
  mapporco(cuadr=T,lwdl=2)
  points(gridbase[,1:2])
  points(hauls[,1:2],pch=16,col=2,cex=.8)
  gridbase
}
