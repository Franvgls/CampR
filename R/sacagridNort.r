#' Saca el grid de cuadrículas para Demersales Norte 
#' 
#' Crea el grid de cuadrículas de Porcupine con los puntos centrales de cada una de las cuadrículas (5x5 millas náuticas) disponibles en la campaña de Demersales Cantábrico/Galicia
#' @seealso sacagrid {\link{sacagrid.r}}
#' @return Devuelve un data.frame con el punto central de cada una de las cuadriculas en la campaña de Porcupine. Un objeto llamado por defecto Porc.grid data.frame con cuatro columnas x,y,pt (número correlativo en cada estrato),strat (estrato del punto)
sacagridNort<-function() {
  require(maps)
  require(gstat)
  MapNort(cuadr=T,lwdl=2)
  dumb<-data.frame(x=NULL,y=NULL,pt=NULL,strat=NULL)
  i2=1
  for (nstrat in 1:length(which(Nort.map$names!="Costa"))) {
    for (i in seq(41.5-(.5/12),45-(.5/12),by=1/12)){
      for (i1 in seq(-12+(1.5/26),-1+(1.5/26),by=3/26)) {
        if (point.in.polygon(i1,i,map(Nort.map,Nort.map$names[nstrat],plot=F)[[1]],
                             map(Nort.map,Nort.map$names[nstrat],plot=F)[[2]])) {
          dumb<-rbind(dumb,data.frame(x=i1,y=i,pt=i2,strat=substr(as.character(Nort.map$names[nstrat]),1,2)))
          i2=i2+1
        }
      }
    }
  }
  dumb
}
