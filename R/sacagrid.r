#' Saca el grid de cuadrículas para Porcupine
#'
#' Crea el grid de cuadrículas de Porcupine con los puntos centrales de cada una de las cuadrículas (5x5 millas náuticas) disponibles en la campaña de Porcupine
#' @seealso {\link{sorteo}}
#' @return Devuelve un data.frame con el punto central de cada una de las cuadriculas en la campaña de Porcupine. Un objeto llamado por defecto Porc.grid data.frame con cuatro columnas x,y,pt (número correlativo en cada estrato),strat (estrato del punto)
#' @export
sacagrid<-function(color=2,cexlab=.7,etiq=TRUE) {
  mapporco(cuadr=TRUE,lwdl=2)
  dumb<-data.frame(x=NULL,y=NULL,pt=NULL,strat=NULL)
  i2=1
  for (nstrat in 1:length(which(Porc.map$names!="narr"))) {
    for (i in seq(54-(.5/12),51-(.5/12),by=-1/12)){
      for (i1 in seq(-15+(1.5/23),-12+(21/23),by=3/23)) {
        if (sp::point.in.polygon(i1,i,maps::map(Porc.map,Porc.map$names[nstrat],plot=FALSE)[[1]],
                             maps::map(Porc.map,Porc.map$names[nstrat],plot=FALSE)[[2]])) {
          dumb<-rbind(dumb,data.frame(x=i1,y=i,pt=i2,strat=substr(as.character(Porc.map$names[nstrat]),1,2)))
          if (etiq) text(y~x,dumb[nrow(dumb),],label=pt,cex=cexlab,col=color)
          i2=i2+1
        }
      }
    }
  }
  dumb
  }
