#' Mapa general del banco de Porcupine
#'
#' Mapa con la estratificación y el área general del banco de Porcupine incluyendo parte de Irlanda
#' @param es Textos en español, si F en inglés
#' @param leg incluye la leyenda con los colores/patrones de los estratos
#' @param bw Si T mapa en blanco y negro respecto a tierra y puntos, en caso contrario en color. Para sacar el diseño de estratos de Porcupine se utiliza sectcol=TRUE y leg=TRUE
#' @param dens si mayor de 0 las superficies de los estratos tienen patrones de líneas
#' @param sectcol si T pone los sectores con color de fondo, en caso contrario lo deja en blanco, bueno para armap.tot
#' @examples maparea(sectcol=TRUE,leg=TRUE)
#' @examples maparea(sectcol=FALSE,leg=FALSE)
#' @family mapas
#' @family Porcupine
#' @export
maparea<-function(es=TRUE,leg=TRUE,bw=FALSE,dens=0,ICESrect=F,sectcol=F) {
  library(mapdata)
  maps::map("worldHires",c("ireland","UK:Northern Ireland"),ylim=c(50.5,54.5),xlim=c(-15.5,-8.2),
		fill=TRUE,col="saddlebrown",type="n")
	box()
	rect(-16,50.,-8.,55)
	abline(v=c(-15:-6),h=c(51:54),lty=3,col=gray(.2))
	if (ICESrect) {
	  abline(h=seq(50,55,by=.5),col=gray(.2),lwd=.6)
	  abline(v=seq(-18,-10,by=1),col=gray(.2),lwd=.6)
	  }
	maps::map("worldHires",c("ireland","UK:Northern Ireland"),fill=TRUE,col=ifelse(!bw,"wheat","gray95"),add=TRUE)
	detach("package:mapdata")
	points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
	text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
	text(-(8.95),(52.2),label=ifelse(es,"IRLANDA","IRELAND"),cex=1.3,font=2)
	if (sectcol) colrs=c("Steelblue2","Steelblue2","Steelblue","blue4","green","darkgreen",gray(.7))
	else {
		colrs=c("white","white","white","white","white","white",gray(.7))
		#dens=0
		}
	maps::map(Porc.map,add=TRUE,fill=TRUE,col=colrs)
	if (dens>0) {
		polygon(maps::map(Porc.map,"1Aa",plot=FALSE)$x,maps::map(Porc.map,"1Aa",plot=FALSE)$y,density=dens)
		polygon(maps::map(Porc.map,"1Ab",plot=FALSE)$x,maps::map(Porc.map,"1Ab",plot=FALSE)$y,density=dens)
		polygon(maps::map(Porc.map,"1B",plot=FALSE)$x,maps::map(Porc.map,"1B",plot=FALSE)$y,density=dens,angle=0)
		polygon(maps::map(Porc.map,"2B",plot=FALSE)$x,maps::map(Porc.map,"2B",plot=FALSE)$y,density=dens,angle=0)
		polygon(maps::map(Porc.map,"2C",plot=FALSE)$x,maps::map(Porc.map,"2C",plot=FALSE)$y,density=dens,angle=135)
		polygon(maps::map(Porc.map,"1C",plot=FALSE)$x,maps::map(Porc.map,"1C",plot=FALSE)$y,density=dens,angle=135)
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
	box()
	axis(1,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(2,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	axis(3,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(4,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	}
