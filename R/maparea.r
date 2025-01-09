#' Mapa general del banco de Porcupine
#'
#' Mapa con la estratificación y el área general del banco de Porcupine incluyendo parte de Irlanda
#' @param ICESrect Si T incluye los los rectángulos ICES
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param SACs si T incluye la silueta de las areas de especial atención irlandesas SACs
#' @param filSAC si T las SACs se llenan con color verde, si F sólo queda el contorno
#' @param es Textos en español, si F en inglés
#' @param leg incluye la leyenda con los colores/patrones de los estratos
#' @param dpth si T incluye los datos de estratos batimétricos en la leyenda, si F solo los sectores
#' @param bw Si T mapa en blanco y negro respecto a tierra y puntos, en caso contrario en color. Para sacar el diseño de estratos de Porcupine se utiliza sectcol=TRUE y leg=TRUE
#' @param dens si mayor de 0 las superficies de los estratos tienen patrones de líneas
#' @param sectcol si T pone los sectores con color de fondo, en caso contrario lo deja en blanco, bueno para armap.tot
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @examples maparea(sectcol=TRUE,leg=TRUE,graf="MapArea.ejemplo")
#' @examples maparea(sectcol=FALSE,leg=FALSE)
#' @examples maparea(sectcol=FALSE,leg=FALSE,SACs=T,filSAC=T)
#' @family mapas
#' @family Porcupine
#' @export
maparea<-function(ICESrect=FALSE,ICESlab=FALSE,ICESlabcex=.7,es=TRUE,leg=TRUE,dpth=TRUE,bw=FALSE,dens=0,
                  sectcol=FALSE,SACs=FALSE,filSAC=FALSE,graf=FALSE,xpng=1000,ypng=900,ppng=15) {
  library(mapdata)
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  maps::map("worldHires",c("ireland","UK:Northern Ireland"),ylim=c(50.5,54.5),xlim=c(-15.5,-8.2),
		fill=TRUE,col=ifelse(bw,gray(.7),"saddlebrown"),type="n")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=ifelse(bw,"white","lightblue1"))
  #box()
	#rect(-16,50.,-8.,55)
	abline(v=c(-15:-6),h=c(51:54),lty=3,col=gray(.2))
	if (ICESrect) abline(h=seq(50,55,by=.5),v=seq(-18,-10,by=1),col=gray(.2),lwd=.6)
	if (ICESlab) text(c(stat_y+.215)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
	maps::map("worldHires",c("ireland","UK:Northern Ireland"),fill=TRUE,col=ifelse(!bw,"bisque","gray85"),add=TRUE)
	detach("package:mapdata")
	points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
	text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
	text(-(8.95),(52.2),label=ifelse(es,"IRLANDA","IRELAND"),cex=1.3,font=2)
	if (sectcol) colrs=c("Steelblue2","Steelblue2","Steelblue","blue4","green","darkgreen",gray(.7))
	else {
		if (bw) {colrs=c(rep(ifelse(bw,"white","lightblue"),6),gray(.7))} else {colrs=c(rep("lightblue1",6),"antiquewhite")}
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
	if (SACs) {
	  polygon(hmmSAC$long,hmmSAC$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	  polygon(NewPorcCanyon$long,NewPorcCanyon$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	  polygon(PorcNWSAC$long,PorcNWSAC$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	  polygon(PorcSWSAC$long,PorcSWSAC$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	  polygon(PorcShelfSAC$long,PorcShelfSAC$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	  polygon(BelgicaMound$long,BelgicaMound$lat,lwd=2,col=ifelse(filSAC,"darkolivegreen",NA))
	}
	if (leg) {
		rect(-13.35,50.7,-10.2,51.3,col="white")
		rect(-13.2,51.05,-12.95,51.2,col=colrs[1])
		rect(-12.95,51.05,-12.7,51.2,col=colrs[3])
		rect(-12.7,51.05,-12.45,51.2,col=colrs[4])
		rect(-13.,50.8,-12.75,50.95,col=colrs[5])
		rect(-12.75,50.8,-12.5,50.95,col=colrs[6])
		if (dens>0) {
			rect(-13.05,51.05,-12.8,51.2,col=1,density=15)
			rect(-12.8,51.05,-12.55,51.2,col=1,density=15,angle=0)
			rect(-12.55,51.05,-12.30,51.2,col=1,density=15,angle=135)
			rect(-12.9,50.8,-12.65,50.95,col=1,density=15,angle=0)
			rect(-12.65,50.8,-12.4,50.95,col=1,density=15,angle=135)
		   }
		text(-12.5,(51.2+51.05)/2,label=ifelse(es,"Sector 1 (norte)","Sector 1 (North)"),pos=4,cex=.9,font=2)
		text(-12.5,(50.8+50.95)/2,label=ifelse(es,"Sector 2 (sur)","Sector 2 (south)"),pos=4,cex=.9,font=2)
		if (dpth) {
		  par(lheight=1.5)
		  text(-10.85,(50.76+51.26)/2,label=ifelse(es,"Estratos\nE (170-300 m)\nF (301-450 m)\n G (451-800 m)","Depth strata:\nE (170-300 m)\nF (301-450 m)\n G (451-800 m)"),cex=.9,font=2)
		  rect(-10.46,51.1,-10.34,51.02,col=colrs[1])
		  rect(-10.46,50.91,-10.34,50.98,col=colrs[3])
		  rect(-10.41,50.91,-10.34,50.98,col=colrs[5])
		  rect(-10.46,50.785,-10.4,50.86,col=colrs[4])
		  rect(-10.41,50.785,-10.34,50.86,col=colrs[6])
		  par(lheight=1)
		}
	}
	box()
	axis(1,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(2,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	axis(3,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(4,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	if (!is.logical(graf)) {
	  dev.off()
	  message(paste0("figura: ",getwd(),"/",graf,".png"))
	}
	}
