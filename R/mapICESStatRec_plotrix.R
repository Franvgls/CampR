#' Mapa distribución por edad en rectángulos estadísticos ICES en la campaña
#'
#' Mapa de la distribución geográfica en rectángulos ICES por edad de la especie en la campaña solicitada
#' @param gr Grupo de la especie: Solo hay dados de edad para algunos peces y cigala ?
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña o campañas a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa". Medits "Medi" no tiene sentido en esta gráfica por no utilizar rectángulos ICES
#' @param age Edad solicitada
#' @param plus Edad plus: incluir la edad considerada como plus, solo afecta si se pide como plus la edad solicitada que suma todas las edades mayores
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param AltAlk Clave talla edad alternativa sin ruta ni extensión, NA por defecto usa la clave de la campaña edadXYY.dbf
#' @param ICESrect Si T representa las líneas de los rectángulos ICES. No para dns="Medi"
#' @param ICESlab Si T saca etiqueta del rectángulo ices en la parte superior. No para dns="Medi"
#' @param incl2 si T representa los datos de lances especiales, si F los excluye
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín y año de la campaña o campaña si years=F
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T saca los ejes y legendas en español, si F lo saca en inglés
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param years Si T permite sacar el dato de la campaña como nombre campaña o como año
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero, tienen valores default para cant,porc,arsa, no tocar si no
#' @param ypng height archivo png si graf es el nombre del fichero, tienen valores default para cant,porc,arsa, no tocar si no
#' @param ppng points png archivo si graf es el nombre del fichero

#' @param mediahora Valor para obtener abundancias por hora si media hora es mayor
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,0,1,2,...,Age+ (número de individuos edad 0,1,2...),camp. Si out.dat=F saca el Gráfico en pantalla o como objeto para combinar con otros Gráficos con print.trellis
#' @examples mapICESStatRec.plotrix(1,74,"N05","Cant",plus=3,ICESlab = T,bw=F)
#' @examples mapICESStatRec.plotrix(1,43,"P21","Porc",plus=7,ICESlab = T,bw=F)
#' @examples mapICESStatRec.plotrix(1,73,"219","Arsa",plus=3,ICESlab = T,bw=F)
#' @family mapas
#' @family edades
#' @export
mapICESStatRec.plotrix<-function(gr,esp,camp,dns="Cant",plus=3,cor.time=TRUE,ICESrect=TRUE,ICESlab=FALSE,AltAlk=NA,incl2=FALSE,bw=FALSE,
                                 ti=TRUE,plot=TRUE,out.dat=FALSE,ind="n",idi="l",es=TRUE,years=TRUE,escmult=.1,ceros=FALSE,mediahora=2,
                                 graf=FALSE,xpng=NA,ypng=NA,ppng=15) {
  options(scipen=2)
  if (length(esp)>1) {stop("Esta función sólo admite una especie")}
  if (dns=="Medits") {stop(ifelse(es,"En el Medíteráneo no están definidos los rectángulos ICES","ICES rectangles are not used in the Mediterranean"))}
  esp<-format(esp,width=3,justify="r")
  #colo<-ifelse(bw,gray(.1),4)
  #if (bw) {colo=gray(.1)} else {colo=4}
  # function(gr,esp,camp,dns="Cant",plus=8,mediahora=2,cor.time=TRUE,AltAlk=NA,incl2=FALSE,DatGraf=FALSE,es=T)
  mapage<-maphistage(gr,esp,camp,dns,age=0,plus=plus,out.dat = T,plot=F)
  mapice<-AbAgStatRec.camp(gr,esp,camp,dns,plus=plus,DatGraf = T)
  #maphis<-maphist(gr,esp,camp,dns,out.dat = T,plot=F)
  if (bw) paleta<-grey.colors(plus+1,0,1)
  else paleta<-palette("default")[1:c(plus+1)]
  if (!is.logical(graf)) {
    if (is.na(xpng & dns=="Cant")) {xpng=1200;ypng=600}
    if (is.na(xpng & dns=="Porc")) {xpng=750;ypng=900}
    if (is.na(xpng & dns=="Cant")) {xpng=1200;ypng=800}
    png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  }
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
		    mapporco(ICESrect = ICESrect,ICESlab = ICESlab,bw=bw)
	    if (ti) {
	      title(main=buscaesp(gr,esp),line=2.2,font.main=4)
	      mtext(ifelse(years,camptoyear(camp),camp),3,line=1.1,adj=0.5,font=2,cex=.9)
	    }
	    points(lat~long,mapage,pch=20,bg="black",cex=.8)
	    points(lat~long,dplyr::filter(mapage,rowSums(mapage[,5:c(5+plus)])>0),pch=21,bg="yellow")
	    for (i in 1:nrow(mapice)) {
		    if (sum(mapice[i,3:c(3+plus)])>0) {
		      plotrix::floating.pie(mapice$long[i],mapice$lat[i],as.matrix(mapice[i,3:c(3+plus)]),
		                            radius=.2,col=paleta)
		    }
		  }
		  legend("bottomright",c(paste0("Ag",0:c(plus-1)),paste0("Ag",plus,"+")),cex=1.5,fill=paleta,col="white",inset = .02,bg="white",box.col = "white")
		  legend("bottom",c("Haul 0 catches","Haul with catches"),pch=c(20,21),pt.bg=c(NA,"yellow"),pt.cex=c(.8,1),horiz = T,inset = c(.02,.02),bg="white",box.col = "white")
	}
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		MapNort(ICESrect = ICESrect,ICESlab = ICESlab,bw=bw)
		if (ti) {
		title(main=buscaesp(gr,esp),line=2.2,font.main=4)
		mtext(ifelse(years,camptoyear(camp),camp),3,line=1.1,adj=0.5,font=2,cex=.9)
		}
		points(lat~long,mapage,pch=20,cex=.8,bg="black")
		points(lat~long,dplyr::filter(mapage,rowSums(mapage[,5:c(5+plus)])>0),pch=21,bg="yellow")
		for (i in 1:nrow(mapice)) {
		  if (sum(mapice[i,3:c(3+plus)])>0) {
		    plotrix::floating.pie(mapice$long[i],mapice$lat[i],as.matrix(mapice[i,3:c(3+plus)]),
		                          radius=.2,col=paleta)
		  }
		}
		legend("bottomright",c(paste0("Ag",0:c(plus-1)),paste0("Ag",plus,"+")),cex=1.5,horiz = T,fill=paleta,col="white",inset = .02,bg="white",box.col = "white")
		legend("bottomright",c("Haul 0 catches","Haul with catches"),pch=c(20,21),pt.bg=c(NA,"yellow"),pt.cex=c(.8,1),horiz = T,inset = c(.02,.12),bty="n")
	}
	if (dns=="Arsa") {
		#asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		MapArsa(ICESrect = ICESrect,ICESlab = ICESlab,bw=bw)
	  if (ti) {
	    title(main=buscaesp(gr,esp),line=2.2,font.main=4)
	    mtext(ifelse(years,camptoyear(camp),camp),3,line=1.1,adj=0.5,font=2,cex=.9)
	  }
	  points(lat~long,mapage,pch=20,cex=.8,bg="black")
		points(lat~long,dplyr::filter(mapage,rowSums(mapage[,5:c(5+plus)])>0),pch=21,bg="yellow")
		  for (i in 1:nrow(mapice)) {
		    if (sum(mapice[i,3:c(3+plus)])>0) {
		      plotrix::floating.pie(mapice$long[i],mapice$lat[i],as.matrix(mapice[i,3:c(3+plus)]),
		                            radius=.2,col=paleta)
		    }
		  }
		  legend("topright",c(paste0("Ag",0:c(plus-1)),paste0("Ag",plus,"+")),cex=1.5,horiz = F,fill=paleta,col="white",inset = .02,bg="white",box.col = "white")
		  legend("topright",c("Haul 0 catches","Haul with catches"),pch=c(20,21),pt.bg=c(NA,"yellow"),pt.cex=c(.8,1),horiz = T,inset = c(.14,.02),bg="white",box.col = "white")
	}
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
  if (out.dat) mapice
}
