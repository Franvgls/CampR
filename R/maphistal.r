#' Mapa distribución entre tallas tmin y tmax
#'
#' Saca mapas de distribución de una especie entre tallas tmin y tmax para varias campañas
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados. 6 Desechos y otros inorgánicos no tiene sentido sacar tallas, sólo recogidas en peces, crustáceos decápodos y algunos moluscos
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tmin Talla mínima del intervalo de tallas a incluir, si 0 el subtítulo sale =< tmax
#' @param tmax Talla máxima del intervalo de tallas a incluir, si 999 el subtítulo sale >= tmin
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param incl2 Si F no presenta los lances especiales, si T si los tiene en cuenta, pero puede dar problemas por que no puede calcular las abundancias estratificadas
#' @param ind Permite elegir entre número "n" o peso "p" peso sólo funciona cuando existen a y b en especies.dbf y se elige sólo una especie
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param bw gráfico en blanco en negro si T o en color si F
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín. También se puede poner como list con parámetros **main**: el título, **font** y demás.
#' @param subti ti T añade un subtítulo bajo la gráfica con el rango de tallas seleccionado.
#' @param plot Saca el gráfico (T) o si se salva como objeto se puede componer para componer con otros gráficos de lattice (F)
#' @param out.dat Si T el resultado final de la funcion es la figura en pantalla, pero los datos en objeto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param graf Si F no saca nada, si pones el nombre de un gráfico lo saca saca como archivo png y al final del proceso dice dónde está el mapa con ese nombre:
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples maphistal(1,50,Psh[1:12],"Porc",1,23,layout=c(4,3),out.dat=TRUE)
#' @family mapas
#' @family tallas
#' @export
maphistal<-function(gr,esp,camps,dns="Porc",tmin=0,tmax=999,cor.time=TRUE,incl2=TRUE,ind="n",ICESrect=FALSE,
                    sex=NA,bw=FALSE,ti=TRUE,subti=TRUE,plot=TRUE,out.dat=FALSE,idi="l",layout=NA,leg=TRUE,ceros=TRUE,
                    escmult=.25,cexleg=1,years=TRUE,graf=FALSE,xpng=1200,ypng=800,ppng=15) {
  options(scipen=2)
  colo<-ifelse(bw,gray(.1),4)
  if (plot) {
    if (bw) {
      colo=gray(.2)
      lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
    }
    else {
      lattice::trellis.par.set("strip.background",list(col="ivory2"))
      colo=4
    }
    }
  if (length(esp)>1 | any(esp=="999")) {
    message("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    if (!is.na(sex)) {
      stop("No se pueden seleccionar sexo para más de una especie")
      }
    if (ind=="p") {
      stop("No se pueden calcular capturas en peso de un rango de tallas para más de una especie")
      }
    medida<-c("cm")
    }
  else { medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm","mm") }
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camps)
	dumb<-NULL
	for (i in 1:ndat) {
	  if (!is.null(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex,ind))) dumb<-rbind(dumb,cbind(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex,ind),camp=camps[i]))
    }
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp)
    }
	dumb$camp<-factor(dumb$camp)
  if (ind=="n") {
    leyenda<-signif(max(dumb$numero,na.rm=TRUE)*.9,1)
    escala<-signif(max(dumb$numero,na.rm=TRUE),1)*escmult
    }
  else {
    leyenda<-signif(max(dumb$peso,na.rm=TRUE)*.9,1)
    escala<-signif(max(dumb$peso,na.rm=TRUE),1)*escmult
    }
	if (is.logical(ti)) {
	  if (ti) {titulo<-list(label=buscaesp(gr,esp,id=idi),font=ifelse((idi=="l" & gr!="9" & esp!="999"),4,2))
	  }
	  else {titulo<-NULL}
	}
	else {
	  if(is.list(ti)) titulo<-ti
	  else titulo<-list(label=ti)
	}
	if (subti) {
	if (tmin==0) sub<-list(label=bquote(" "<=.(format(paste0(tmax,ifelse(unid.camp(gr,esp)$MED==2," mm"," cm"))))),font.sub=2,cex=cexleg*.9)
	if (tmax==999) sub<-list(font.sub=2,label=bquote(" ">=.(format(paste0(tmin,ifelse(unid.camp(gr,esp)$MED==2," mm"," cm"))))),cex=cexleg*.9)
	if (tmin!=0 & tmax!=999) sub<-list(font.sub=2,label=paste(tmin,"-",tmax,ifelse(unid.camp(gr,esp)$MED==2,"mm","cm")),cex=cexleg*.9)
	if (tmin==0 & tmax==999) sub<-list(font.sub=2,label=paste(tmin,"-",tmax,ifelse(unid.camp(gr,esp)$MED==2,"mm","cm")),cex=cexleg*.9)
	}
#	if (is.logical(graf)) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
	if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),
      cex=cexleg,x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
			  lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
			  if (bw) lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
			  if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
			  lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
          if (ind=="n") {
    				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
		  				lattice::panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			lattice::ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			       }
            lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
						  pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
          if (ind=="p") {
    				if (leg & max(dumb$peso[subscripts],na.rm=TRUE)>0) {
		   				lattice::panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			lattice::ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			        }
            lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
						  pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
					})
			}
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Nort.map$range[c(1,2)],main=titulo,sub=sub,xlab=NULL,ylab=NULL,subscripts=TRUE,
			ylim=Nort.map$range[c(3,4)],aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
			  lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
			  if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
			  lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
        if (ind=="n") {
				  if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
				    lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
		        lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
				    }
  				lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
	         pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
	        }
        else {
          if (leg & max(dumb$peso[subscripts],na.rm=TRUE)>0) {
	   			  lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
	 	        lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			   	  }
  				lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
	         pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
          }
				})
			}
	if (dns=="Arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-8.1,-5.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,subscripts=TRUE,
	    ylim=c(35.95,37.30),aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-5),labels=as.character(abs(-10:-5))),
      y=list(at=seq(35,36,by=1),rot=90)),as.table=TRUE,panel=function(x,y,subscripts=subscripts) {
        lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
        if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
        lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	    grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],
      default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
				#lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-6,3),c(36.3,36.4,36.5),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-6,3),c(36.3,36.4,36.5),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (dns=="Medi") {
		asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-5.7,5),ylim=c(35,43),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
      subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),
      "W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=seq(36,42,by=1),rot=90)),as.table=TRUE,
      panel=function(x,y,subscripts=subscripts) {
        lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
        lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	      grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[2]],
        default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-4,3),c(39.1,39.6,40.),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-4,3),c(39.1,39.6,40.),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (!is.logical(graf)) {
	  png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
	  print(mapdist)
	  dev.off()}
	if (plot) {print(mapdist)}
	if (!plot) return(mapdist)
  if (!is.logical(graf)) {
	  dev.off()
	  message(paste0("figura: ",getwd(),"/",graf,".png"))
	}
	if (!is.logical(graf)) par(mar=c(5, 4, 4, 2) + 0.1)
	if (out.dat) {
	  if (ind=="n") dumb$numero<-round(dumb$numero,1)
    if (ind=="p") dumb$peso<-round(dumb$peso,2)
	  if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
}
