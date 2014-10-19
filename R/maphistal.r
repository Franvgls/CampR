#' Mapa distribución entre tallas tmin y tmax
#'
#' Saca mapas de distribución de una especie entre tallas tmin y tmax para varias campañas  
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camps Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tmin Talla mínima del intervalo de tallas a incluir
#' @param tmax Talla máxima del intervalo de tallas a incluir
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ind Permite elegir entre número "n" o peso "p" peso sólo funciona cuando existen a y b en especies.dbf y se elige sólo una especie
#' @param sx Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param bw gráfico en blanco en negro si T o en color si F
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la funcion es la figura en pantalla, pero los datos en objeto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @return Si out.dat=T devuelve un data.frame con columnas: lan,lat,long,prof,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples maphistal(1,50,Psh[1:12],"Pnew",1,23,layout=c(4,3),out.dat=T)
#' @export
maphistal<-function(gr,esp,camps,dns="Pnew",tmin=0,tmax=999,cor.time=T,ind="n",sx=NA,bw=T,ti=T,
  plot=T,out.dat=F,idi="l",layout=NA,leg=T,ceros=T,escmult=.25,cexleg=1,years=F) {
	require(lattice)
	require(grid)
	require(maps)
  options(scipen=2)
#	if (plot) 
  if (length(esp)>1 | esp=="999") {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    if (!is.na(sx)) {
      stop("No se pueden seleccionar sexo para más de una especie")
      }
    if (ind=="p") {
      stop("No se pueden calcular capturas en peso de un rango de tallas para más de una especie")
      }
    medida<-c("cm")
    }
  else { medida<-ifelse(unid.camp(gr,esp)[1]==1,"cm","mm") }
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camps)
	dumb<-NULL
	for (i in 1:ndat) {
	  if (!is.null(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time,sx,ind))) dumb<-rbind(dumb,cbind(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time=cor.time,sx,ind),camp=camps[i]))
    }
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	dumb$camp<-factor(dumb$camp)
  if (ind=="n") {
    leyenda<-signif(max(dumb$numero,na.rm=T)*.9,1)
    escala<-signif(max(dumb$numero,na.rm=T),1)*escmult
    }
  else {
    leyenda<-signif(max(dumb$peso,na.rm=T)*.9,1)
    escala<-signif(max(dumb$peso,na.rm=T),1)*escmult
    }
	if (is.logical(ti)) {
		if (ti) {
      titulo<-list(label=paste(buscaesp(gr,esp,id=idi),"\n",tmin,"-",tmax,medida,ifelse(is.na(sx),"",sx)),
        font=ifelse(c(idi=="l" & gr!="9" & esp!="999"),4,2))
      }
		else {titulo<-NULL}
		}
	else {
    if(is.list(ti)) titulo<-ti
    else titulo<-list(label=ti)
    }
	if (bw) {
    colo=gray(.2)
    trellis.par.set(strip.background = list(col = grey(7:1/8)))
    }
	else {
    colo=4
    trellis.par.set(col.whitebg())
    }
  if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=F,tck=c(1,0),
      cex=cexleg,x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=T,
			panel=function(x,y,subscripts=subscripts) {
				panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid.polygon(map(Porc.map,"narr",plot=F)[[1]],map(Porc.map,"narr",plot=F)[[2]],
					default.units = "native",gp=gpar(fill=gray(.7)))
          if (ind=="n") {
    				if (leg & max(dumb$numero[subscripts],na.rm=T)>0) {
		  				panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			       }
            panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
						  pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
          if (ind=="p") {
    				if (leg & max(dumb$peso[subscripts],na.rm=T)>0) {
		   				panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			        }
            panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
						  pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
					})
			}
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew" | dns=="Medi") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),main=titulo,xlab=NULL,ylab=NULL,subscripts=T,
			ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=F,tck=c(1,0),cex=cexleg,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=T,
			panel=function(x,y,subscripts=subscripts) {
				panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid.polygon(map(Nort.map,"Costa",plot=F)[[1]],map(Nort.map,"Costa",plot=F)[[2]],
					default.units = "native",gp=gpar(fill=gray(.8)))
        if (ind=="n") {
				  if (leg & max(dumb$numero[subscripts],na.rm=T)>0) {
				    panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
		        ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
				    }
  				panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
	         pch=ifelse(dumb$numero[subscripts]>0,16,20),col=colo)
	        }
        else {
          if (leg & max(dumb$peso[subscripts],na.rm=T)>0) {
	   			  panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
	 	        ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			   	  }
  				panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
	         pch=ifelse(dumb$peso[subscripts]>0,16,20),col=colo)
          }
				})
			}
	if (dns=="Arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-8.1,-5.5),main=titulo,xlab=NULL,ylab=NULL,subscripts=T,
	    ylim=c(35.95,37.30),aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=F,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-5),labels=as.character(abs(-10:-5))),y=list(at=seq(35,36,by=1),rot=90)),
      as.table=T,panel=function(x,y,subscripts=subscripts) {
      panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	    grid.polygon(map(Arsa.map,c("Portugal","Costa"),plot=F)[[1]],map(Arsa.map,c("Portugal","Costa"),plot=F)[[2]],default.units = "native",gp=gpar(fill=gray(.8)))
				if (leg & max(dumb$numero[subscripts],na.rm=T)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					panel.xyplot(rep(-6,3),c(36.3,36.4,36.5),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					ltext(rep(-6,3),c(36.3,36.4,36.5),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (plot) {print(mapdist)}
	if (out.dat) {
	  if (ind=="n") dumb$numero<-round(dumb$numero,1)
    if (ind=="p") dumb$peso<-round(dumb$peso,2)
	  if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
	else {
    if (!plot) mapdist
    }
	}