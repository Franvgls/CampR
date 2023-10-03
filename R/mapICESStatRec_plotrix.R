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
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param es Si T saca los ejes y legendas en español, si F lo saca en inglés
#' @param layout Organización de gráficos en filas columnas c(r,c)
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param years Si T permite sacar el dato de la campaña como nombre campaña o como año
#' @param mediahora Valor para obtener abundancias por hora si media hora es mayor
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,0,1,2,...,Age+ (número de individuos edad 0,1,2...),camp. Si out.dat=F saca el Gráfico en pantalla o como objeto para combinar con otros Gráficos con print.trellis
#' @examples mapICESStatRec(1,74,"N22","Cant",plus=3)
#' @examples mapICESStatRec(1,43,"P21","Porc",age=3,plus=8,bw=F,lattice=F,plotrix=F)
#' @examples mapICESStatRec(1,74,"N22",age=1,"Cant",lattice = F,plotrix = F)
#' @family mapas
#' @family edades
#' @export
mapICESStatRec.plotrix<-function(gr,esp,camp,dns="Cant",age=0,plus=3,cor.time=TRUE,ICESrect=TRUE,ICESlab=FALSE,AltAlk=NA,incl2=FALSE,bw=FALSE,ti=TRUE,plot=T,
  out.dat=FALSE,ind="n",cexleg=1,idi="l",es=TRUE,years=TRUE,escmult=.1,layout=NA,ceros=FALSE,mediahora=2) {
  options(scipen=2)
  colo<-ifelse(bw,gray(.1),4)
  if (bw) {colo=gray(.1)} else {colo=4}
  if (length(esp)>1) {stop("Esta función sólo admite una especie")}
  esp<-format(esp,width=3,justify="r")
  # function(gr,esp,camp,dns="Cant",plus=8,mediahora=2,cor.time=TRUE,AltAlk=NA,incl2=FALSE,DatGraf=FALSE,es=T)
  Result3<-AbAgStatRec.camp(gr=gr,esp = esp,camp = camp,dns = dns,plus = plus,cor.time = cor.time,AltAlk = AltAlk,incl2 = incl2,DatGraf = T)
  MapHistAge<-maphistage(gr,esp,camp,dns,age=0,plus=plus,out.dat=T,plot=F,incl2 = F)
  leyenda<-signif(max(Result3$numero)*.9,1)
  escala<-signif(max(Result3$numero),1)*escmult
	if (is.logical(ti)) {
		if (ti) {titulo<-list(label=paste(buscaesp(gr,esp,id=idi),"\n",ifelse(years,camptoyear(camp),camp),sep=""),
		font=ifelse(idi=="l",4,2),cex=cexleg)}
		else {titulo<-NULL}
		}
	else {titulo<-list(label=ti)}
	if (any(is.na(layout))) {
		if (plus!=4) layout=c(1,plus)
		if (plus==4) layout=c(2,2)
		}
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
    if (lattice) {
		mapdist<-lattice::xyplot(lat~long|name,Result3,layout=layout,xlim=c(-15.5,-10.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
			x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
			  lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
			  if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
			  if (ICESlab) lattice::ltext(x=Area$stat_x,y=Area$stat_y+.22,label=Area$ICESNAME,cex=.7,font=2)
			  lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
					if (max(Result3$numero[subscripts],na.rm=TRUE)>0) {
						lattice::panel.xyplot(-12.5,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
						lattice::ltext(-12.5,51.2,labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=.8)
					}
          lattice::panel.xyplot(x,y,cex=ifelse(Result3$numero[subscripts]>0,sqrt((Result3$numero[subscripts])/escala),.35),
						pch=ifelse(Result3$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
			})}
		  if (!lattice & !plotrix) {
		    MapHistAge<-maphistage(gr,esp,camp,dns,age=0,plus=plus,out.dat=T,plot=F,incl2 = F)
		    mapporco(ICESrect = T,ICESlab = T)
		    title(main=buscaesp(gr,esp),line=2.1,font.main=4)
		    title(main=paste0(ifelse(es,"Edad","Age")," ",age,ifelse(age==plus,"+","")),cex.main=.9,line=1.1)
		    points(lat~long,Result3,pch=21,subset=c(name==paste0("Ed",age,ifelse(plus==age,"+","")) & numero>0),bg="red",cex=sqrt(numero))
		    points(lat~long,MapHistAge,pch=21,subset=MapHistAge[,age+5]>0,bg="blue")
		    legend("bottomright",c("ICES StatRec","Lances Camp"),pch=21,pt.bg=c("red","blue"),inset=c(.1,.1),bg="white")
		    return()
		    }
		  if (plotrix)
		    dat<-AbAgStatRec.camp(gr,esp,camp,dns,plus=plus,plotrix=T)
		    mapporco(ICESrect = T,ICESlab = T)
		    for (i in which(rowSums(dat[,5:plus])>0)) {
		      floating.pie(xpos=dat$long[i],ypos=dat$lat[i],x=prop.table(as.matrix(dat[i,c(5:8)]),1),
		                   col=c("blue","red","green","yellow"),radius=sqrt(rowSums(dat[i,c(5:8)]))/(200))
		      plotrix::floating.pie(xpos=dat$long[i],ypos=dat$lat[i],x=prop.table(as.matrix(dat[i,c(2:c(plus+2))]),1),
		               col=rainbow(length(0:plus)),radius=sqrt(rowSums(dat[2,2:c(plus+1)]))/(200))
		    }
		    plotrix::floating.pie(ypos=51.5,xpos=-12,x=1,col=1,radius=sqrt(10)/200)
		    plotrix::floating.pie(ypos=51.25,xpos=-12,x=1,col=1,radius=sqrt(100)/200)
		    plotrix::floating.pie(ypos=51,xpos=-12,x=1,col=1,radius=sqrt(1000)/200)
		    text(-12,51,"1000 ind.",pos=4,offset=1.5)
		    text(-12,51.25,"100 ind.",pos=4,offset=1)
		    text(-12,51.5,"10 ind.",pos=4,offset=.75)
			}
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		if (lattice) {
		mapdist<-lattice::xyplot(lat~long|name,Result3,layout=layout,xlim=Nort.map$range[c(1,2)],main=titulo,xlab=NULL,ylab=NULL,subscripts=TRUE,
			ylim=Nort.map$range[c(3,4)],aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
			  lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
			  if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
			  if (ICESlab) lattice::ltext(x=Area$stat_x,y=Area$stat_y+.22,label=Area$ICESNAME,cex=.7,font=2)
			  lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
					if (max(Result3$numero[subscripts],na.rm=TRUE)>0) {
						lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
						lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=cexleg)
						}
				lattice::panel.xyplot(x,y,cex=ifelse(Result3$numero[subscripts]>0,sqrt((Result3$numero[subscripts])/escala),.35),
					pch=ifelse(Result3$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
			})}
		  MapHistAge<-maphistage(gr,esp,camp,"Cant",age=0,plus=plus,out.dat=T,plot=F,incl2 = F)
		if (!lattice & !plotrix) {
		  MapNort(ICESrect = T,ICESlab = T)
		  title(main=buscaesp(gr,esp),line=2.2,font.main=4)
		  title(main=paste0(ifelse(es,"Edad","Age")," ",age,ifelse(age==plus,"+","")),cex.main=.9,line=1.2)
		  points(lat~long,Result3,pch=21,subset=c(name==paste0("Ed",age,ifelse(plus==age,"+","")) & numero>0),bg="red")
		  points(lat~long,MapHistAge,pch=21,subset=MapHistAge[,age+5]>0,bg="blue")
		  legend("bottomright",c("ICES StatRec","Lances Camp"),pch=21,pt.bg=c("red","blue"),inset=c(.1,.1),bg="white")
		  return()
		}
		if (plotrix)
		  MapNort(ICESrect = T,ICESlab = T)
		for (i in which(rowSums(MapHistAge[,5:plus])>0)) {
		  plotrix::floating.pie(xpos=MapHistAge$long[i],ypos=MapHistAge$lat[i],x=prop.table(as.matrix(MapHistAge[i,c(5:8)]),1),
		               col=c("blue","red","green","yellow"),radius=sqrt(rowSums(MapHistAge[i,c(5:8)]))/(200))
		}
		plotrix::floating.pie(ypos=51.5,xpos=-12,x=1,col=1,radius=sqrt(10)/200)
		plotrix::floating.pie(ypos=51.25,xpos=-12,x=1,col=1,radius=sqrt(100)/200)
		plotrix::floating.pie(ypos=51,xpos=-12,x=1,col=1,radius=sqrt(1000)/200)
		text(-12,51,"1000 ind.",pos=4,offset=1.5)
		text(-12,51.25,"100 ind.",pos=4,offset=1)
		text(-12,51.5,"10 ind.",pos=4,offset=.75)

	}
	if (dns=="Arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		if (lattice) {
    mapdist<-lattice::xyplot(lat~long|name,Result3,layout=layout,xlim=c(-8.1,-5.5),main=titulo,xlab=NULL,ylab=NULL,subscripts=TRUE,
	    ylim=c(35.95,37.30),aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-5),labels=as.character(abs(-10:-5))),y=list(at=seq(35,36,by=1),rot=90)),
      as.table=TRUE,panel=function(x,y,subscripts=subscripts) {
        lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
        if (ICESrect) lattice::panel.abline(h=seq(10,60,by=.5),v=seq(-20,10),col=gray(.2),lwd=.5)
        if (ICESlab) lattice::ltext(x=Area$stat_x,y=Area$stat_y-.22,label=Area$ICESNAME,cex=.7,font=2)
        lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	      grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],
	                         default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
				if (max(Result3$numero[subscripts],na.rm=TRUE)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-6,3),c(36.3,36.4,36.5),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-6,3),c(36.3,36.4,36.5),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				lattice::panel.xyplot(x,y,cex=ifelse(Result3$numero[subscripts]>0,sqrt((Result3$numero[subscripts])/escala),.35),
					pch=ifelse(Result3$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
      })}
		else {
		  MapHistAge<-maphistage(gr,esp,camp,"Arsa",age=0,plus=plus,out.dat=T,plot=F,incl2 = F)
		  MapArsa(ICESrect = T,ICESlab = T)
		  title(main=buscaesp(gr,esp),line=2.1,font.main=4)
		  title(main=paste0(ifelse(es,"Edad","Age")," ",age,ifelse(age==plus,"+","")),cex.main=.9,line=1.1)
		  points(lat~long,Result3,pch=21,subset=c(name==paste0("Ed",age,ifelse(plus==age,"+","")) & numero>0),bg="red")
		  points(lat~long,MapHistAge,pch=21,subset=MapHistAge[,age+5]>0,bg="blue")
		  legend("bottomright",c("ICES StatRec","Lances Camp"),pch=21,pt.bg=c("red","blue"),inset=c(.1,.1),bg="white")
		  return()
		}
			}
	# if (dns=="Medi") {
	#   asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
	#   leyenda<-signif(c(1,.5,.25)*leyenda,1)
	#   mapdist<-lattice::xyplot(lat~long|name,Result3,layout=layout,xlim=c(-5.7,5),ylim=c(35,43),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
	#                            subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
	#                            scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),
	#                                                                                                                 "W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=seq(36,42,by=1),rot=90)),as.table=TRUE,
	#                            panel=function(x,y,subscripts=subscripts) {
	#                              lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
	#                              lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	#                              grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[2]],
	#                                                 default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
	#                              if (leg & max(Result3$numero[subscripts],na.rm=TRUE)>0) {
	#                                #lrect(-5.98,36.25, -5.54, 36.54,col="white")
	#                                lattice::panel.xyplot(rep(-4,3),c(39.1,39.6,40.),cex=sqrt((leyenda)/escala),pch=16,col=colo)
	#                                lattice::ltext(rep(-4,3),c(39.1,39.6,40.),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
	#                              }
	#                              if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(Result3$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
	#                                                                   cex=ifelse(Result3$peso[subscripts]>0,sqrt((Result3$peso[subscripts])/escala),.35),col=colo)}
	#                              else {lattice::panel.xyplot(x,y,cex=ifelse(Result3$numero[subscripts]>0,sqrt((Result3$numero[subscripts])/escala),.35),
	#                                                          pch=ifelse(Result3$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
	#                            })
	# }
	if (plot & lattice) {print(mapdist)}
# 	if (out.dat) {
#     if (years) Result3<-Result3$camp
# #    browser()
#     if (!ceros) Result3<-Result3[any(colSums(Result3[,4:c(ncol(Result3)-2)])>0),]
#     Result3[,1:c(ncol(Result3)-1)]  #print()
#     }
# 	else {
    if (!plot) mapdist
 #   }
	}
