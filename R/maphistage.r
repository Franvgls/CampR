#' Mapa distribución por edad en la campaña
#' 
#' Mapa de la distribución geográfica por edad de la especie en las campañas solicitadas
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña o campañas a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa". Medits "Medi" (únicamente para sacar datos al IBTS, no gráficos)
#' @param age Edad solicitada 
#' @param plus Edad plus: incluir la edad considerada como plus, solo afecta si se pide como plus la edad solicitada que suma todas las edades mayores
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param es Si T saca los ejes y legendas en español, si F lo saca en inglés
#' @param layout Organización de gráficos en filas columnas c(r,c)
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param years Si T permite sacar los años como nombre de campaña en los paneles lattice de campañas
#' @param mediahora Valor para obtener abundancias por hora si media hora es mayor
#' @return Si out.dat=T devuelve un data.frame con columnas: lan,lat,long,0,1,2,...,Age+ (número de individuos edad 0,1,2...),camp. Si out.dat=F saca el Gráfico en pantalla o como objeto para combinar con otros Gráficos con print.trellis
#' @examples maphistage("1"," 42","N05","Cant",2,ceros=T,out.dat=T)
#' @export
maphistage<-function(gr,esp,camp,dns="Pnew",age,plus=8,cor.time=T,bw=T,ti=T,plot=T,
  out.dat=F,ind="n",idi="l",es=T,layout=NA,ceros=F,years=F,mediahora=1) {
	require(lattice)
	require(grid)
	require(maps)
  options(scipen=2)
	if (plot) trellis.par.set(col.whitebg())
  if (length(esp)>1) {
    stop("Esta función sólo admite una especie")
    }
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camp)
	dumb<-NULL
	for (i in 1:ndat) {
	  if (!is.null(datagegr.camp(gr,esp,camp[i],dns,plus,mediahora=mediahora,cor.time=cor.time))) {
	   	anyo<-ifelse(as.numeric(substr(camp[i],2,3))>50,1900,2000)+as.numeric(substr(camp[i],2,3))
  			dumb<-rbind(dumb,cbind(datagegr.camp(gr,esp,camp[i],dns,plus,cor.time=cor.time),camp=camp[i]))
  			}
	}
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	dumb$camp<-factor(dumb$camp)
	dumb$numero<-dumb[,age+c(5)] # el dato de n?mero se usa s?lo para sacar en la gr?fica la edad solicitada age
  leyenda<-signif(max(dumb$numero)*.9,1)
	escala<-signif(max(dumb$numero),1)*35/100
	if (is.logical(ti)) {
		if (ti) {titulo<-list(label=paste(buscaesp(gr,esp,id=idi),"\n",paste(ifelse(es,"Edad","Age"),paste(age,ifelse(plus==age,"+",""),sep="")),sep=""),
		font=ifelse(idi=="l",4,2),cex=1)}
		else {titulo<-NULL}
		}
	else {titulo<-list(label=ti)}
	if (bw & plot) {colo=gray(.1)
		trellis.par.set("strip.background",list(col=c(gray(.80))))
		}
	else colo=4
	if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=.7,font=2),scales=list(alternating=F,tck=c(1,0),cex=.7,
			x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=T,
			panel=function(x,y,subscripts=subscripts) {
				panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid.polygon(map(Porc.map,"narr",plot=F)[[1]],map(Porc.map,"narr",plot=F)[[2]],
					default.units = "native",gp=gpar(fill=gray(.7)))
					if (max(dumb$numero[subscripts],na.rm=T)>0) {
						panel.xyplot(-12.5,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
						ltext(-12.5,51.2,labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=.8)
					}
					if (ind=="p") {panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
						pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
					else {panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
						pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
					})
			}
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew" | dns=="Medi") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),main=titulo,xlab=NULL,ylab=NULL,subscripts=T,
			ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=.8,font=2),scales=list(alternating=F,tck=c(1,0),cex=.7,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=T,
			panel=function(x,y,subscripts=subscripts) {
				panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid.polygon(map(Nort.map,"Costa",plot=F)[[1]],map(Nort.map,"Costa",plot=F)[[2]],
					default.units = "native",gp=gpar(fill=gray(.8)))
					if (max(dumb$numero[subscripts],na.rm=T)>0) {
						panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
						ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=.7)
						}
				if (ind=="p") {panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.4),
					pch=ifelse(dumb$peso[subscripts]>0,16,19),col=colo)}
				else {panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,20),col=colo)}
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
    if (years) dumb<-dumbcamp
#    browser()
    if (!ceros) dumb<-dumb[any(colSums(dumb[,4:c(ncol(dumb)-2)])>0),]
    print(dumb[,1:c(ncol(dumb)-1)])
    }
	else {
    if (!plot) mapdist 
    }
	}