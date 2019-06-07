#' Mapa distribución en Galicia entre tallas tmin y tmax
#'
#' Saca mapas de distribución de una especie entre tallas tmin y tmax para varias campañas  
#' @param gr Grupo de la especie: Sólo hay tallas para 1 peces, 2 crustáceos decápodos y 3 algunos moluscos
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX". Solo Demersales para Galicia
#' @param dns Elige el origen de las bases de datos: sólo Galicia "Cant" o "Cnew"
#' @param tmin Talla mínima del intervalo de tallas a incluir
#' @param tmax Talla máxima del intervalo de tallas a incluir
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ind Permite elegir entre número "n" o peso "p" peso s?lo funciona cuando existen a y b en especies.dbf y se elige s?lo una especie
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param bw gráfico en blanco en negro si T o en color si F
#' @param ti Si T a?ade titulo al mapa, el nombre de la especie en latín
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la funcion es la figura en pantalla, pero los datos en objeto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas y columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del n?mero de la leyenda
#' @param years Si T saca los años como nombre de campa?a en los paneles lattice de campa?as
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples MapGaltal(1,50,Nsh[21:29],"Cant",1,23,layout=c(3,3),out.dat=TRUE,ceros=FALSE,incl2=TRUE)
#' @family mapas
#' @family tallas
#' @export
MapGaltal<-function(gr,esp,camps,dns="Cant",tmin=1,tmax=999,cor.time=TRUE,ind="n",sex=NA,bw=TRUE,ti=TRUE,plot=TRUE,idi="l",cexleg=1,years=TRUE,layout=NA,out.dat=TRUE,leg=TRUE,ceros=TRUE,escmult=.25,incl2=FALSE) {
  if (!c(dns %in% c("Cnew","Cant","Cantred"))) {stop("Mapa de Galicia, dns tiene que ser Cant, Cnew o Cantred")}
  options(scipen=2)
 	lattice::trellis.par.set(lattice::col.whitebg())
  if (length(esp)>1 | esp=="999") {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    if (!is.na(sex)) {
      stop("No se pueden seleccionar sexo para m?s de una especie")
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
	  if (!is.null(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time,incl2,sex,ind))) dumb<-rbind(dumb,cbind(dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time,incl2,sex,ind),camp=camps[i]))
	}
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	if (any(is.na(layout))) {
		if (ndat<4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
    if (ndat>4) layout=c(2,ndat)
		}
	dumb$camp<-factor(dumb$camp)
	dumb<-dumb[(dumb$long<c(-6)|dumb$long==0),]
	if (ind=="p") {
		dumb$peso<-dumb$peso.gr/1000
		leyenda<-signif(max(dumb$peso)*.9,1)
		escala<-signif(max(dumb$peso),1)*35/150 }
	else {
		leyenda<-signif(max(dumb$numero)*.9,1)
		escala<-signif(max(dumb$numero),1)*35/100 }
	if (is.logical(ti)) {
		if (ti) {
      titulo<-list(label=paste(buscaesp(gr,esp,id=idi),"\n",tmin,"-",tmax,medida,ifelse(is.na(sex),"",paste("sexo=",sex))),
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
    lattice::trellis.par.set(strip.background = list(col = grey(7:1/8)))
  }
  else {
    colo=4
    lattice::trellis.par.set(lattice::col.whitebg())
  }
  asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-6))*cos(mean(c(41.82,44.3))*pi/180))
	leyenda<-signif(c(1,.5,.25)*leyenda,1)
	mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-6),main=titulo,xlab=NULL,ylab=NULL,subscripts=TRUE,
		ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=cexleg*.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg*.7,
		x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
		panel=function(x,y,subscripts=subscripts) {
			lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
			grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
				default.units = "native",gp=grid::gpar(fill=gray(.8)))
			if (max(dumb$numero[subscripts],na.rm=TRUE)>0) {
				lattice::panel.xyplot(rep(-8.2,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
				lattice::ltext(rep(-8.2,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1,cex=cexleg*.8
        )
				}
			if (ind=="p") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.4),
				pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
			else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
				pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
			})
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