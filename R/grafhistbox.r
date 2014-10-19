#' Evolución en biomasa o abundancia de la especie en la serie histórica
#' 
#' Gráfico de cajas con la evolución en biomasa o abundancia en la serie histórica
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Codigo de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps campañas de la serie de datos a representar en el gráfico de abundancias Demersales Nsh, Porcupine Psh, Arsa primavera Ash y Arsa otoño 2sh
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T el gráfico está en kgs, si F en gramos
#' @param ci.lev El intervalo de confianza a representar
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param SE Si T dibuja las cajas representando el error estándar, si F no aparecen la cajas
#' @param es Si T ejes y unidades en español, si F en inglés
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sector Alternativa a excl.sect para un sólo sector. Si especificado como carácter solo toma el sector elegido
#' @param ti Si T muestra el título con el nombre de la especie (y el sector si se elige sect o excl.sect si falso no muestra nada. También se puede dar un valor y aparece el texto incluido o dar un list(label=(título), font, cex)
#' @param Nas Si F no representa las cajas en los sectores/estratos en los que algún sector o estrato tiene un sólo lance. Si T utiliza el valor del estrato y evita los NAs
#' @param ymax Valor máximo del eje y
#' @param mar Si se quiere dejar un margen ya establecido hacerlo igual a F
#' @param tline Si T dibuja una línea de tendencia a traves de un glm con los datos de abundancia. gráficos evaluación MSFD.
#' @param years Si T saca los años como nombre de campaña en el eje de las equis en vez de de campañas
#' @param subtit Añade un subtítulo con el valor que se le ponga si no F
#' @param cex.main Sirve para modificar los tamaños de letra del gráfico, funciona con ejes y títulos. Por defecto en 1.1
#' @param mar Sirve para definir los margenes de la figura alrededor del gráfico mismo. Por defecto es NA y deja los definicos por defecto por R. Si se definen tiene que ser un vector con cuatro valores numéricos en líneas: c(bottom,left,upper,right)
#' @return Crea una gráfica de evolución de las abundancias en biomasa o número y devuelve en consola un data.frame con columnas: avg,SE (error estándar),camp
#' @seealso grafhistbox.comp {\link{grafhistbox.comp}}
#' @examples grafhistbox(1,45,Nsh[7:27],"Cant",es=F,years=T,tline=T,ti=T,subtit=T)
#' @export
grafhistbox<-function(gr,esp,camps,dns="Pnew",ind="p",cor.time=T,kg=T,ci.lev=.8,idi="l",SE=T,
  es=T,excl.sect=NA,sector=NA,ti=T,Nas=F,ymax=NA,mar=NA,tline=F,years=F,subtit=F,cex.main=1.1) {
	library(boot)
  options(scipen=2)
  if (length(sector)>1) {
    stop("Para calcular más de un sector utilice excl.sect quitando los no deseados")
    } 
  op<-par("mar")
	if (any(is.na(mar))) par(mar=c(4, 4.5, 2.5, 2.5) + 0.1)
  else par(mar=mar,mgp=c(2.8,.8,0))
#  par(mgp=c(2,ifelse(is.na(ymax),.7,1.5),0))
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camps)
	dumb<-NULL
	dumbSETot<-data.frame(avg=NULL,SE=NULL,camp=NULL)
	for (i in 1:ndat) {
		dumb<-rbind(dumb,cbind(datos.camp(gr,esp,camps[i],dns,cor.time=cor.time,kg=kg),camp=camps[i]))
		dumbSE<-CV.camp(gr,esp,camps[i],dns,cor.time=cor.time,Nas=Nas,ind=ind,excl.sect=excl.sect)
		if (is.na(sector)) {
			dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSE$total[1],SE=dumbSE$total[2],camp=camps[i]))
			}
		else {
			if (nchar(sector)==2) {
				dumbSect<-dumbSE$locales[,which(colnames(dumbSE$locales)==sector)]
				dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
				}
			else {
				if (!is.na(match(sector,colnames(dumbSE[[2]]))>0)) {
					dumbSect<-dumbSE[[2]][,which(colnames(dumbSE[[2]])==sector)]
					dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
					}
				if (!is.na(match(sector,colnames(dumbSE[[3]])))) {
					dumbSect<-dumbSE[[3]][,which(colnames(dumbSE[[3]])==sector)]
					dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
					}
				}
			}
		}
	if (any(!is.na(excl.sect))) {
		# print(dumb)
	  dumb$sector<-gsub("NA","N",dumb$sector) 
		for (i in 1:length(excl.sect)) {if(length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
		dumb$sector<-factor(as.character(dumb$sector))
		}
	dumb$camp<-factor(dumb$camp)
	dumbSETot$camp<-factor(dumbSETot$camp)
	if (ind=="p") {
		if (any(is.na(sector))){
			if (ci.lev>0) dumb.env<-envelope(boot(dumb$peso,strmean.camps,1000,stype="f",strata=dumb$sector,sector=dumb$sector,
				area=dumb$arsect,camps=dumb$camp),level=ci.lev)
			dumb.mean<-strmean.camps(dumb$peso,dumb$sector,dumb$arsect,camps=dumb$camp)}
		else {
			if (ci.lev>0) dumb.env<-envelope(boot(dumb$peso[grep(sector,as.character(dumb$sector))],strmean.camps,1000,stype="f",
				strata=dumb$sector[grep(sector,as.character(dumb$sector))],sector=dumb$sector[grep(sector,as.character(dumb$sector))],
				area=dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))]),level=ci.lev)
			dumb.mean<-strmean.camps(dumb$peso[grep(sector,as.character(dumb$sector))],dumb$sector[grep(sector,as.character(dumb$sector))],
				dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))])}
			yetiq<-ifelse(es,expression("Yst"~~("kg"%*%"lan"^-1)),expression("kg"%*%"haul"^-1))
		}
	else {
		if (is.na(sector)){
			if (ci.lev>0) dumb.env<-envelope(boot(dumb$num,strmean.camps,1000,stype="f",strata=dumb$sector,sector=dumb$sector,
			area=dumb$arsect,camps=dumb$camp),level=ci.lev)
		dumb.mean<-strmean.camps(dumb$num,dumb$sector,dumb$arsect,camps=dumb$camp)}
		else {
			if (ci.lev>0) dumb.env<-envelope(boot(dumb$num[grep(sector,as.character(dumb$sector))],strmean.camps,1000,stype="f",
				strata=dumb$sector[grep(sector,as.character(dumb$sector))],sector=dumb$sector[grep(sector,as.character(dumb$sector))],
				area=dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))]),level=ci.lev)
			dumb.mean<-strmean.camps(dumb$num[grep(sector,as.character(dumb$sector))],dumb$sector[grep(sector,as.character(dumb$sector))],
				dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))])}
			yetiq<-ifelse(es,expression("Ind"%*%"lan"^-1),expression("Ind"%*%"haul"^-1))
		}
	xetiq<-ifelse(es,ifelse(years,"Año","Campaña"),ifelse(years,"Year","Survey"))
	if (any(is.na(sector))) especie<-buscaesp(gr,esp,idi)
	else {especie<-sector
		idi="e"}
	if (is.na(ymax)) ymax<-max(.5,ifelse(ci.lev>0,max(dumb.env$point[1,]),max(dumbSETot$SE+dumbSETot$avg,na.rm=T))*1.05)
	plot(dumb.mean,xlab=xetiq,ylab=yetiq,ylim=c(0,ymax),axes=F) 
	rect(-1000,-1000,10^5,10^5,col="white")
	if (is.logical(ti)) {
		if (ti) {title(main=ifelse(any(is.na(sector)),especie,paste(especie,sector)),
		  font.main=ifelse((idi!="l" | any(esp=="999")),2,4),line=ifelse(any(is.character(subtit),subtit),1.5,1))}
		}
	else {title(main=ti,font.main=4,line=1.1)}
	if (is.logical(subtit)) {
		if (subtit) {title(main=ifelse(ind=="p",ifelse(es,"Biomasa","Biomass"),ifelse(es,"Número","Number")),
		font.main=2,line=.5,cex.main=.9)}
		}
	else title(main=subtit,line=.5,font.main=2,cex.main=0.9)
	grid(nx=NA,ny=NULL,col=gray(.4))
	if (ci.lev>0) {
    lines(1:ndat,dumb.env$point[1,],lty=1)
  	lines(1:ndat,dumb.env$point[2,],lty=1)
    }
	if (tline) abline(lm(dumb.mean~c(1:ndat)),lty=2,col=2,lwd=2)
  if (SE) {
		for (i in 1:ndat) {
			rect(i-.15,dumbSETot$avg[i]+dumbSETot$SE[i],i+.15,dumbSETot$avg[i]-dumbSETot$SE[i],col=gray(.8))
			}
		points(dumbSETot$avg,pch=16,cex=.6)
		}
	box()
	axis(2,las=2,cex.axis=.9)
  if (years) axis(1,at=1:ndat,labels=camptoyear(camps),las=1,cex.axis=.9)
  else axis(1,at=1:ndat,labels=camps,las=1,cex.axis=.9)
	if(ci.lev>0) axis(4,at=dumb.env$point[,ndat],labels=rev(paste(round(dumb.env$k.pt/10,0),"%")),
		tick=F,cex.axis=.7,las=1,line=-.5)
	par(op)
  dumbSETot
	}
# grafhistbox(1,218,Psh,"Pnew",es=F)
# grafhistbox(1,218,Psh,"Pnew",es=F,years=T)
