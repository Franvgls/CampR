#' Crea un histograma distribución de tallas estratificada especie y campañas corrigiendo el que haya unas campañas a .5 y otras a 1 cm
#'
#'  Histograma de la distribución de tallas media estratificada por sexos a partir de ficheros del camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo. Si se saca más de una especie avisa de que distintas especies pueden estar medidas en diferentes unidades, pero saca títulos y datos como si todos tuvieran las propiedades de la primera.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param cor.5 si T cambia las distribuciones de tallas .5 a 1 cm, sólo sirve para las campañas/peces medidos a 1 cm .5 o todos a .5, distingue si una campaña es a .5 y otra a 1
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param sub Si caracter lo añade como subtítulo en el gráfico
#' @param leg Si T añade leyenda
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param sex Sacar la distribución de tallas por sexos si existen (T), o conjunta (F)
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param clms Número de columnas para ordenar la serie histórica
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param ymax Valor máximo del eje y
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param verbose Si T saca avisos de consistencia en tallas, sino los omite
#' @return Si plot=T saca el gráfico, pero si out.dat=T puede exportar una matriz talla(filas)xCampañas(columnas)
#' @family Distribuciones de tallas
#' @examples dtall.camp(1,63,Psh,"Porc",es=F,sex=F,ti=T,years=T)
#' @examples dtall.camp(1,50,Nsh[c(length(Nsh)-9):length(Nsh)],"Cant",es=F,ti=T,years=T,out.dat=T)
#' @export
dtall.campb<- function(gr,esp,camp,dns,cor.time=TRUE,cor.5=FALSE,ti=FALSE,sub=NA,leg=TRUE,cexleg=1,bw=TRUE,es=TRUE,sex=TRUE,plot=T,idi="l",clms=2,
  layout=NA,excl.sect=NA,ymax=NA,out.dat=FALSE,years=TRUE,verbose=TRUE) {
  library(lattice)
  options(scipen=2)
  if (length(gr)>1) stop("No tiene sentido mezclar distribuciones de tallas de especies en distintos clases taxonómicas")
  esp<-format(esp,width=3,justify="r")
  if (length(esp)>1 | any(esp=="999")) {
    increm<-NULL;medida<-NULL
    for (i in esp) {
      increm<-c(increm,as.numeric(unid.camp(gr,i)["INCREM"]))
      medida<-c(medida,ifelse(unid.camp(gr,i)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm")))
      }
    if (length(unique(increm))>1 | length(unique(medida))>1) stop("Seleccionadas especies medidas en distintas unidades (mm y cm o .5 cm) o a la aleta anal")
    else increm<-unique(increm);medida<-unique(medida)
      }
      else {
    increm<-unid.camp(gr,esp)["INCREM"]
    medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm"))
  }
  if (bw) {
    colbars<-c(gray(.2),gray(.6),"white")
    }
	else {
    colbars<-c("lightyellow","steelblue","yellow1")
    }
	if (es) {sxn<-c("Indet","Machos","Hembras")
		ax<-c(paste0("Talla (",medida,")"),expression("Ind"%*%"lan"^-1))}
	else {sxn<-c("Undet","Male","Female")
		ax<-c(paste0("Length (",medida,")"),expression("Ind"%*%"haul"^-1))}
	sexn<-c("2","3","1")
	sixn<-c("1","2","3")
	dtalln<-c("machos","hembras","indet")
	if (is.logical(ti)) {
		if (ti) {tit<-list(label=buscaesp(gr,esp,id=idi),font=ifelse(idi=="l",4,2),cex=1*cexleg)}
		else {tit<-NULL}
		}
	else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
    }
	ndat<-length(camp)
	for (i in 1:ndat) {
    #browser()
		dtall<-dattal.camp(gr,esp,camp[i],dns,cor.time=cor.time,excl.sect=excl.sect,sex=sex,verbose=verbose)
    names(dtall)<-c("talla","indet") #### sección para corregir datos de tallas a .5 cm dejándolos a 1 cm
		if (!cor.5 & diff(range(dtall$talla))>100) {
		  dtall$talb<- trunc(dtall$talla/10)
		  dtall<-aggregate(indet~talb,dtall,sum)
		  dtall<-filter(dtall,talb>0)
		  names(dtall)<-c("talla","indet")
		  increm<-1
		  medida<-"cm"
		}
		names(dtall)<-c("talla",sexn[which(!is.na(match(dtalln,names(dtall)[2:ncol(dtall)])))])
		sxs<- match(sixn,names(dtall)[2:length(names(dtall))])
		if (sex) {
			ard<-c(NULL,NULL,NULL,NULL)
			if (!is.na(sxs[2])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("2",names(dtall))],rep(2,nrow(dtall)))
			else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(2,nrow(dtall)))
			ard<-rbind(ard,a1)
			if (!is.na(sxs[3])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("3",names(dtall))],rep(3,nrow(dtall)))
			else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(3,nrow(dtall)))
			ard<-rbind(ard,a1)
			if (!is.na(sxs[1])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("1",names(dtall))],rep(1,nrow(dtall)))
			else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(1,nrow(dtall)))
			ard<-rbind(ard,a1)
			}
		else {
			if (ncol(dtall)>2) {
         ard<-as.data.frame(cbind(dtall[,1],rep(camp[i],nrow(dtall)),rowSums(dtall[,c(2:ncol(dtall))]),rep(1,nrow(dtall))))
			}
			else ard<-as.data.frame(cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,2],rep(1,nrow(dtall))))
			}
		if (i==1) a<-ard
		else a<-rbind(a,ard)
		}
	a<-as.data.frame(a)
	names(a)<-c("talla","camp","n","sex")
  if (years) {
    acamp<-a
    acamps<-camp
    camp<-camptoyear(camp)
    a$camp<-camptoyear(a$camp)
    }
	a$camp<-factor(as.character(a$camp),levels=camp)
	a$talla<-as.numeric(as.character(a$talla))
	a$n<-as.numeric(as.character(a$n))
	if (sum(a$n)==0) stop(paste0(ifelse(es,"No hay capturas de la especie ","No catches of species "),buscaesp(gr,esp),ifelse(es," en las campañas seleccionadas"," in surveys selected")))
	a$sex<-factor(as.character(a$sex),levels=c(1:3))
	maxy<-tapply(a$n,a[,c(1,2)],sum)
	maxy[which(is.na(maxy))]<-0
	ylim<-c(0,ifelse(is.na(ymax),max(maxy)*1.05,ymax))
	haysex<-sum(tapply(a$n,a$sex,sum)[c(2,3)])
	if (sex & (haysex != 0)) {
#		if (length(camp)==1) {
			sxn<-sxn[(sxs)]          # sxn<-sxn[which(!is.na(sxs))]
			colbars<-colbars[which(!is.na(sxs))]
			a$sex<-factor((a$sex),exclude=sixn[which(is.na(sxs))])  # a$sex<-factor((a$sex),exclude=sixn[which(is.na(sxs))])
#			}
		}
	else {
		if (bw) colbars<-gray(.3)
		else colbars<-"olivedrab1"
		leg=F
		}
  #browser()
  if (leg & (haysex != 0)) {
  	ddd<-tapply(a$n,a$sex,sum)
    leg<-list(columns=3,space="top",rectangles=list(T,size=5),
		  col=colbars[c(2,3,1)],text=list(labels=sxn[c(3,1,2)],col="black",
		                                  cex=cexleg*ifelse(!plot,.7,.9)))}     #,col=colbars
	else {leg<-NULL}
	xlimi<-c(min(a$talla)*(.95-1),max(a$talla)*1.05)
	if (is.character(sub)) sub=list(label=sub,font=2,cex=cexleg*.9)
  if (length(camp)==1) {
		foo<-lattice::barchart(n~talla,a,groups=a$sex,subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,as.table=T,
			scales=list(alternating=F,tck=c(1,1),
			            x=list(at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
			            rot=45)),
			stack=T,h=F,main=tit,par.strip.text=list(cex=cexleg*.8,font=2),
			xlab=list(label=ax[1],cex=cexleg*1.2),ylab=list(label=ax[2],cex=cexleg*1.2),sub=sub,strip=TRUE,
			panel=function(x,y,...) {lattice::panel.fill(col="white")
#  			media=sum((x)*y*100)/sum(y*100)
				lattice::panel.grid(-1,0,lty=3,col=gray(.2))
#  			lattice::panel.abline(v=media,lty=1)
				lattice::panel.barchart(x,y,col=colbars,...)
    		#lattice::panel.axis(side="bottom",
    		                    # at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
    		                    # labels=round(a$talla/10,1),
    		                    # outside=T,
    		                    # draw.labels = T)
    	#	lattice::ltext(60,3.5,paste("avg=",round(media,1)),cex=.6)
        }
			)
			names(dtall)<-c("talla",dtalln[which(!is.na(match(sexn,names(dtall)[2:ncol(dtall)])))])
			#print(dtall)
			}
	else {
		orden=NULL
    if (any(is.na(layout))) {
			if (ndat>3) layout<-c(clms,ceiling(ndat/clms))
			else {layout<-c(1,ndat)}
			}
    #*browser()
		foo<-lattice::barchart(n~talla|camp,a,groups=a$sex,subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,col=colbars,drop.unused.levels=FALSE,
			scales=list(alternating=F,tck=c(1,0),cex=cexleg*.7,x=list(at=a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
			            rot=45)),stack=T,h=F,main=tit,xlab=list(label=ax[1],cex=cexleg*.9),
			ylab=list(label=ax[2],cex=cexleg*.9),layout=layout,par.strip.text=list(cex=cexleg*.8,font=2),as.table=ifelse(!is.null(orden),F,T),sub=sub,
			panel=function(x,y,...) {
				lattice::panel.fill(col="white")
#  			media=sum((x)*y*100)/sum(y*100)
				lattice::panel.grid(-1,0,lty=3,col="gray60")
				lattice::panel.barchart(x,y,...)
#    		lattice::ltext(60,3.5,paste("avg=",round(media,1)),cex=.6)
				},
			strip = function(...) lattice::strip.default(style=1,...))
		}
  #browser()
	if (plot) {
     	if (bw) {
    colbars<-c(gray(.2),gray(.5),"white")
		lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
    }
	else {
    colbars<-c("lightyellow", "steelblue", "yellow1")
    lattice::trellis.par.set(lattice::col.whitebg())
    }
print(foo)
     }
	if (out.dat) {
    if (years) {
      a<-acamp
      a$n<-as.numeric(as.character(a$n))
      a$talla<-as.numeric(as.character(a$talla))
      camp<-acamps
      }
#    browser()
    tapply0<-tapply(a$n,a[,1:2],sum,na.rm=T)
    tapply0[is.na(tapply0)]<-0
    print(tapply0)
#    print(tapply(a$n,a[,1:2],sum,na.rm=T)[which(rowSums(tapply(a$n,a[,1:2],sum),na.rm=T)>0),camp])
    }
	else {
    if (!plot) foo
    }
	}

