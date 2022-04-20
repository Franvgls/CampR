#' Histograma de distribución de tallas de un lance concreto o la distribución media en varios lances
#'
#' Dibuja el histograma de la distribución de tallas de la especie por sexos (si existen). Si se selecciona más de un lance es la distribución de tallas media en los lances seleccionados, si sólo hay un lance es la total del lance.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos y 5 invertebrados normalmente no medidos)
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña con el lance a representar en el histograma (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lances Lance o lances de los que se quiere sacar la distribución de tallas. Si NA coge todos los lances de la campaña.
#' @param depth_range NA por defecto, si no poner dos valores, min y max para establecer los límites batimétricos de los lances.
#' @param cor.time T por defecto, si T corrige los lances con tiempo distinto de 30'
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín.
#' @param legend Si T (por defecto) añade la leyenda, si no se saca por sexos no añade información
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param bw Gráfico en blanco en negro si T o en color si F.
#' @param es Si T gráfico en castellano, si F gráfico en inglés.
#' @param sex por defecto (T) da valores por sexos si los hay, si solo hay indeterminados funciona igual.
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param idi Nombre científico de la especie ("l") o nombre común ("e").
#' @param ymax permite establecer el valor máximo de las ordenadas en el gráfico.Por defecto (NA) se ajusta al valor máximo del número de individuos.
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @return Saca el gráfico en pantalla, para sacar datos utilizar {\link{dattal.camp}}
#' @family Distribuciones de tallas
#' @examples dtall.lan(1,36,"P08","Porc",lances=c(10:15,17),ti=TRUE)
#' @export
dtall.lan<- function(gr,esp,camp,dns="Cant",lances=NA,cor.time=TRUE,depth_range=c(NA),ti=FALSE,legend=TRUE,cexleg=1,bw=TRUE,es=TRUE,
                     sex=TRUE,plot=TRUE,idi="l",ymax=NA,out.dat=FALSE) {
  if (length(camp)>1) stop("Esta función sólo se puede utilizar para una sola campaña")
  if (length(esp)>1 | any(esp=="999")) {
    increm<-NULL;medida<-NULL
    for (i in esp) {
      increm<-c(increm,as.numeric(unid.camp(gr,i)["INCREM"]))
      medida<-c(medida,ifelse(unid.camp(gr,i)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm")))
    }
    if (length(unique(increm))>1 | length(unique(medida))>1) stop("Seleccionadas especies medidas en distintas unidades (mm y cm o .5 cm) o a la aleta anal")
    else increm<-increm[1];medida<-medida[1]
  }
  else {
    increm<-c(as.numeric(unid.camp(gr,esp)["INCREM"]))
    medida<-c(ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm")))
  }
  esp<-format(esp,width=3,justify="r")
  sxn<-c("indet","machos","hembras")
  if (bw) {
    colbars<-c(gray(.2),gray(.6),"white")
  }
  else {
    colbars<-c("lightyellow","steelblue","yellow1")
  }
  sexn<-c("2","3","1")
	sixn<-c("1","2","3")
	dtalln<-c("machos","hembras","indet")
  if (is.logical(ti)) {
    if (ti) {tit<-list(buscaesp(gr,esp,id=idi),font=ifelse(idi=="l",4,2),cex=1*cexleg)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
  }
  dtall<-dtallan.camp(gr,esp,camp,dns,cor.time = cor.time,depth_range = depth_range,sex=sex,lances=lances)
  dtall<-cbind(talla=dtall[,1],dtall[,rev(2:length(dtall))]/length(lances))
  sexn<-c("2","3","1")
  sixn<-c("1","2","3")
  dtalln<-c("machos","hembras","indet")
  #sxn<-c("Machos","Hembras","Indet")
  sxs<-tolower(sxn) %in% colnames(dtall[,-1])
  #sxn<-as.character(factor(c("Machos","Hembras","Indet")[sxs],ordered=T))
  if(is.na(ymax)) {ymax<-ifelse(ncol(dtall)==2,max(dtall[,2]),max(rowSums(dtall[,-1])))*1.05}
  leg<-rev(sxn)
  if (ncol(dtall)==4) {
    if (bw) colbars<-c(gray(.2),gray(.6),"white")
    else colbars<-c("lightyellow","steelblue","yellow1")
    }
  if (ncol(dtall)==2) {
    colbars=colbars[2]
    leg=F
  }
#  sxs<- match(sixn,names(dtall)[2:length(names(dtall))])
  sxs<- match(tolower(as.character(sxn)),names(dtall)[2:length(names(dtall))])
  if (sex) {
    ard<-c(NULL,NULL,NULL,NULL)
    if (!is.na(sxs[2])) a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),dtall[,"hembras"],rep(2,nrow(dtall)))
    else a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),rep(0,nrow(dtall)),rep(2,nrow(dtall)))
    ard<-rbind(ard,a1)
    if (!is.na(sxs[3])) a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),dtall[,"indet"],rep(3,nrow(dtall)))
    else a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),rep(0,nrow(dtall)),rep(3,nrow(dtall)))
    ard<-rbind(ard,a1)
    if (!is.na(sxs[1])) a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),dtall[,"machos"],rep(1,nrow(dtall)))
    else a1<-cbind(dtall[,1],rep(camp,nrow(dtall)),rep(0,nrow(dtall)),rep(1,nrow(dtall)))
    ard<-rbind(ard,a1)
  }
  else {
    if (ncol(dtall)>2) {
      ard<-as.data.frame(cbind(dtall[,1],rep(camp[i],nrow(dtall)),rowSums(dtall[,c(2:ncol(dtall))]),rep(1,nrow(dtall))))
    }
    else ard<-as.data.frame(cbind(dtall[,1],rep(camp,nrow(dtall)),dtall[,2],rep(1,nrow(dtall))))
  }
  a<-ard
  a<-as.data.frame(a)
names(a)<-c("talla","camp","n","sex")
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
  #colbars<-colbars[which(!is.na(sxs))]
  a$sex<-factor((a$sex),exclude=sexn[which(is.na(sxs))])  # a$sex<-factor((a$sex),exclude=sixn[which(is.na(sxs))])
  #			}
}
else {
  if (bw) colbars<-gray(.3)
  else colbars<-"olivedrab1"
  leg=F
}
if (es) {sxn<-c("Indet","Machos","Hembras")
ax<-c(paste0("Talla (",medida,")"),expression("Ind"%*%"lan"^-1))}
else {sxn<-c("Undet","Male","Female")
ax<-c(paste0("Length (",medida,")"),expression("Ind"%*%"haul"^-1))}
if (!is.logical(leg) & (haysex != 0)) {
  ddd<-tapply(a$n,a$sex,sum)
  leg<-list(columns=3,space="top",rectangles=list(T,size=5),
            col=c(gray(.2),gray(.6),gray(.99)),text=list(labels=sxn[c(1:3)],col="black",
                                            cex=cexleg*ifelse(!plot,.7,.9)))}     #,col=colbars
else {leg<-NULL}
xlimi<-c(min(a$talla)*(.95-1),max(a$talla)*1.05)
if (!any(is.na(depth_range))) 	{
#  tt="Depth"
if (depth_range[1]==0) sub<-list(label=bquote("Depth " <=.(format(paste(depth_range[2],"m")))),font.sub=2,cex=cexleg*1.1)
if (depth_range[2]==999) sub<-list(font.sub=2,label=bquote("Depth: " >=.(format(paste0(depth_range[1],"m")))),cex=cexleg*1.1)
if (depth_range[1]!=0 & depth_range[2]!=999) sub<-list(font.sub=2,label=paste("Depth range: ",depth_range[1],"-",depth_range[2],"m"),cex=cexleg*1.1)
if (depth_range[1]==0 & depth_range[2]==999) sub<-NA #list(font.sub=2,label=paste(depth_range[1],"-",depth_range[2],"m"),cex=cexleg*.9)
}
if (is.character(sub)) sub=list(label=sub,font=2,cex=cexleg*.9)
  foo<-lattice::barchart(n~talla,a,groups=factor(a$sex),subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,
                         scales=list(alternating=F,tck=c(1,1),
                                     x=list(at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
                                            rot=45)),
                         stack=T,h=F,main=tit,par.strip.text=list(cex=cexleg*.8,font=2),
                         xlab=list(label=ax[1],cex=cexleg*1.2),ylab=list(label=ax[2],cex=cexleg*1.2),sub=sub,strip=TRUE,
                         panel=function(x,y,...) {lattice::panel.fill(col="white")
                           #  			media=sum((x)*y*100)/sum(y*100)
                           lattice::panel.grid(-1,0,lty=3,col=gray(.2))
                           #  			lattice::panel.abline(v=media,lty=1)
                           lattice::panel.barchart(x,y,col=colbars[c(2,3,1)],...)
                           #lattice::panel.axis(side="bottom",
                           # at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
                           # labels=round(a$talla/10,1),
                           # outside=T,
                           # draw.labels = T)
                           #	lattice::ltext(60,3.5,paste("avg=",round(media,1)),cex=.6)
                         }
  )
  names(dtall)<-c("talla",dtalln[which(!is.na(match(sexn,names(dtall)[2:ncol(dtall)])))])
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

# a<-dtall
#   names(a)<-c("talla","n")
#   foo<-lattice::barchart(n~talla,a,groups=a$sex,subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,
#                          scales=list(alternating=F,tck=c(1,1),
#                                      x=list(at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
#                                             rot=45)),
#                          stack=T,h=F,main=tit,par.strip.text=list(cex=cexleg*.8,font=2),
#                          xlab=list(label=ax[1],cex=cexleg*1.2),ylab=list(label=ax[2],cex=cexleg*1.2),sub=sub,strip=TRUE,
#                          panel=function(x,y,...) {lattice::panel.fill(col="white")
#                            #  			media=sum((x)*y*100)/sum(y*100)
#                            lattice::panel.grid(-1,0,lty=3,col=gray(.2))
#                            #  			lattice::panel.abline(v=media,lty=1)
#                            lattice::panel.barchart(x,y,col=colbars,...)
#                            #lattice::panel.axis(side="bottom",
#                            # at= a$talla[abs(round(a$talla/10,1)-round(a$talla/10))==.5 | abs(round(a$talla/10,1)-round(a$talla/10))==0],
#                            # labels=round(a$talla/10,1),
#                            # outside=T,
#                            # draw.labels = T)
#                            #	lattice::ltext(60,3.5,paste("avg=",round(media,1)),cex=.6)
#                          }
#                          # barplot(t(as.matrix(dtall[,-1])),names.arg=as.character(dtall[,1]),space=0,beside=FALSE,
#   #         legend.text=leg,col=colbars,main=tit,ylim=c(0,ymax),ylab=ax[2],xlab=ax[1],
#   #         cex.lab=.9,cex.axis=.8,cex.names=.8,axis.lty=1)
#   # box()
#   )
#   print(dtall)
# }
