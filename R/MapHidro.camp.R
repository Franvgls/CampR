#' Mapa de índices ecológicos para un grupo en una campaña
#'
#' Utiliza los datos del Camp representar la variación geográfica de los datos hidrologicos, temperatura y salinidad
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param ind Elige el valor (t)emperatura o (s)alinidad
#' @param plot Saca el gráfico (si T) o si se salva como objeto se puede componer para componer con otros gráficos de lattice (F)
#' @param ti Añade la Campaña, si F no añade titulo
#' @param sub Si T añade el parámetro (temperatura, salinidad) como subtítulo bajo el gráfico en inglés o español
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T saca los titulos y rotulos en español, si F en inglés
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param cex.pt Varía el tamaño de los puntos en los gráficos
#' @param cexleg varía el tamaño del texto de la leyenda y los ejes
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param graf si F no el gráfico va a pantalla, si nombre fichero va a fichero en el directorio en que está wdf
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Saca el mapa de diversidad en la campaña seleccionada.
#' @examples MapHidro.camp("N24","Cant",ind="t",ti=T)
#' @family mapas
#' @family hidrología
#' @export
MapHidro.camp<-function(camp,dns="Porc",ind="t",plot=TRUE,subtit=TRUE,bw=FALSE,ti=TRUE,es=TRUE,out.dat=FALSE,layout=NA,cex.pt=1,
                       cexleg=1,years=TRUE,graf=FALSE,xpng=1200,ypng=800,ppng=15,cuts=4) {
  if (!(ind %in% c("t","s"))) {
    stop(paste("el índice",ind,"no está implementado, índices disponibles: temperatura 't' y salinidad 's'"))
  }
  lattice::trellis.par.set(lattice::col.whitebg())
  ndat<-length(camp)
  dumb<-NULL
  for (i in 1:ndat) {
    dumb<-rbind(dumb,datlan.camp(camp[i],dns,redux = T))
  }
  if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp)
  }
  dumb$camp<-factor(dumb$camp)
  if (ind=="t") {
    if (all(is.na(dumb$temp))) {stop("No existen datos de temperatura en la campaña solicitada")}
    dumb$tempF<-cut(dumb$temp,breaks=cuts,right=T) #etiquetas leyenda
    dumb$tempC<-cut(dumb$temp,breaks=cuts,labels = heat.colors(cuts),right=T) # colores puntos
    leyenda<-levels(dumb$tempF)
  if (!es) sub<-"Temperature"
  else sub<-sub<-"Temperatura"
       }
  if (ind=="s") {
    if (all(is.na(dumb$sali))) {stop("No existen datos de salinidad en la campaña solicitada")}    #leyenda<-c(0,15,20,25,50)
    dumb$salF<-cut(dumb$sali,breaks=cuts,right=T)  #etiquetas leyenda
    dumb$salC<-cut(dumb$sali,breaks=cuts,labels = terrain.colors(cuts),right=T) # colores puntos
    leyenda<-levels(dumb$salF)
    if (!es) sub<-"Salinity"
    else sub<-sub<-"Salinidad"
  }
  if (ti) titulo<-list(label=ifelse(es,"Hidrología","Hydrology"),font=2)      #ifelse(ind=="t","Temperatura","Salinidad"),font=2)
  #if (ti & !es) #titulo<-list(label=ifelse(ind=="t","Temperature","Salinity"),font=2)
  else titulo<-NULL
  if (bw) {
    lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
    colo=gray(.1)
  }
  else {
    lattice::trellis.par.set("strip.background",list(col="ivory2"))
    colo=4
  }
  if (any(is.na(layout))) {
    if (ndat!=4) layout=c(1,ndat)
    if (ndat==4) layout=c(2,2)
  }
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
#    leyenda<-signif(c(1,.5)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,xlab=NULL,ylab=NULL,
                    ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                                                                                                x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,
                    panel=function(x,y,subscripts) {
                      lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
                      lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=gray(.7)))
                      lattice::panel.xyplot(c(-12.5,-12.5,-12.5,-12.5),c(51.5,51.3,51.1,50.9),cex=1*cex.pt,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-12.5,4),c(51.5,51.3,51.1,50.9),labels=leyenda,pos=4,offset=1,cex=.8)
                      if (ind=="t") {
                        lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$tempC))
                        lattice::panel.xyplot(x,y,subset=is.na(dumb$temp),cex=1*cex.pt,pch="X",col=1)
                      }
                      if (ind=="s") {lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$salC))}
                    }) }
  if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
    asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
#    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),ylim=c(41.82,44.3),main=titulo,xlab=NULL,ylab=NULL,
                             aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                                                                                       x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=(42:44),rot=90)),as.table=TRUE,sub=ifelse(subtit,sub,""),
                             panel=function(x,y,subscripts) {
                               lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
                               lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.2))
                               grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
                                                  default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.7),"bisque")))
                               lattice::ltext(rep(-7,4),c(43.,42.80,42.60,42.4),labels=leyenda,pos=4,offset=1.1,cex=.7)
                               if (ind=="t") {
                                 lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$tempC))
                                 lattice::panel.xyplot(rep(-7,4),c(43.,42.80,42.60,42.4),cex=1*cex.pt,pch=21,col=1,fill=as.character(levels(dumb$tempC)))
                               }
                               if (ind=="s") {
                                 lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$salC))
                                 lattice::panel.xyplot(rep(-7,4),c(43.,42.80,42.60,42.4),cex=1*cex.pt,pch=21,col=1,fill=as.character(levels(dumb$salC)))
                               }
                             })}
  if (substr(dns,1,4)=="Arsa") {
    asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
#    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Arsa.map$range[c(1,2)],ylim=Arsa.map$range[c(3,4)],main=titulo,xlab=NULL,ylab=NULL,
                    aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,x=list(at=c(-7:-5),
                    labels=as.character(abs(-7:-5))),y=list(at=(36:37),rot=90)),as.table=TRUE,
                    panel=function(x,y,subscripts) {
                      lattice::panel.fill(col=ifelse(bw,"white","lightblue1"))
                      lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.7),"bisque")))
                      lattice::panel.xyplot(rep(-5.9,4),c(36.4,36.5,36.6,36.7),cex=1*cex.pt,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-5.9,4),c(36.4,36.5,36.6,36.7),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (ind=="t") {lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$tempC))}
                      if (ind=="s") {lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$salC))}
                    })}
  if (substr(dns,1,4)=="Medi") {
    asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Medits.tot$range[c(1,2)],ylim=Medits.tot$range[c(3,4)],main=titulo,xlab=NULL,
                    ylab=NULL,aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                    x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),"W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=(36:42),rot=90)),as.table=TRUE,
                    panel=function(x,y,subscripts) {
                      lattice::panel.xyplot(Medits.tot$x,Medits.tot$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.8),"bisque")))
                      lattice::panel.xyplot(rep(-4,4),c(39.1,39.4,39.7,40.0),cex=1*cex.pt,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-4,4),c(39.1,39.4,39.7,40.0),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (ind=="t") {lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$tempC))}
                      if (ind=="s") {lattice::panel.xyplot(x,y,cex=1*cex.pt,pch=21,col=1,fill=as.character(dumb$salC))}
                    })}
  if (!plot) return(mapdist)
  else print(mapdist)
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
  if (out.dat) dumb
}
