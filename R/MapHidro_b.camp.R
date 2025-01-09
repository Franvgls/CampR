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
MapHidro_b.camp<-function(camp,dns="Cant",ind="t",plot=TRUE,bw=FALSE,ti=TRUE,es=TRUE,out.dat=FALSE,layout=NA,cex.pt=1,
                       cexleg=1,years=TRUE,graf=FALSE,xpng=1200,ypng=800,ppng=15,cuts=4) {
  if (!(ind %in% c("t","s"))) {
    stop(paste("el índice",ind,"no está implementado, índices disponibles: temperatura 't' y salinidad 's'"))
  }
  if (length(camp)>1) {stop("Sólo se pueden sacar imágenes de una campaña, para montar más sacalas a png con graf")}
  dumb<-datlan.camp(camp,dns,redux = T)
  if (ind=="t") {
    if (all(is.na(dumb$temp))) {stop("No existen datos de temperatura en la campaña solicitada")}
    }
  if (ind=="s") {
    if (all(is.na(dumb$sali))) {stop("No existen datos de salinidad en la campaña solicitada")}    #leyenda<-c(0,15,20,25,50)
  }
  if (ti) titulo<-paste(ifelse(es,"Hidrología","Hydrology"),camp)
  #if (ti & !es) #titulo<-list(label=ifelse(ind=="t","Temperature","Salinity"),font=2)
  else titulo<-NULL
  if (bw) {
  }
  else {
  }
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
    asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
    MapNort()
    if (ind=="s") {
    points(lat~long,dumb,pch=21,cex=1.2,bg="navy",subset=sali<quantile(sali,.20,na.rm=T))
    points(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=sali>=quantile(sali,.2,na.rm=T))
    points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=sali>=quantile(sali,.4,na.rm=T))
    points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=sali>=quantile(sali,.6,na.rm=T))
    points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=sali>=quantile(sali,.8,na.rm=T))
    points(lat~long,dumb,pch=4,cex=1.2,subset = is.na(dumb$temp),lwd=2)
    legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$sali,c(.8,.8,.6,.4,.2),
                                                                            na.rm = T),2),"psu"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
           bg="white",title=ifelse(es,"Salinidad","Salinity"),inset=c(.4,.23))
    }
    if (ind=="t") {
      points(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=temp<quantile(temp,.20,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=temp>=quantile(temp,.2,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=temp>=quantile(temp,.4,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=temp>=quantile(temp,.6,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red4",subset=temp>=quantile(temp,.8,na.rm=T))
      points(lat~long,dumb,pch=4,cex=1.2,subset = is.na(dumb$sali),lwd=2)
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$temp,c(.8,.8,.6,.4,.2),na.rm = T),2),"ºC"),pch=21,cex=1.2,
             pt.bg = c("red4","red","orange","lightblue","steelblue"),bg="white",
             title=ifelse(es,"Temparatura","Temperature"),inset=c(.4,.23))
    }
    if (ti) title(main=titulo,font=2,line=2)
  }
  if (substr(dns,1,4)=="Porc" | substr(dns,1,4)=="Pnew") {
    asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
    mapporco()
    if (ind=="s") {
      points(lat~long,dumb,pch=21,cex=1.2,bg="navy",subset=sali<quantile(sali,.20,na.rm=T))
      pointgs(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=sali>=quantile(sali,.2,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=sali>=quantile(sali,.4,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=sali>=quantile(sali,.6,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=sali>=quantile(sali,.8,na.rm=T))
      points(lat~long,dumb,pch=4,cex=1.2,subset = is.na(dumb$sali),lwd=2)
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$sali,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"psu"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Salinity",inset = c(.05))
    }
    if (ind=="t") {
      points(lat~long,dumb,pch=21,cex=1.2,bg="navy",subset=temp<quantile(temp,.20,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=temp>=quantile(temp,.2,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=temp>=quantile(temp,.4,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=temp>=quantile(temp,.6,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=temp>=quantile(temp,.8,na.rm=T))
      points(lat~long,dumb,pch=4,cex=1.2,subset = is.na(dumb$temp),lwd=2)
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$temp,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"ºC"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Temperature",inset = c(.05))

    }
    if (ti) title(titulo,line=2,font=2,sub=sub)
  }
  if (substr(dns,1,4)=="Arsa") {
    asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
    MapArsa()
    if (ind=="s") {
      points(lat~long,dumb,pch=21,cex=1.2,bg="navy",subset=sali<quantile(sali,.20,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=sali>=quantile(sali,.2,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=sali>=quantile(sali,.4,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=sali>=quantile(sali,.6,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=sali>=quantile(sali,.8,na.rm=T))
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$sali,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"psu"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Salinity",inset = c(.1,.3))
    }
    if (ind=="t") {
      points(lat~long,dumb,pch=21,cex=1.2,bg="navy",subset=temp<quantile(temp,.20,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="steelblue",subset=temp>=quantile(temp,.2,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="lightblue",subset=temp>=quantile(temp,.4,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="orange",subset=temp>=quantile(temp,.6,na.rm=T))
      points(lat~long,dumb,pch=21,cex=1.2,bg="red",subset=temp>=quantile(temp,.8,na.rm=T))
      points(lat~long,dumb,pch=4,cex=1.2,subset = is.na(dumb$sali),lwd=2)
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(dumb$temp,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"ºC"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Temperature",inset = c(.1,.3))

    }
    if (ti) title(main=titulo,font=2,line=2,sub=sub)
    }
  if (substr(dns,1,4)=="Medi") {
    asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
    MapIberia(xlims = c(-5.5,4.2),ylims = c(36,43))
    if (ind=="t") {
      points(lat~long,lm10,pch=21,cex=1.2,bg="navy",subset=temp<quantile(temp,.20,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="steelblue",subset=temp>=quantile(temp,.2,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="lightblue",subset=temp>=quantile(temp,.4,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="orange",subset=temp>=quantile(temp,.6,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="red",subset=temp>=quantile(temp,.8,na.rm=T))
      points(lat~long,lm10,pch=4,cex=1.2,subset = is.na(lm10$sali),lwd=2)
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(lm10$temp,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"ºC"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Temperature",inset = c(.1,.3))
    }
    if (ind=="s") {
      points(lat~long,lm10,pch=21,cex=1.2,bg="navy",subset=sali<quantile(sali,.20,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="steelblue",subset=sali>=quantile(sali,.2,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="lightblue",subset=sali>=quantile(sali,.4,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="orange",subset=sali>=quantile(sali,.6,na.rm=T))
      points(lat~long,lm10,pch=21,cex=1.2,bg="red",subset=sali>=quantile(sali,.8,na.rm=T))
      legend("bottomright",legend=paste(c(">","<","<","<","<"),round(quantile(lm10$sali,c(.8,.8,.6,.4,.2),
                                                                              na.rm = T),2),"psu"),pch=21,cex=1.2,pt.bg = c("red","orange","lightblue","steelblue","navy"),
             bg="white",title="Salinity",inset = c(.1,.3))

    }
    if (ti) title(main=titulo,font=2,line=2,sub=sub)
  }
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
  if (out.dat) dumb
}
