#' Gráficos de parámetros del arte con la profundidad
#'
#' @description
#' Crea gráficos comparando los datos del comportamiento del arte en los lances de una campaña con la profundidad entre el IEO y DATRAS
#'
#' @param camp Campaña a representar un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o  "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param sub si TRUE Añade un subtítulo en la parte superior izquierda con la campaña y el trimestre
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T rotulos gráfico en español, si F en inglés
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad por defecto NA no filtra profundidades
#' @param ceros por defecto incluye los valores de 0 de los parámetros
#' @param lan.cex tamaño de las etiquetas del numero de lances por campaña
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cex.leg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples
#' ArteParCompD("N23","Cant",Nlans = F,ti=T)
#' ArteParCompD("N23","Cant",Nlans = T,ti=T,lan.cex=2)
#' @family gear
#' @export
ArteParCompD<-function(camp,dns="Cant",incl2=TRUE,es=T,bw=TRUE,ti=TRUE,sub=TRUE,out.dat=FALSE,
  ceros=TRUE,cex.leg=1.1,years=TRUE,profrange=NA,proflab=F,Nlans=TRUE,lan.cex=.8) {
  options(scipen=2)
  colo<-ifelse(bw,gray(1),"lightblue")
	dumb<-NULL
  lan<-datlan.camp(camp=camp,dns=dns,incl2=incl2,redux=T)
  if (dns=="Cant") landatr<- icesDatras::getHHdata("SP-NORTH",camptoyear(camp),4)
  if (dns=="Porc") landatr<- icesDatras::getHHdata("SP-PORC",camptoyear(camp),3)
  if (dns=="Arsa") landatr<- icesDatras::getHHdata("SP-ARSA",camptoyear(camp),ifelse(substr(camp,1,1)==1,1,4))
  landatr<-dplyr::filter(landatr,HaulVal!="I")
  if (any(!is.na(profrange))) {
    lan<-dplyr::filter(lan,prof>min(profrange) & prof<max(profrange))
    landatr<-dplyr::filter(landatr,Depth>min(profrange) & Depth<max(profrange))
    }
  par(mfcol=c(1,2))
  plot(dista_p~prof,lan,pch=ifelse(Nlans,NA,21),ylim=c(0,hablar::max_(dista_p)*1.1),
       xlim=c(hablar::min_(prof),hablar::max_(prof)*1.1),xlab=paste(ifelse(es,"Prof","Depth"),"(m)"),
       ylab=paste(ifelse(es,"Distancia puertas","Door spread"),"(m)"))
  if (Nlans) text(dista_p~prof,lan,label=lance,cex=1*cex.leg,font=2,pos=4)
  if (ti) title(paste(ifelse(es,"Distancia puertas con profundidad","Door spread vs. depth"),"CAMP IEO"))
  if (sub) mtext(paste0(dns," Q",lan$quarter),3,adj=1,font=1,cex=.8)
  plot(DoorSpread~Depth,landatr,pch=ifelse(Nlans,NA,21),subset = DoorSpread>0,
       ylim=c(0,hablar::max_(DoorSpread)*1.1),xlim=c(hablar::min_(Depth),hablar::max_(Depth)*1.1),
       xlab=paste(ifelse(es,"Prof","Depth"),"(m)"),
        ylab=paste(ifelse(es,"Distancia puertas","Door spread"),"(m)"))
  if (Nlans)  text(DoorSpread~Depth,landatr,label=HaulNo,subset = DoorSpread>0,pos=4,font=2,cex=1*cex.leg)
  if (ti) title(paste(ifelse(es,"Distancia puertas con profundidad","Door spread vs. depth"),"DATRAS"))
  if (sub) mtext(paste0(landatr$Survey," Q",landatr$Quarter),3,adj=1,font=1,cex=.8)
  # plot(abert_h~prof,lan,pch=21,bg=colo,ylim=c(0,hablar::max_(abert_h)*1.1),
  #      ylab=paste(ifelse(es,"Abertura calones","Wing spread"),"(m)"),
  #      xlab=paste(ifelse(es,"Prof","Depth"),"(m)"))
  # if (Nlans) text(abert_h~prof,lan,label=lance,cex=lan.cex*1,font=2,pos=4)
  # if (ti) title(ifelse(es,"Abertura calones con profundidad","Wing spread vs depth"))
  # plot(abert_v~prof,lan,pch=21,bg=colo,ylim=c(0,hablar::max_(abert_v)*1.1),
  #      ylab=paste(ifelse(es,"Abertura vertical","Vertical opening"),"(m)"),
  #      xlab=paste(ifelse(es,"Prof","Depth"),"(m)"))
  # if (Nlans) text(abert_v~prof,lan,label=lance,cex=lan.cex*1,font=2,pos=4)
  # if (ti) title(ifelse(es,"Abertura vertical con profundidad","Vertical opening vs depth"))
  if (out.dat) {
    if (out.dat) merge(lan[,c("lance","dista_p")],landatr[,c("HaulNo","DoorSpread")],by.x="lance",by.y = "HaulNo")
  }
#  par(op)
}
