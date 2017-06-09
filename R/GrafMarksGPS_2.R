#' Gráficos de dragas o CTDs a partir de las marcas del PescaWin con leyenda móvil
#'
#' Gráfica todas las dragas o CTDs de una campaña siempre que se hayan marcado con el PescaWin durante la misma
#' Requiere que los ficheros de marcas del PescaWin estén en los directorios "C:/GPS/mrk/porcupine/" para Porcupine o "C:/GPS/mrk/Norte/" para Demersales. Muestra el gráfico y queda pendiente de colocar la leyenda
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" 
#' @param event Elige el evento que se representa, dragas o CTDs
#' @param add Si T añade los puntos al gráfico previo, si F saca el gráfico nuevo
#' @param ti Si T incluye un título con el nombre de la campaña
#' @examples GrafMarksGPS_2("N14","Cant")
#' @family mapas
#' @family PescaWin
#' @export
GrafMarksGPS_2<-function(camp,dns="Cant",event="Draga",label="Dredges",add=FALSE,ti=TRUE,years=TRUE,pch=NA,ptbg=NA,xlims=NA,ylims=NA,cuadr=TRUE,es=T,places=TRUE,bw=FALSE) {
  if (substr(event,1,5)!="Draga" & event!="CTD") stop("Sólo se aceptan Dragas o CTD")
  if(dev.cur() == 1) dev.new()
  if (is.na(ptbg)) {ptbg<-ifelse(add,"red","blue")} else ptbg==ptbg
  if (substr(dns,1,4)=="Cant" |substr(dns,1,4)=="Cnew") {
    di<-"norte"
    fil<-paste0(di,"_",camp,".mrk")
    if (!add) {
      if (any(!is.na(xlims))) {MapNort(xlims=xlims,ylims=ylims,cuadr=cuadr,es=es,places=places,bw=bw)} else MapNort(cuadr=cuadr,places=places,bw=bw,es=es)
    }
    legend(locator(1),paste(label),pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,bty="n")
    if (substr(dns,1,4)=="Porc" |substr(dns,1,4)=="Pnew") {
    di<-"Porcupine"    
    fil<-paste0("Porcupin_",camp,".mrk")
    if (!add) {
      if (any(!is.na(xlims))) {mapporco(xlims=xlims,ylims=ylims,cuadr=cuadr)} else mapporco(cuadr=cuadr)
    }
    legend(locator(1),camp,pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,bty="n")
    }
  if (dns=="Arsa") {
    if (dns=="Arsa") {
      di<-"arsa"
      fil<-paste0("cadiz_",camp,".mrk")
      if (!add) {
        if (any(!is.na(xlims))) {MapArsa(xlims=xlims,ylims=ylims,places=places,cuadr=cuadr,bw=bw)} else MapArsa(places=places,cuadr=cuadr,bw=bw)
      }
      legend(locator(1),paste0(event,"s ",camp),pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,inset=ifelse(add,c(.1,.15),c(.15,.15)),bty="o",bg="white")
    }
  }
  direc<-paste0('c:/gps/mrk/',di,"/",fil)
  mrks<-read.csv(direc,header=F)
  mrks$V3<-as.character(mrks$V3)
  if (nrow(mrks[substr(mrks$V3,1,nchar(event))==event,])==0) stop(paste0("No existen ",paste0(event,"s")," en ",camp))
  points(V2~V1,mrks[substr(mrks$V3,1,nchar(event))==event,],pch=ifelse(is.na(pch),25,pch),bg=ptbg,cex=1)
  if (ti & !add) title(paste0(event,"s"))
  }
  }
#  legend("bottomright","tonto B",pch=ifelse(is.na(pch),25,pch),inset=c(.5,.55))