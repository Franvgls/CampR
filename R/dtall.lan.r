#' Histograma de distribución de tallas de un lance concreto o la distribución media en varios lances
#' 
#' Dibuja el histograma de la distribución de tallas de la especie por sexos (si existen). Si se selecciona más de un lance es la distribución de tallas media en los lances seleccionados, si sólo hay un lance es la total del lance.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos y 5 invertebrados normalmente no medidos)
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña con el lance a representar en el histograma (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lances Lance o lances de los que se quiere sacar la distribución de tallas. Si NA coge todos los lances de la campaña.
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín.
#' @param legend Si T (por defecto) añade la leyenda, si no se saca por sexos no añade información
#' @param bw Gráfico en blanco en negro si T o en color si F.
#' @param es Si T gráfico en castellano, si F gráfico en inglés.
#' @param sex por defecto (T) da valores por sexos si los hay, si solo hay indeterminados funciona igual.
#' @param idi Nombre científico de la especie ("l") o nombre común ("e").
#' @param ymax permite establecer el valor máximo de las ordenadas en el gráfico.Por defecto (NA) se ajusta al valor máximo del número de individuos.
#' @return Saca el gráfico en pantalla, para sacar datos utilizar {\link{dattal.camp}}
#' @family Distribuciones de tallas
#' @examples dtall.lan(1,36,"P08","Porc",lances=c(10:15,17),ti=TRUE)
#' @export
dtall.lan<- function(gr,esp,camp,dns="Cant",lances=NA,ti=FALSE,legend=TRUE,bw=TRUE,es=TRUE,sex=TRUE,idi="l",ymax=NA) {
  if (length(camp)>1) stop("Esta función sólo se puede utilizar para una sola campaña")
  if (length(esp)>1) {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  }
  esp<-format(esp,width=3,justify="r")
  if(!bw) {colbars<-c("lightyellow","steelblue","yellow1")}
  else {colbars<-c("black","white",gray(.5))}
  if (is.logical(ti)) {
    if (ti) {tit<-list(buscaesp(gr,esp,id=idi),font=ifelse(idi=="l",4,2),cex=1)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
  }
  medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm","mm")
  dtall<-dtallan.camp(gr,esp,camp,dns,sex=sex,lances=lances)
  dtall<-cbind(talla=dtall[,1],dtall[,rev(2:length(dtall))]/length(lances))
  sxn<-c("Machos","Hembras","Indet")
  sxs<-tolower(sxn) %in% colnames(dtall[,-1])
  if (es) {sxn<-factor(c("Machos","Hembras","Indet")[sxs],ordered=T)
           ax<-c(paste("Talla (",medida,")",sep=""),expression("Ind"%*%"lan"^-1))}
  else {sxn<-factor(c("Male","Female","Undet")[sxs],ordered=T)
        ax<-c(paste("Length (",medida,")",sep=""),expression("Ind"%*%"haul"^-1))}
  ymax<-ifelse(ncol(dtall)==2,max(dtall[,2]),max(rowSums(dtall[,-1])))*1.05
  leg<-rev(sxn)
  if (ncol(dtall)==3) colbars<-c("white",gray(.5))
  if (ncol(dtall)==2) {
    colbars=colbars[2]
    leg=F
    }
  barplot(t(as.matrix(dtall[,-1])),names.arg=as.character(dtall[,1]),space=0,beside=FALSE,
          legend.text=leg,col=colbars,main=tit,ylim=c(0,ymax),ylab=ax[2],xlab=ax[1],
          cex.lab=.9,cex.axis=.8,cex.names=.8,axis.lty=1)
  box()
  print(dtall)
  if (length(esp)>1|esp=="999") {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  }
}
