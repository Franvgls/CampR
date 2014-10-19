#' Histograma de distribución de tallas de un lance concreto
#' 
#' Dibuja un histograma de la distribución de tallas total por sexos (si existen) en los lances seleccionados a partir de ficheros del camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos y 5 invertebrados normalmente no medidos)
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña con el lance a representar en el histograma (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lances Lance o lances de los que se quiere sacar la distribución de tallas. Si NA coge todos los lances de la campaña.
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín.
#' @param legend Si T (por defecto) añade la leyenda, si no se saca por sexos no añade información
#' @param bw Gráfico en blanco en negro si T o en color si F.
#' @param es Si T gráfico en castellano, si F gráfico en inglés.
#' @param sex por defecto (T) da valores por sexos si los hay, si solo hay indeterminados funciona igual.
#' @param idi Nombre científico de la especie ("l") o nombre común ("e").
#' @param ymax permite establecer el valor máximo de las ordenadas en el gráfico.Por defecto (NA) se ajusta al valor máximo del número de individuos.
#' @return Saca el gráfico en pantalla, para sacar datos utilizar {\link{dattal.camp}}
#' @seealso {\link{dtall.camp}} {\link{dtallbarplot}}
#' @examples dtall.lan(1,36,"P08","Pnew",lances=c(10:15,17),ti=T)
#' @export
dtall.lan<- function(gr,esp,camp,dns="Cant",lances=NA,ti=F,legend=T,bw=T,es=T,sex=T,idi="l",ymax=NA) {
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
  dtall<-dtallan.camp(gr,esp,camp,dns,sex=sex,lances=lances)
  dtall<-cbind(talla=dtall[,1],dtall[,rev(2:length(dtall))])
  ymax<-ifelse(ncol(dtall)==2,max(dtall[,2]),max(rowSums(dtall[,-1])))*1.05
  leg=names(dtall)[-1]
  if (ncol(dtall)==2) {
    colbars=colbars[2]
    leg=F
  }
  barplot(t(as.matrix(dtall[,-1])),names.arg=as.character(dtall[,1]),space=0,beside=F,
          legend.text=leg,col=colbars,main=tit,ylim=c(0,ymax),ylab="Número ind.",xlab="Talla",
          cex.lab=.9,cex.axis=.8,cex.names=1)
  box()
  print(dtall)
  if (length(esp)>1|esp=="999") {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  }
}
