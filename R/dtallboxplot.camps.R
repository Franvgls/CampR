#' **Talla media** de la distribución de tallas media estandarizada del conjunto de la campaña **Xxx**
#'
#' Muestra la talla media de la distribución de tallas de la especie *esp* el conjunto de la campaña seleccionada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año comcreto (xx): Demersales "Nxx", Porcupine "Pxx", Arsa primavera "1XX", Arsa otoño "2xx" Medits "Mxx"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" y Mediterráneo "Medi", gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param ymax si F el límite superior del gráfico es el percentil X de la distribución de tallas, si es un valor pone el valor dado en la opción
#' @param notch si T incluye el notch en el boxplot
#' @param outline si T incluye el los outliers en el boxplot
#' @param notch si T incluye el notch en el boxplot
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Devuelve un data.frame con campos: camp,mean.size
#' @seealso {\link{dattal.camp}}
#' @examples dtallboxplot.camps(gr=1,esp=50,camps=Nsh,dns="Cant")
#' @examples dtallmean.camp(gr=2,esp=19,camps=Psh,dns="Porc",graf="miratu")
#' @export
dtallboxplot.camps<- function(gr,esp,camps,dns="Porc",cor.time=TRUE,cor.5=T,excl.sect=NA,plot=TRUE,es=FALSE,idi="l",cexleg=1,col="steelblue",
                              ymax=NA,notch=TRUE,outline=FALSE,ti=TRUE,sub=TRUE,graf=FALSE,xpng=1200,ypng=800,ppng=15) {
  inicio<-dattal.campb(gr,esp,camps[1],dns,cor.5 = T,sex=F)
  result<-data.frame(camp=camps[1],inicio)
  if (length(camps)>1) for (i in camps[2:length(camps)]) {
    inicio<-dattal.campb(gr,esp,i,dns,sex=F,cor.5 = T)
    result<-rbind(result,cbind(camp=i,inicio))
  }
  result$year<-camptoyear(result$camp)
  increm<-unid.camp(gr,esp)["INCREM"]
  medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm"))
  if (es) {
  ax<-c(paste0("Talla media (",medida,")"),"Año")
  }
  else {ax<-c(paste0("Mean length (",medida,")"),"Year")}
  if (plot) {
    if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
    if (is.logical(graf)) par(xaxs="i",yaxs="i")
    if (is.logical(ti)) {
      if (ti) {tit<-list(buscaesp(gr,esp,id=idi),font=ifelse(idi=="l",4,2),cex=1.2*cexleg)}
      else {tit<-NULL}
    }
    else {
      if(is.list(ti)) tit<-ti
      else tit<-list(label=ti)
    }
    kk1<-boxplot(rep(talla+.5,numero)~rep(year,numero),result,ylim=c(0,22))$stats
    boxplot(rep(talla+.5,numero)~rep(year,numero),result,notch=notch,outline=outline,col=col,
            ylim=c(0,ceiling(max(kk1[nrow(kk1),]))),ylab=ax[1],xlab=ax[2],lwd=2,yaxs="i")
    title(main=tit,line=1.8)
    if (is.logical(sub) & TRUE) title(line=.6,cex.main=.9*cexleg,dns)
    if (!is.logical(sub)) title(line=.6,cex=.6*cexleg,sub)
    box()
    if (!is.logical(graf)) {
      dev.off()
      message(paste0("figura: ",getwd(),"/",graf,".png"))
    }
    if (!is.logical(graf)) par(mar=c(3.1, 3.1, 4, 3.1) + 0.1)
  }
  result
  }
