#' **Talla media** de la distribución de tallas media estandarizada del conjunto de la campaña **Xxx**
#'
#' Muestra la talla media de la distribución de tallas de la especie *esp* el conjunto de la campaña seleccionada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año comcreto (xx): Demersales "Nxx", Porcupine "Pxx", Arsa primavera "1XX", Arsa otoño "2xx" Medits "Mxx"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" y Mediterráneo "Medi", gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Devuelve un data.frame con campos: camp,mean.size
#' @seealso {\link{dattal.camp}}
#' @examples dtallmean.camp(gr=1,esp=50,camps=Nsh,dns="Cant")
#' @examples dtallmean.camp(gr=2,esp=19,camps=Psh,dns="Porc",graf="miratu")
#' @export
dtallmean.camp<- function(gr,esp,camps,dns="Porc",cor.time=TRUE,excl.sect=NA,plot=TRUE,es=FALSE,idi="l",cexleg=1,graf=FALSE,xpng=1200,ypng=800,ppng=15,grid=TRUE,ti=TRUE,sub=TRUE) {
  result<-data.frame(camp=camps[1],mean.size=weighted.mean(x=dattal.camp(gr,esp,camps[1],dns,sex=F)$talla+.5,w=dattal.camp(gr,esp,camps[1],dns,sex=F)$numero))
  if (length(camps)>1) for (i in camps[2:length(camps)]) {
    result<-rbind(result,data.frame(camp=i,mean.size=weighted.mean(x=dattal.camp(gr,esp,i,dns,sex=F)$talla+.5,w=dattal.camp(gr,esp,i,dns,sex=F)$numero)))
  }
  result$year<-camptoyear(result$camp)
  increm<-unid.camp(gr,esp)["INCREM"]
  medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm"))
  if (es) {
  ax<-c(paste0("Talla media (",medida,")"),"Año")}
  else {
  ax<-c(paste0("Mean length (",medida,")"),"Year")}
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
    plot(mean.size~year,result,type="o",lty=1,pch=21,col="navy",bg="steelblue",ylim=c(0,max(result$mean.size)*1.05),
                 ylab=ax[1],xlab=ax[2],lwd=2,yaxs="i")
    title(main=tit,line=1.8)
    if (is.logical(sub) & TRUE) title(line=.6,cex.main=.9*cexleg,dns)
    if (!is.logical(sub)) title(line=.6,cex=.6*cexleg,sub)
    if (grid) grid()
    box()
    if (!is.logical(graf)) {
      dev.off()
      message(paste0("figura: ",getwd(),"/",graf,".png"))
    }
    if (!is.logical(graf)) par(mar=c(3.1, 3.1, 4, 3.1) + 0.1)
  }
  result
  }
