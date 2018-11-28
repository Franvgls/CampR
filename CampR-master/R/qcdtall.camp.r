#' Revisión del histograma de distribución de tallas
#'
#' Revisa el histograma de distribución de tallas total de la especie en un lance concreto de una campaña.
#' Muestra la distribución de talla de cada categoría,los factores de ponderación por categoría, el número total de individuos medidos,
#' y el número ideal de individuos a medir (10 por talla: Gerritsen & McGrath 2007)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lance Permite seleccionar el lance
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param legend Si T añade leyenda
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param ymax Valor máximo del eje y
#' @return Saca en pantalla un gráfico con el histograma de individuos medidos divididos en sus categorías y ponderaciones, ademas del numero ideal de individuos medidos
#' @references Gerritsen & McGrath. 2007. Precision estimates and suggested sample sizes for length-frequency data.Fish. Bull. 106: 116-120)
#' @seealso {\link{dtall.camp}}
#' @examples qcdtall.camp(1,43,"P08","Porc",lance=35)
#' @family Control de calidad
#' @export
qcdtall.camp<- function(gr,esp,camp="P12",dns="Porc",lance,ti=FALSE,legend=TRUE,idi="l",ymax=NA) {
  esp<-format(esp,width=3,justify="r")
  lance<-format(lance,width=3,justify="r")
  ch1<-RODBC::odbcConnect(dsn=dns)
  RODBC::odbcSetAutoCommit(ch1, FALSE)
  ntalls<-RODBC::sqlQuery(ch1,paste("select lance,peso_gr,peso_m,cate,talla,sexo,numer from NTALL",camp,
                             " where grupo='",gr,"' and esp='",esp,"' and lance='",lance,"'",sep=""))
  RODBC::odbcClose(ch1)
  ntalls$wgnum<-ntalls$numer*ntalls$peso_gr/ntalls$peso_m
  dtalls1<-tapply(ntalls$wgnum,ntalls[,c("talla","cate")],sum,na.rm=TRUE)
  dtalls2<-tapply(ntalls$numer,ntalls[,c("talla","cate")],sum,na.rm=TRUE)
  dtalls<-data.frame(talla=as.numeric(rownames(dtalls1)),tot=rowSums(dtalls1,na.rm=TRUE))
  ordcat<-tapply(ntalls$talla[ntalls$numer>0],ntalls[ntalls$numer>0,c("cate")],min,na.rm=TRUE)
  ncat<-length(ordcat)
  wghts<-tapply(ntalls$peso_gr/ntalls$peso_m,ntalls$cate,mean,na.rm=TRUE)
  smps<-tapply(ntalls$peso_m,ntalls$cate,mean,na.rm=TRUE)
  dumb<-as.data.frame(c(1:(trunc(max(dtalls[,1])/10)*10+10)))
  names(dumb)<-"talla"
  dtall<-merge(dumb,dtalls,by="talla",all.x=TRUE)
  #  dtall$tot[is.na(dtall$tot)]<-0
  for (i in 1:ncol(dtalls2)) {
    dumb<-data.frame(talla=as.numeric(rownames(dtalls2)),n=as.vector(dtalls2[,i]))
    names(dumb)<-c("talla",i)
    dtall<-merge(dtall,dumb,by="talla",all.x=TRUE)
  }
  dtall[is.na(dtall)]<-0
  leg<-paste("Categ.",names(ordcat)[order(ordcat)])
  ntots<-colSums(dtall,na.rm=TRUE)
  opar<-par(no.readonly=TRUE)
  #  if (length(wghts)>1) par(mfrow=c(2,2))
  par(mgp=c(3,0.7,0.7),cex.axis=.7)
  barplot(dtall$tot,main=buscaesp(gr,esp),font.main=4,space=0,axes=FALSE,ylim=c(0,max(dtall$tot)*1.1))   # names=dtall$talla,
  axis(2,cex.axis=.8,line=0)
  axis(1,at=seq(1,nrow(dtall),by=3)+.5,labels=dtall$talla[seq(1,nrow(dtall),by=3)],cex.axis=.8,line=0)
  box()
  barplot(t(as.matrix(dtall[,3:ncol(dtall)])),add=TRUE,col=2:5,space=0,axes=FALSE)
  if (legend) legend("topright",rev(leg),fill=2:5,inset=.05)
  #  browser()
  valadj<-seq(0,1,length.out=ncat+2)[2:(ncat+1)]
  counts<-0
  mtext(paste("Campaña:",camp,"    No. lance:",lance),line=.5,side=3,cex=.8,font=2)
  for (i in names(ordcat)[order(ordcat)]) {
    counts<-counts+1
    mtext(paste("Categ.",i,sep=""),line=1.8,side=1,cex=.8,font=2,adj=valadj[counts])
    mtext(paste("n=",round(ntots[i],2),"F.pond=",round(wghts[i],2)),line=2.8,side=1,cex=.8,font=2,adj=valadj[counts])
    if (wghts[i]>1) mtext(paste("n min. ideal=",sum(dtall[,i]>0)*10),side=1,line=3.8,cex=.8,font=2,adj=valadj[counts])
  }
  par(opar)
}