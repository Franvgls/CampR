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
#' @param mm Usar T para especies medidas en milímetros
#' @param areg Para cambiar el coeficiente "a" de la regresión si es distinto al archivado en el CAMP
#' @param breg Para cambiar el coeficiente "b" de la regresión si es distinto al archivado en el CAMP
#' @return Saca en pantalla un gráfico con el histograma de individuos medidos divididos en sus categorías y ponderaciones, ademas del numero ideal de individuos medidos
#' @references Gerritsen & McGrath. 2007. Precision estimates and suggested sample sizes for length-frequency data.Fish. Bull. 106: 116-120)
#' @seealso {\link{dtall.camp}}
#' @examples qcdtall.camp(1,43,"P08","Porc",lance=35)
#' @family Control de calidad
#' @export
qcdtall.camp<- function(gr,esp,camp="P12",dns="Porc",lance,ti=FALSE,legend=TRUE,idi="l",ymax=NA,mm=FALSE,areg=NA,breg=NA) {
  esp<-format(esp,width=3,justify="r")
  lance<-format(lance,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  # tall_esp<-DBI::dbGetQuery(ch1,paste0("select lance,peso_m,cate,talla,numer from NTALL",camp,
  #                                      " where grupo='",gr,"' and esp='",esp,"'"))
  ntalls<-DBI::dbGetQuery(ch1,paste("select lance,peso_gr,peso_m,cate,talla,sexo,numer from NTALL",camp,
                             " where grupo='",gr,"' and esp='",esp,"' and lance='",lance,"'",sep=""))
  fauna<-DBI::dbGetQuery(ch1,paste0("select lance,grupo,esp,peso_gr,numero from FAUNA",camp,
                                    " where grupo='",gr,"' and esp='",esp,"' and lance='",lance,"'",sep=""))
  DBI::dbDisconnect(ch1)
  ch2<-DBI::dbConnect(odbc::odbc(), "Camp")
  esps<-DBI::dbGetQuery(ch2,"select * from especies")
  DBI::dbDisconnect(ch2)
  #  tall_esp<-ntalls[ntalls$GRUPO==gr & ntalls$ESP==esp,]
  a<-ifelse(is.na(areg),esps$A[esps$GRUPO==gr & esps$ESP==esp],areg)
  b<-ifelse(is.na(breg),esps$B[esps$GRUPO==gr & esps$ESP==esp],breg)
  if (mm) ntalls$peso<-(a*((ntalls$talla/10)+.25)^b)*ntalls$numer
  else ntalls$peso<-(a*(ntalls$talla+.5)^b)*ntalls$numer
  ntalls$wgnum<-ntalls$numer*ntalls$peso_gr/ntalls$peso_m
  ntalls$wgpeso<-(a*(ntalls$talla+.5)^b)*ntalls$wgnum
  sop<-sum(ntalls$wgpeso)
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
  if (ncol(dtall)>3) bio.cats<-round(colSums((a*(dtall[,1]+.5)^b)*dtall[,3:ncol(dtall)]))
  else bio.cats<-sum((a*(dtall[,1]+.5)^b)*dtall[,2])
  bio.mue<-round(tapply(ntalls$peso_m,ntalls$cate,mean))
  leg<-paste("Categ.",names(ordcat)[order(ordcat)])
  ntots<-colSums(dtall,na.rm=TRUE)
  opar<-par(no.readonly=TRUE)
  #  if (length(wghts)>1) par(mfrow=c(2,2))
  par(mar=c(7.1,4.1,4.1,2.1),mgp=c(4,0.7,0.7),cex.axis=.7)
  barplot(dtall$tot,main=buscaesp(gr,esp),font.main=4,space=0,axes=FALSE,ylim=c(0,max(dtall$tot)*1.1))   # names=dtall$talla,
  axis(2,cex.axis=.8,line=0)
  axis(1,at=seq(1,nrow(dtall),by=3)+.5,labels=dtall$talla[seq(1,nrow(dtall),by=3)],cex.axis=.8,line=0)
  box()
  barplot(t(as.matrix(dtall[,3:ncol(dtall)])),add=TRUE,col=2:5,space=0,axes=FALSE)
  if (legend) legend("topright",rev(leg),fill=2:5,inset=.05)
  #  browser()
  counts<-0
  mtext(paste("Campaña:",camp,"    No. lance:",lance),line=.5,side=3,cex=.8,font=2)
  mtext(paste0("Peso= ",fauna$peso_gr,"g, Bio= ",round(sop),"g, SOP ratio= ",round(sop/fauna$peso_gr,2)),side=3,line=-1,font=2,cex=.8)
  for (i in names(ordcat)[order(ordcat)]) {
    counts<-counts+1
    mtext(paste0("Categ.",i),line=1.8,side=1,cex=.8,font=2,at=mean(rep(dtall$talla,dtall[,i])))
    if(ncol(dtall)>3) mtext(paste0("Peso= ",bio.cats[i],"g, Bio= ",bio.mue[i],"g, Sop.ratio= ",round(bio.mue[i]/bio.cats[i],2)),line=2.8,side=2,cex=.8,font=2,at=mean(rep(dtall$talla,dtall[,i])))
    mtext(paste("n=",round(ntots[i],2),"F.pond=",round(wghts[i],2)),line=3.8,side=1,cex=.8,font=2,at=mean(rep(dtall$talla,dtall[,i])))
    if (wghts[i]>1) mtext(paste("n min. ideal=",sum(dtall[,i]>0)*10),side=1,line=4.8,cex=.8,font=2,at=mean(rep(dtall$talla,dtall[,i])))
  }
  par(opar)
}
