#' Outliers capturas según tallas medidas y relación talla-peso
#'
#' Busca outliers en las capturas en el fichero de tallas a partir de la relación talla-peso y los pesos y tallas muestreadas. El tamaño de los círculos reflejan el numero de peces medidos, y los distintos colores marcan las distintas categorias si existe muestreo por categorías.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param margerr Margen de error que delimita los datos que estan dentro o fuera del intervalo.
#' @param out.dat si T saca los datos estimados, observados y el error correspondiente. Por defecto (si F) saca estos datos, únicamente, de aquellos lances con errores mayores que margerr
#' @param mm Usar T para especies medidas en milímetros 
#' @param areg Para cambiar el coeficiente "a" de la regresión si es distinto al archivado en el CAMP
#' @param breg Para cambiar el coeficiente "b" de la regresión si es distinto al archivado en el CAMP
#' @return Saca una tabla con datos estimados, observados y el error asociado a la relación talla peso y la distribución de tallas. Si out.dat=T crea un data.frame con los datos para todos los lances de la campaña, en caso contrario sólo saca en la consola los que están fuera de los margenes de error
#' @examples qcLW.camp(1,50,"N08","Cant")
#' @family Control de calidad
#' @export
qcLW.camp<- function(gr,esp,camp="P11",dns="Porc",margerr=20,out.dat=FALSE,mm=FALSE,areg=NA,breg=NA) {
  esp<-format(esp,width=3,justify="r")
  ch1<-RODBC::odbcConnect(dsn=dns)
  RODBC::odbcSetAutoCommit(ch1, FALSE)
  tall_esp<-RODBC::sqlQuery(ch1,paste("select LANCE,PESO_M,CATE,TALLA,NUMER from NTALL",camp,
                               " where grupo='",gr,"' and esp='",esp,"'",sep=""))
  fauna<-RODBC::sqlFetch(ch1,paste("FAUNA",camp,sep=""),as.is=TRUE)
  fauna<-fauna[fauna$GRUPO==gr & fauna$ESP==esp,]
  dumblan<-levels(as.factor(tall_esp$LANCE))
  dumblan<-gsub(" ","",dumblan)
  dumbtal<-levels(as.factor(fauna$LANCE))
  dumbtal<-gsub(" ","",dumbtal)
  #  browser()
  if (length(dumblan[!dumblan%in% dumbtal])>0) print(paste("Lances: ",dumbtal[!dumbtal %in% dumblan],
                                                           " sin distribución de tallas de ",buscaesp(gr,esp),sep=""))
  RODBC::odbcClose(ch1)
  ch2<-RODBC::odbcConnect(dsn="CAMP")
  RODBC::odbcSetAutoCommit(ch2, FALSE)
  esps<-RODBC::sqlFetch(ch2,"ESPECIES",as.is=TRUE)
  RODBC::odbcClose(ch2)
  #  tall_esp<-talls[talls$GRUPO==gr & talls$ESP==esp,]
  a<-ifelse(is.na(areg),esps$A[esps$GRUPO==gr & esps$ESP==esp],areg)
  b<-ifelse(is.na(breg),esps$B[esps$GRUPO==gr & esps$ESP==esp],breg)
  if (mm) tall_esp$peso<-(a*((tall_esp$TALLA/10)+.25)^b)*tall_esp$NUMER
  else tall_esp$peso<-(a*(tall_esp$TALLA+.5)^b)*tall_esp$NUMER
  regr<-tapply(tall_esp$peso,tall_esp[,c("LANCE","CATE")],sum,na.rm=TRUE)
  muestr<-tapply(tall_esp$PESO_M,tall_esp[,c("LANCE","CATE")],mean,na.rm=TRUE)
  nmuest<-tapply(tall_esp$NUMER,tall_esp[,c("LANCE","CATE")],sum,na.rm=TRUE)
  dats<-data.frame(lance=NULL,estim=NULL,cate=NULL,obs=NULL,n=NULL)
  for (i in 1:dim(muestr)[2]) {
    dats<-rbind(dats,data.frame(lance=as.numeric(rownames(regr)),estim=as.vector(regr[,i]),cate=i,obs=as.vector(muestr[,i]),n=as.vector(nmuest[,i])))
  }
  dats$error<-(dats$estim-dats$obs)*100/dats$obs
  dats<-dats[!is.na(dats$estim),]
  dats<-dats[order(dats$lance,dats$cate),]
  ylim<-c(range(dats$error)[1]*margerr/10,abs(range(dats$error)[2])*margerr/10)
  #if (max(abs(dats$error))<abs(median(dats$error))+margerr*1.2) ylim<-c(-margerr*1.2,margerr*1.2)+median(dats$error)
  #else ylim<-c(-max(abs(dats$error)),max(abs(dats$error)))
  plot(error~lance,dats,cex=sqrt(dats$n/max(dats$n,na.rm=TRUE))*5,bg=dats$cate+1,pch=21,ylim=ylim,
       main=buscaesp(gr,esp),font.main=4)
  mtext(paste("Campaña",camp," a=",a," b=",b),line=.5,side=3,cex=.8,font=2)
  mtext(expression("Error"==sum ("Peso"-("a" %*%("Tal"+.5)^"b"))),line=.5,side=3,cex=.8,font=2,adj=1)
  abline(h=c(0,median(dats$error)),lty=c(1,2),col=c(gray(.5),1))
  abline(h=c(median(dats$error)-margerr,median(dats$error)+margerr),lty=2,col="red")
  errgr<-dats[(dats$error)> (margerr+median(dats$error)) | dats$error< (median(dats$error)-margerr),]
  if (nrow(errgr)>0) text(errgr$lance,errgr$error,label=errgr$lance,pos=1,font=2,cex=.8)
  print(errgr)
  if (out.dat) dats
}