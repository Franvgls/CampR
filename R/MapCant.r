#' Mapa de Demersales Norte para una sola campaña
#'
#' Crea un mapa para el Cantábrico y Galicia con información para la especie solicitada en la campaña solicitada, sólo una campaña para mas campañas ver maphist
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 Moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie númerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param color Color de los puntos que representan las abundancias
#' @param add Si T añade los puntos al gráfico actual, si F dibuja uno nuevo
#' @param escala Varia el tamaño de los puntos
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param peso Si T representa los datos de biomasa, si F representa los datos de abundancia
#' @seealso maphist {\link{maphist}}
#' @export
MapCant<- function(gr,esp,camp,dns,color=1,add=F,escala=NA,ti=F,peso=T){
  if (length(camp)>1) {stop("Seleccionadas mas de una campaña, no se pueden sacar resultados de más de una")}
	require(RODBC)
  options(scipen=2)
	require(maps)
  esp<-format(esp,width=3,justify="r")
	if (!add) {
    MapNort(lwdl=1)
		}
	if (ti) {
    especie<-buscaesp(gr,esp)
		}
	else {especie=NULL}
	ch1<-odbcConnect(dsn=dns)
	odbcSetAutoCommit(ch1, FALSE)
  #browser()
	absp<-sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"' and esp='",esp,"'",sep=""))
	lan<-sqlQuery(ch1,paste("select lance,latitud_l,latitud_v,longitud_l,longitud_v,ewl,ewv from LANCE",camp," where validez<>'0'",sep=""))
	if (ti) {
		ident<-sqlQuery(ch1,paste("select ident from CAMP",camp,sep=""))[[1]]
		ident<-as.character(ident)
		}
	odbcClose(ch1)
#	names(lan)<-gsub("_",".",names(lan))
#	names(absp)<-gsub("_",".",names(absp))
	lan[,"latitud_l"]<-sapply(lan[,"latitud_l"],gradec)
	lan[,"latitud_v"]<-sapply(lan[,"latitud_v"],gradec)
	lan[,"longitud_l"]<-sapply(lan[,"longitud_l"],gradec)*ifelse(lan$ewl=="W",-1,1)
	lan[,"longitud_v"]<-sapply(lan[,"longitud_v"],gradec)*ifelse(lan$ewv=="W",-1,1)
	lan[,"lat"]<-(lan[,"latitud_l"]+lan[,"latitud_v"])/2
	lan[,"long"]<-(lan[,"longitud_l"]+lan[,"longitud_v"])/2
	lan<-lan[,c("lance","lat","long")]
	names(lan)<-c("lan","lat","long")
	mm<-merge(lan,absp,by.x="lan",by.y="lance",all.x=T)
	if (!identical(as.numeric(which(is.na(mm[,4]))),numeric(0))) {
		mm[which(is.na(mm[,4])),4]<-0
		mm[which(is.na(mm[,5])),5]<-0
		}
	mi<-ifelse(peso,4,5)
	milab<-ifelse(peso,"kg/lance","N/lance")
	if (peso) {maxmm<-max(mm[,mi])}
	else {
		maxmm<-max(mm[,mi])*1000
		mm[,mi]<-mm[,mi]*1000
		}
	if ((signif(maxmm,1)/(10^nchar(maxmm)))>.7) {maxml<-10^nchar(maxmm)}
	else {
		if ((signif(maxmm,1)/(10^nchar(maxmm)))>.28) {
			maxml<-5*10^(nchar(maxmm)-1)}
		else {maxml<-10^(nchar(maxmm)-1)}
		}
	if (is.na(escala)) {
		for (i in 1:length(mm[,1])) {
			points(mm[i,3],mm[i,2],cex=sqrt(mm[i,mi]*7/maxmm),lwd=2,col=color,pch=19)
			if (mm[i,mi]==0) {points(mm[i,3],mm[i,2],cex=0.6,pch="+",col=color)}
			}
		if (!add) {
			leyenda<-cbind(rep(-4,5),seq(42.2,43,by=.2),maxml*c(.05,.1,.25,.5,1))
			for (i in 1:5) {points(leyenda[i,1],leyenda[i,2],cex=sqrt(leyenda[i,3]*7/maxmm),lwd=2,col=1,bg="white")}
			polygon(c(-4.25,-4.25,-3.4,-3.4,-4.25),c(41.9,43.15,43.15,41.9,41.9),col="white")
			text(leyenda[,1]+.3,leyenda[,2]+.02,labels=round(leyenda[,3]/1000,2),cex=.9,adj=c(.5,.5))
			text(-3.8,42.05,milab)
			for (i in 1:5) {points(leyenda[i,1],leyenda[i,2],cex=sqrt(leyenda[i,3]*7/maxmm),lwd=2,col=1,bg="white")}
			}
		}
	else {
		for (i in 1:length(mm[,1])) {
			points(mm[i,3],mm[i,2],cex=sqrt(mm[i,mi]/escala),lwd=2,col=color,pch=19)
			if (mm[i,4]==0) {points(mm[i,3],mm[i,2],cex=0.6,pch="+",col=color)}
			if (!add) {
				leyenda<-cbind(rep(-4.6,5),seq(42.2,43,by=.2),maxml*c(.05,.1,.25,.5,1))
				polygon(c(-4.85,-4.85,-4,-4,-4.85),c(41.9,43.15,43.15,41.9,41.9),col="white")
				text(leyenda[,1]+.3,leyenda[,2]+.02,labels=round(leyenda[,3]/1000,2),cex=.9,adj=c(.5,.5))
				text(-4.43,42.05,milab)
				for (i in 1:5) {points(leyenda[i,1],leyenda[i,2],cex=sqrt(leyenda[i,3]/escala),lwd=2,bg="white")}
				}
			}
		}
	if (ti) {
		text(-2.7,42.8,ident,cex=1.5)
		text(-2.7,42.6,especie,font=4)
		#	text(-2.7,42.45,"kg/lance")
		}
	}