#' Mapa de Demersales Norte para una sola campaña
#'
#' Crea un mapa para el Cantábrico y Galicia con información para la especie solicitada en la campaña solicitada, sólo una campaña para mas campañas ver maphist
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros, 9 escoge todos los orgánicos pero excluye desechos
#' @param esp Código de la especie númerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: solo para el Cantábrico "Cant"
#' @param color Color de los puntos que representan las abundancias
#' @param add Si T añade los puntos al gráfico actual, si F dibuja uno nuevo
#' @param escala Varia el tamaño de los puntos
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param ind Si "p" representa los datos de biomasa, si "n" representa los datos de abundancia
#' @param ceros Si T representa los ceros con cruces +, si F no representa lances sin captura
#' @seealso {\link{maphist}}
#' @family mapas base
#' @family Norte Demersales
#' @export
MapCant<- function(gr,esp,camp,dns="Cant",color=1,add=FALSE,escala=NA,ti=FALSE,ind="p",ceros=F){
  if (length(camp)>1) {stop("Seleccionadas mas de una campaña, no se pueden sacar resultados de más de una")}
  options(scipen=2)
  esp<-format(esp,width=3,justify="r")
	if (!add) {
    MapNort(lwdl=1)
		}
	if (ti) {
    especie<-buscaesp(gr,esp)
		}
	else {especie=NULL}
	ch1<-RODBC::odbcConnect(dsn=dns)
	RODBC::odbcSetAutoCommit(ch1, FALSE)
  #browser()
	absp<-RODBC::sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"' and esp='",esp,"'",sep=""))
	if (ti) {
		ident<-RODBC::sqlQuery(ch1,paste("select ident from CAMP",camp,sep=""))[[1]]
		ident<-as.character(ident)
		}
	RODBC::odbcClose(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,incl2=TRUE,incl0=FALSE)
	lan<-lan[,c("lance","lat","long")]
	names(lan)<-c("lan","lat","long")
	mm<-merge(lan,absp,by.x="lan",by.y="lance",all.x=TRUE)
	if (!identical(as.numeric(which(is.na(mm[,4]))),numeric(0))) {
		mm[which(is.na(mm[,4])),4]<-0
		mm[which(is.na(mm[,5])),5]<-0
		}
  if (ind=="p") peso=T else peso=F
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
			if (mm[i,mi]==0 & ceros) {points(mm[i,3],mm[i,2],cex=0.6,pch="+",col=color)}
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