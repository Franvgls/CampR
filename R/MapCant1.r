#' Mapa de Demersales Norte para una o varias especies y tambie´n combina campañas superponiéndolas
#' para dar una idea de la distribución de la especie. Si es muy escasa se pasa de la representación
#' en bolas a puntos de aparición
#'
#' Crea un mapa para el Cantábrico y Galicia con información para la especie solicitada en la campaña solicitada, sólo una campaña para mas campañas ver maphist
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros, 9 escoge todos los orgánicos pero excluye desechos
#' @param esp Código de la especie númerico o caracter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campañas a representar en el mapa, con MapCant1 se puede sacar más de  un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: solo para el Cantábrico "Cant"
#' @param color Color de los puntos que representan las abundancias
#' @param bw si T los colores salen en blanco, si F en lightblue
#' @param add Si T añade los puntos al gráfico actual, si F dibuja uno nuevo
#' @param escala Varia el tamaño de los puntos
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param ind Si "p" representa los datos de biomasa, si "n" representa los datos de abundancia
#' @param ceros Si T representa los ceros con cruces +, si F no representa lances sin captura
#' @seealso {\link{maphist}}
#' @family mapas base
#' @family Norte Demersales
#' @export
MapCant1<- function(gr,esp,camps,dns="Cant",color=1,bw=F,add=FALSE,escala=NA,ti=FALSE,ind="p",ceros=F,es=T,places=T){
  options(scipen=2)
  esp<-format(esp,width=3,justify="r")
	ch1<-RODBC::odbcConnect(dsn=dns)
	RODBC::odbcSetAutoCommit(ch1, FALSE)
  if (nrow(RODBC::sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camps[1]," where grupo='",gr,"' and esp='",esp,"'",sep="")))==0) {stop(paste("No existe",buscaesp(gr,esp),"en la campaña",camps[1],"revise especie y/o campaña"))}
	absp<-cbind(camp=camps[1],RODBC::sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camps[1]," where grupo='",gr,"' and esp='",esp,"'",sep="")))
	if (length(camps)>1) for (i in 2:length(camps)) {absp<-rbind(absp,cbind(camp=camps[i],RODBC::sqlQuery(ch1,
	                                                                                                   paste("select lance,peso_gr,numero from FAUNA",camps[i],
	                                                                                                             " where grupo='",gr,"' and esp='",esp,"'",sep=""))))}
	if (ti) {
		ident<-RODBC::sqlQuery(ch1,paste("select ident from CAMP",camps[1],sep=""),as.is=TRUE)[[1]]
		ident<-stringr::word(ident)
		}
	RODBC::odbcClose(ch1)
  lan<-datlan.camp(camps,dns,redux=TRUE,incl2=TRUE,incl0=FALSE)
	lan<-lan[,c("camp","lance","lat","long")]
	names(lan)<-c("camp","lan","lat","long")
	mm<-merge(lan,absp,by.x=c("camp","lan"),by.y=c("camp","lance"),all.x=TRUE)
	if (!identical(as.numeric(which(is.na(mm[,5]))),numeric(0))) {
		mm[which(is.na(mm[,5])),5]<-0
		mm[which(is.na(mm[,6])),6]<-0
		}
  if (ind=="p") peso=T else peso=F
	mi<-ifelse(peso,5,6)
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
	if (!add) {
	  MapNort(lwdl=1,es=es,places=places)
	}
	if (ti) {
	  especie<-buscaesp(gr,esp)
	}
	else {especie=NULL}
	if (is.na(escala)) {
			points(lat~long,mm,subset=peso>0,cex=sqrt(mm[,mi]*7/maxmm),lwd=1,col=color,pch=21,bg=ifelse(bw,"white","lightblue"))
			if (ceros) {points(lat~long,mm,subset=peso==0,cex=0.8,pch="+",col=color)}
		if (!add) {
		  leyenda<-cbind(rep(-4,55),seq(42.2,43,by=.2),maxml*c(.05,.1,.25,.5,1))
		  points(leyenda[,1],leyenda[,2],cex=sqrt(leyenda[,3]*7/maxmm),lwd=1,col=1,bg=ifelse(bw,"white","lightblue"))
		  polygon(c(-4.3,-4.3,-3.3,-3.3,-4.3),c(41.9,43.15,43.15,41.9,41.9),col="white")
		  text(leyenda[,1]+.4,leyenda[,2]+.02,labels=round(leyenda[,3]/1000,2),cex=.9,adj=c(.5,.5))
		  text(-3.8,42.05,milab,font=2)
		  for (i in 1:5) {points(leyenda[i,1],leyenda[i,2],cex=sqrt(leyenda[i,3]*7/maxmm),lwd=1,col=1,pch=21,bg=ifelse(bw,"white","lightblue"))}
		  }
		}
	else {
		for (i in 1:length(mm[,1])) {
			points(lat~long,mm,subset=peso>0,cex=sqrt(mm[,mi]/escala),lwd=1,col=color,pch=21,bg=ifelse(bw,"white","lightblue"))
			if (ceros) {points(lat~long,mm,subset=peso>0,cex=0.8,pch="+",col=color)}
			if (!add) {
				leyenda<-cbind(rep(-4.6,5),seq(42.2,43,by=.2),maxml*c(.05,.1,.25,.5,1))
				polygon(c(-4.85,-4.85,-4,-4,-4.85),c(41.9,43.15,43.15,41.9,41.9),col="white")
				text(leyenda[,1]+.3,leyenda[,2]+.02,labels=round(leyenda[,3]/1000,2),cex=.9,adj=c(.5,.5))
				text(-4.43,42.05,milab,font=2)
				for (i in 1:5) {points(leyenda[i,1],leyenda[i,2],cex=sqrt(leyenda[i,3]/escala),lwd=1,pch=21,bg=ifelse(bw,"white","lightblue"))}
				}
			}
		}
	if (ti) {
		years<-camptoyear(camps)
	  title(ident,sub=especie,cex.main=1.3,font.sub=4,cex.sub=1,line=2)
		title(paste0(years[1],"-",years[length(years)]),font.main=2,cex.main=1,line=1.1)
		#	text(-2.7,42.45,"kg/lance")
		}
	}
