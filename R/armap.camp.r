#' Mapa con las estaciones realizadas
#'
#' Crea un mapa de un único año con las estaciones realizadas
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param lwdl Anchura de las líneas
#' @param col Color de los puntos que representan estaciones
#' @param argr Grosor de las flechas de recorrido
#' @param cuadr Si T dibuja cuadrícula
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD) 
#' @param arrow Si T añade flechas de reccorrido 
#' @param leg Si T añade leyenda
#' @param es Si T nombres en castellano, si F nombres en inglés
#' @param cols Si T colorea los estratos y sectores, si F mapa en blanco y negro
#' @param noval Si T añade las estaciones nulas
#' @param CTDs Si T añade las estaciones de CTD realizadas
#' @param strat Si T delimita los sectores y los identifica
#' @param Nlans Si T identifica las estaciones numéricamente
#' @param NCTDs Si T identifica las estaciones de CTD numéricamente
#' @param places Si T añade los nombres de las principales ciudades
#' @param bw Gráfico en blanco y negro si T o en color si F
#' @param lans Si T marca las estaciones de muestreo con un punto (si Nlans=F) 
#' @param xlims Delimita la longitud del mapa mediante un vector (ejem.c(-10.25,-1.4))
#' @param ylims Delimita la latitud del mapa mediante un vector (ejem.c(41.82,44.48)) 
#' @export
armap.camp<-function(camp,dns="Pnew",lwdl=1,col=2,argr=2,cuadr=F,cuadrMSFD=F,arrow=F,leg=T,es=F,cols=T,noval=T,
	CTDs=F,strat=F,Nlans=F,NCTDs=F,places=F,bw=F,lans=T,xlims=c(-10.25,-1.4),ylims=c(41.82,44.48)) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
	require(RODBC)
	ch1<-odbcConnect(dsn=dns)
	odbcSetAutoCommit(ch1, FALSE)
	lan<-sqlQuery(ch1,paste("select lance,latitud_l,latitud_v,longitud_l,longitud_v,validez,ewl,ewv from LANCE",camp,sep=""))
	if (any(sqlTables(ch1)$TABLE_NAME==paste("HIDRO",camp,sep="")))
    {hidro<-sqlQuery(ch1,paste("select estn,latitud,longitud,eswe from HIDRO",camp,sep=""))}
  else {
    if (CTDs | NCTDs) warning(paste("Solicitados datos de CTDs, falta fichero HIDRO",camp,".dbf. No se muestran los CTDS",sep=""))
    CTDs=F
    }
	odbcClose(ch1)
	names(lan)<-gsub("_",".",names(lan))
  lan$latitud.l<-sapply(lan$latitud.l,gradec)
  lan$longitud.l<-sapply(lan$longitud.l,gradec)*ifelse(lan$ewl=="W",-1,1)
  lan$latitud.v<-sapply(lan$latitud.v,gradec)
  lan$longitud.v<-sapply(lan$longitud.v,gradec)*ifelse(lan$ewv=="W",-1,1)
 	if (CTDs | NCTDs) {
    hidro$latitud<-gradec(hidro$latitud)
    hidro$longitud<-gradec(hidro$longitud)*ifelse(hidro$eswe=="W",-1,1)
    }
	lan[,"lat"]<-(lan[,"latitud.l"]+lan[,"latitud.v"])/2
	lan[,"long"]<-(lan[,"longitud.l"]+lan[,"longitud.v"])/2
	lan<-lan[,c("lance","lat","long","validez")]
	names(lan)<-c("lan","lat","long","val")
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") mapporco(lwdl=lwdl,cuadr=cuadr)
	if (dns=="Arsa") MapArsa(lwdl=lwdl,cuadr=cuadr)
	if (substr(dns,1,4)=="Cant"| dns=="Cnew") MapNort(strat=strat,bw=bw,es=es,places=places,cuadr=cuadr,cuadrMSFD=cuadrMSFD,xlims=xlims,ylims=ylims) #,places=places
	if (arrow & !Nlans) {
	arrows(lan[c(1:(length(lan[,1])-1)),c(3)],
		lan[c(1:(length(lan[,1])-1)),c(2)],
		lan[c(2:length(lan[,1])),c(3)],
		lan[c(2:length(lan[,1])),c(2)],0.05,col=col,lwd=argr)
		}
	if (lans & noval) points(lan[lan$val==0,c(3,2)],pch=13,cex=1.2)
  if (CTDs & !Nlans& !NCTDs) points(hidro[,c(3,2)],pch=25,cex=.8,bg=ifelse(bw,"white","lightblue"))
  if (lans & !Nlans & !NCTDs) {
		points(lan[lan$val==1,c(3,2)],pch=21,cex=1.1,bg="gray40")
		points(lan[lan$val==2,c(3,2)],pch=21,col=ifelse(bw,1,3),cex=1.1,bg=ifelse(bw,"white","green"))
		}
	else {
		if (Nlans) text(lan[lan$val==1,c(3)],lan[lan$val==1,c(2)],lan[lan$val==1,1],cex=ifelse(NCTDs & Nlans,.6,.8),font=2)
		if (Nlans) text(lan[lan$val==2,c(3)],lan[lan$val==2,c(2)],lan[lan$val==2,1],cex=ifelse(NCTDs & Nlans,.6,.8),font=2)
		if (NCTDs) text(hidro$longitud,hidro$latitud,hidro$estn,cex=ifelse(NCTDs & Nlans,.6,.8),font=2,col=2)
	   }
	#print(str(hidro))
	if (leg & !Nlans) {
		if (lans) {
      l1<-c(ifelse(es,"Lances válidos","Valid tows"),ifelse(es,"Lances extra","Extra tows"))
		  pts<-c(21,21)
		  cexs<-c(1.2,1.2)
		  bgs<-ifelse(bw,c("gray40"),c("gray40"))
		  bgs<-c(bgs,ifelse(bw,c("white"),c("green")))
		  }
    else {
      l1<-NULL
      pts<-NULL
      cexs<-NULL
      bgs<-NULL
      bgs<-NULL
    }
    if (lans & noval) {
			l1<-c(l1,ifelse(es,"Lances nulos","Null tows"))
			pts<-c(pts,13)
			bgs<-c(bgs,1)
			cexs<-c(cexs,1.2)
			}
		if (CTDs) {
			l1<-c(l1,"CTDs")
			pts<-c(pts,25)
			bgs<-c(bgs,ifelse(bw,"white","lightblue"))
			cexs<-c(cexs,1)
			}
		legend("bottomright",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.05,bg="white",cex=.9)
		}
	}

#armap.camp("N08","Cant",Nlans=T,NCTDs=T)
#armap.camp("N08","Cant",noval=F,CTDs=T,places=T,cuadrMSFD=T)
#armap.camp("P06",cols=F,arrow=F,leg=T,CTDs=T,es=T,noval=T)
#armap.camp("106","Arsa",cols=F,arrow=F,leg=T,CTDs=F,es=T,noval=T)
