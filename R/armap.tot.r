#' Mapa ampliado de Porcupine con las estaciones realizadas
#' 
#' Crea un mapa de Porcupine con las estaciones realizadas(al igua que "armap.camp") pero con la referencia de Irlanda para su mejor ubicación.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant" (ARSA para sacar datos al IBTS, no gráficos)
#' @param lwdl Anchura de las líneas
#' @param col Color de los puntos que representan estaciones
#' @param argr Grosor de las flechas de recorrido
#' @param arrow Si T añade flechas de reccorrido 
#' @param leg Si T añade leyenda
#' @param es Si T nombres en castellano, si F nombres en inglés
#' @param bw Si T mapa en blanco y negro, si F colorea los estratos y sectores
#' @param noval Si T añade las estaciones nulas
#' @param CTDs Si T añade las estaciones de CTD realizadas
#' @param strat strat Si T represent los estratos 
#' @export
armap.tot<-function(camp,dns="Pnew",lwdl=1,col=2,argr=2,arrow=F,leg=F,es=F,bw=T,noval=F,
	CTDs=F,strat=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
	require(RODBC)
	ch1<-odbcConnect(dsn=dns)
	odbcSetAutoCommit(ch1, FALSE)
	lan<-sqlQuery(ch1,paste("select lance,latitud_l,latitud_v,longitud_l,longitud_v,validez,ewl,ewv from LANCE",camp,sep=""))
	if (any(sqlTables(ch1)$TABLE_NAME==paste("HIDRO",camp,sep="")))
    {hidro<-sqlQuery(ch1,paste("select latitud,longitud,eswe from HIDRO",camp,sep=""))}
  else CTDs=F
	odbcClose(ch1)
	names(lan)<-gsub("_",".",names(lan))
  lan$latitud.l<-sapply(lan$latitud.l,gradec)
  lan$longitud.l<-sapply(lan$longitud.l,gradec)*ifelse(lan$ewl=="W",-1,1)
  lan$latitud.v<-sapply(lan$latitud.v,gradec)
  lan$longitud.v<-sapply(lan$longitud.v,gradec)*ifelse(lan$ewv=="W",-1,1)
 	if (CTDs) {
    hidro$latitud<-gradec(hidro$latitud)
    hidro$longitud<-gradec(hidro$longitud)*ifelse(hidro$eswe=="W",-1,1)
    }
	lan[,"lat"]<-(lan[,"latitud.l"]+lan[,"latitud.v"])/2
	lan[,"long"]<-(lan[,"longitud.l"]+lan[,"longitud.v"])/2
	lan<-lan[,c("lance","lat","long","validez")]
	names(lan)<-c("lan","lat","long","val")
	if (dns=="Pnew" | dns=="Porc") maparea(es=es,leg=F,bw=bw)
  else {
	  if (dns=="Cant" | dns=="Cnew") MapNort(strat=strat,bw=bw,es=es)
    else {
      if (dns=="Arsa") MapArsa(es=es)
      }
    }
	if (arrow) {
	arrows(lan[c(1:(length(lan[,1])-1)),c(3)],
		lan[c(1:(length(lan[,1])-1)),c(2)],
		lan[c(2:length(lan[,1])),c(3)],
		lan[c(2:length(lan[,1])),c(2)],0.05,col=col,lwd=argr)
		}
	if (noval) points(lan[lan$val==0,c(3,2)],pch=13,cex=1.5)
	if (CTDs) points(hidro[,c(2,1)],pch=25,cex=1,bg="lightblue")
	points(lan[lan$val==1,c(3,2)],pch=16)
	points(lan[lan$val==2,c(3,2)],pch=16,col=3)
	print(str(hidro))
	if (leg) {
		l1<-c(ifelse(es,"Lances válidos","Valid tows"),ifelse(es,"Lances extra","Extra tows"))
		pts<-c(16,21)
		cexs<-c(1.3,1.3)
		bgs<-c(1,"green")
		if (noval) {
			l1<-c(l1,ifelse(es,"Lances nulos","Null tows"))
			pts<-c(pts,13)
			bgs<-c(bgs,1)
			cexs<-c(cexs,1.3)
			}
		if (CTDs) {
			l1<-c(l1,"CTD")
			pts<-c(pts,25)
			bgs<-c(bgs,"lightblue")
			cexs<-c(cexs,1.3)
			}
		legend("bottom",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.05,bg="white",cex=.9)
		}
	}
#armap.tot("P08","Pnew",noval=F,CTDs=T,arrow=F,cols=F,leg=T)
#armap.tot("P06",cols=F,arrow=F,leg=F,es=T,CTDs=F)
#title("Porcupine 2006",line=2.5)
