#' Mapa ampliado de Porcupine con las estaciones realizadas
#'
#' Crea un mapa de Porcupine con las estaciones realizadas(al igua que "armap.camp") pero con la referencia de Irlanda para su mejor ubicación.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant" (ARSA para sacar datos al IBTS, no gráficos)
#' @param lwdl Anchura de las líneas
#' @param col Color de los puntos que representan estaciones
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param argr Grosor de las flechas de recorrido
#' @param arrow Si T añade flechas de reccorrido
#' @param leg Si T añade leyenda
#' @param es Si T nombres en castellano, si F nombres en inglés
#' @param bw Si T mapa en blanco y negro, si F colorea los estratos y sectores
#' @param noval Si T añade las estaciones nulas
#' @param CTDs Si T añade las estaciones de CTD realizadas
#' @param NCTDs Si T identifica las estaciones de CTD numéricamente
#' @param Dates Si T saca las fechas en la que se han realizado los lances (dd-mm), no incluye información año.
#' @param lans Si T marca las estaciones de muestreo con un punto (si Nlans=FALSE)
#' @param strat strat Si T represent los estratos
#' @return Saca mapa con el desarrollo de la campaña con la zona y estratificación incluyendo tierra (Porcupine)
#' @examples armap.tot("P14",dns="Porc",noval=TRUE,CTDs=FALSE,bw=TRUE,strat=FALSE,leg=TRUE)
#' @examples armap.tot("N14",dns="Cant",noval=TRUE,CTDs=FALSE,bw=FALSE,leg=TRUE)
#' @family mapas
#' @family resumen general
#' @export
armap.tot<-function(camp,dns="Porc",ICESrect=FALSE,lwdl=1,col=2,argr=2,arrow=FALSE,leg=FALSE,
                    es=FALSE,bw=TRUE,noval=FALSE,Nlans=FALSE,CTDs=FALSE,NCTDs=FALSE,Dates=FALSE,lans=TRUE,strat=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
	lan<-datlan.camp(camp,dns,redux=T,incl2=TRUE,incl0=TRUE)
  if (CTDs |NCTDs) {
	ch1<-DBI::dbConnect(odbc::odbc(), dns)
	if (DBI::dbExistsTable(ch1,paste0("HIDRO",camp))) {
	  hidro<-DBI::dbReadTable(ch1,paste0("HIDRO",camp))
	  names(hidro)<-tolower(names(hidro))
	  hidro<-dplyr::select(hidro,estn,latitud,longitud,eswe)
	  if(nrow(hidro)==0) message("Fichero de CTDs sin datos")
	}
	else {
	  if (CTDs | NCTDs) message(paste0("Solicitados datos de CTDs, falta fichero HIDRO",camp,".dbf. No se muestran los CTDS"))
	  CTDs=F
	}
	DBI::dbDisconnect(ch1)
  }
	#camp.name<-DBI::dbReadTable(ch1, paste0("CAMP",camp[1]))$IDENT
	#camp.name<-stringr::word(camp.name)
	if (CTDs | NCTDs) {
    hidro$latitud<-gradec(hidro$latitud)
    hidro$longitud<-gradec(hidro$longitud)*ifelse(hidro$eswe=="W",-1,1)
    }
	lan<-lan[,c("lance","lat","long","validez")]
	names(lan)<-c("lan","lat","long","val")
	if (dns=="Pnew" | dns=="Porc") maparea(es=es,leg=FALSE,sectcol = F,bw=bw,ICESrect=ICESrect)
  else {
	  if (dns=="Cant" | dns=="Cnew") MapNort(strat=strat,bw=bw,es=es,ICESrect = ICESrect)
    else {
      if (dns=="Arsa") MapArsa(es=es,ICESrect = ICESrect)
      }
    }
	if (arrow) {
	arrows(lan[c(1:(length(lan[,1])-1)),c(3)],
		lan[c(1:(length(lan[,1])-1)),c(2)],
		lan[c(2:length(lan[,1])),c(3)],
		lan[c(2:length(lan[,1])),c(2)],0.05,col=col,lwd=argr)
		}
	if (lans & noval& !Dates) points(lan[lan$val==0,c(3,2)],pch=13,cex=1.2)
	if (lans & !Nlans & !NCTDs & !Dates) {
	  points(lan[lan$val==1,c(3,2)],pch=21,cex=1.1,bg="gray40")
	  points(lan[lan$val>1,c(3,2)],pch=21,col=ifelse(bw,1,3),cex=1.1,bg=ifelse(bw,"white","green"))
	}
	else {
	  if (Nlans) text(lan[lan$val==1,c(3)],lan[lan$val==1,c(2)],lan[lan$val==1,1],cex=ifelse(NCTDs & Nlans,.6,.8),font=2)
	  if (Nlans & length(lan[lan$val>1,1])>0) text(lan[lan$val>1,c(3)],lan[lan$val>1,c(2)],lan[lan$val>1,1],cex=ifelse(NCTDs & Nlans,.6,.8),font=2)
	  if (NCTDs) text(hidro$longitud,hidro$latitud,hidro$estn,cex=ifelse(NCTDs & Nlans,.6,.8),font=2,col=2)
	  if (Dates)   text(lan$long,lan$lat,substr(lan$fecha,1,5),cex=.7,font=1)
	}
	if (CTDs & !Nlans& !NCTDs) points(hidro[,c(3,2)],pch=25,cex=.8,bg=ifelse(bw,"white","lightblue"))
	if (leg) {
		l1<-c(ifelse(es,"Lances válidos","Valid tows"),ifelse(es,"Lances extra","Extra tows"))
		pts<-c(16,21)
		cexs<-c(1.3,1.3)
		bgs<-c(1,ifelse(bw,"white","green"))
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
		legend("bottomright",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.05,bg="white",cex=.9)
		}
	}
#armap.tot("P08","Porc",noval=FALSE,CTDs=TRUE,arrow=FALSE,leg=TRUE)
#armap.tot("P06",cols=FALSE,arrow=FALSE,leg=FALSE,es=TRUE,CTDs=FALSE)
#title("Porcupine 2006",line=2.5)
