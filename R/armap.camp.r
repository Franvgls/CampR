#' Mapa con las estaciones realizadas
#'
#' Crea un mapa de un único año con las estaciones realizadas
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX", Arsa otoño "2XX" y Medits "MXX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa", Mediterráneo "Medi"
#' @param lwdl Anchura de las líneas
#' @param col Color de los puntos que representan estaciones
#' @param argr Grosor de las flechas de recorrido
#' @param cuadr Si T dibuja cuadrícula
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD)
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param arrow Si T añade flechas de recorrido
#' @param leg Si T añade leyenda
#' @param es Si T nombres en castellano, si F nombres en inglés
#' @param cols Si T colorea los estratos y sectores, si F mapa en blanco y negro
#' @param noval Si T añade las estaciones nulas
#' @param CTDs Si T añade las estaciones de CTD realizadas
#' @param strat Si T delimita los sectores y los identifica
#' @param Nlans Si T identifica las estaciones numéricamente
#' @param NCTDs Si T identifica las estaciones de CTD numéricamente
#' @param Dates Si T saca las fechas en la que se han realizado los lances (dd-mm), no incluye información año.
#' @param places Si T añade los nombres de las principales ciudades
#' @param bw Gráfico en blanco y negro si T o en color si F
#' @param ti si TRUE incluye un título con el nombre de la campaña
#' @param lans Si T marca las estaciones de muestreo con un punto (si Nlans=FALSE)
#' @param xlims Delimita la longitud del mapa mediante un vector (ejem.c(-10.25,-1.4))
#' @param ylims Delimita la latitud del mapa mediante un vector (ejem.c(41.82,44.48))
#' @return Saca mapa con el desarrollo de la campaña con la zona y estratificación
#' @examples
#' op<-par(no.readonly = TRUE)
#' par(mfrow=c(1,1))
#' armap.camp("N14",dns="Cant",noval=TRUE,CTDs=FALSE,bw=FALSE)
#' armap.camp("C14","Cant",xlims=c(-10.2,-7.6),es=TRUE)
#' par(mfrow=c(1,2),oma=c(0,0,2.5,0),mar=c(3,3,3,3))
#' armap.camp("P14","Porc",xlims=c(-10.2,-7.6),es=FALSE)
#' title("Trawl hauls",cex=1,line=2.5)
#' armap.camp("P14","Porc",xlims=c(-10.2,-7.6),lans=TRUE,es=FALSE,CTDs=TRUE)
#' title("CTD casts",cex=1,line=2.5)
#' mtext("Porcupine 2014 survey",line=1,outer=TRUE,cex=1.4,font=2)
#' par(op)
#' @family mapas
#' @family resumen general
#' @export
armap.camp<-function(camp,dns="Porc",ti=FALSE,lwdl=1,col=2,argr=2,cuadr=FALSE,cuadrMSFD=FALSE,ICESrect=FALSE,ICESlab=FALSE,arrow=FALSE,leg=TRUE,es=FALSE,cols=TRUE,noval=TRUE,
	CTDs=FALSE,strat=FALSE,Nlans=FALSE,NCTDs=FALSE,Dates=F,places=FALSE,bw=FALSE,lans=TRUE,xlims=c(-10.25,-1.4),ylims=c(41.82,44.48)) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
	lan<-datlan.camp(camp,dns,redux=T,incl2=TRUE,incl0=TRUE,bio=F,outhidro=F)
	ch1<-DBI::dbConnect(odbc::odbc(), dns)
	if (DBI::dbExistsTable(ch1,paste0("HIDRO",camp))) {
	  hidro<-DBI::dbReadTable(ch1,paste0("HIDRO",camp))
    hidro<-dplyr::rename_with(hidro,tolower)
	  hidro<-dplyr::select(hidro,estn,latitud,longitud,eswe)
	  if(nrow(hidro)==0) message("Fichero de CTDs sin datos")
	  }
  else {
    if (CTDs | NCTDs) message(paste0("Solicitados datos de CTDs, falta fichero HIDRO",camp,".dbf. No se muestran los CTDS"))
    CTDs=F
    }
	camp.name<-DBI::dbReadTable(ch1, paste0("CAMP",camp[1]))$IDENT
	#camp.name<-stringr::word(camp.name)
	DBI::dbDisconnect(ch1)
 	if (CTDs | NCTDs) {
    hidro$latitud<-gradec(hidro$latitud)
    hidro$longitud<-gradec(hidro$longitud)*ifelse(hidro$eswe=="W",-1,1)
    }
	lan<-lan[,c("lance","lat","long","validez","fecha")]
	names(lan)<-c("lan","lat","long","val","fecha")
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    mapporco(lwdl=lwdl,cuadr=cuadr,ICESrect = ICESrect,ICESlab = ICESlab,bw=bw)
    if (ti) {title(camp.name,line=2)}
	}
  if (dns=="Arsa") {
    MapArsa(xlims=c(-8.14,-5.54),ylims=c(35.95,37.33),lwdl=lwdl,cuadr=cuadr,ICESrect = ICESrect,ICESlab = ICESlab,bw=bw)
    if (ti) {title(camp.name,line=2)}
  }
  if (dns=="Medi") {
    MapMedit(lwdl=lwdl,cuadr=cuadr,bw=bw,es=es,places=places)
    if (ti) {title(camp.name,line=2)}
  }
  if (substr(dns,1,4)=="Cant"| dns=="Cnew"){
    MapNort(strat=strat,bw=bw,es=es,places=places,cuadr=cuadr,cuadrMSFD=cuadrMSFD,ICESrect = ICESrect,ICESlab = ICESlab,xlims=xlims,ylims=ylims) #,places=places
    if (ti) {title(camp.name,line=2)}
  }
	if (arrow & !Nlans) {
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
  #print(str(hidro))
	if (leg & !Nlans & !Dates) {
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
    if (dns=="Medi" & dns!= "Arsa") legend("topleft",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.08,bg="white",cex=.9)
		if (dns=="Arsa" & dns!= "Medi") legend("topright",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.05,bg="white",cex=.9)
		if (dns!="Medi" & dns!= "Arsa") legend("bottomright",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.1,bg="white",cex=.9)
		}
	}

#armap.camp("N08","Cant",Nlans=TRUE,NCTDs=TRUE)
#armap.camp("N08","Cant",noval=FALSE,CTDs=TRUE,places=TRUE,cuadrMSFD=TRUE)
#armap.camp("P06",cols=FALSE,arrow=FALSE,leg=TRUE,CTDs=TRUE,es=TRUE,noval=TRUE)
#armap.camp("106","Arsa",cols=FALSE,arrow=FALSE,leg=TRUE,CTDs=FALSE,es=TRUE,noval=TRUE)
