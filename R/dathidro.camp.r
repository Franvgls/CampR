#' Características del hidrografía de la campaña desde el fichero hidro
#'
#' Función de acceso a datos:
#' Extrae las características de las hidrología para una campaña determinada
#'
#' Un problema que ocurre al utilizar el CampR con ficheros dbf de las primeras campañas
#' puede ser que al fichero lanceXXX.dbf le falte algún campo, habitualmente
#' el campo **ESTN** utilizado en las últimas versiones del **CAMP** para relacionar los lances con las estaciones de CTD.
#' El error usual es **$ operator is invalid for atomic vectors**
#' Si se detecta este error revisar la estructura de lanceXXX.dbf con la de
#' otros ficheros de lances de los últimos años
#'
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param year si T incluye una columna con el año al final de los datos
#' @param quarter si T incluye una columna con el trimestre de los datos teniendo en cuenta la fecha del lance, puede cambiar a mitad de la campaña, cuidado con campañas IBTS adscritas a un trimestre particular.
#' @return Devuelve un data.frame con datos de cada estación hidrografica y la correspondencia a los lances del fichero lance
#' @seealso {\link{MapLansGPS}}
#' @examples
#'   print(dathidro.camp(Nsh[24],"Cant"))
#'   print(datlan.camp("P16","Porc",bio=T,outhidro=T))
#' @export
dathidro.camp<-function(camp,dns,year=TRUE,quarter=TRUE) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    ch1<-DBI::dbConnect(odbc::odbc(), dns)
    on.exit(DBI::dbDisconnect(ch1), add = TRUE)
    if (!DBI::dbExistsTable(ch1,paste0("HIDRO",camp))) {stop(paste0("No existe fichero de CTDs para ",camp));outhidro=FALSE}
    else
        {dathidro<-DBI::dbReadTable(ch1,paste0("HIDRO",camp))
        names(dathidro)<-tolower(names(dathidro))
        dathidro<-data.frame(camp=camp,dathidro,stringsAsFactors = FALSE)
        dathidro<-dplyr::rename(dathidro,prof.ctd=prof) #dathidro$prof.ctd<-dathidro$prof
        dathidro<-dplyr::rename(dathidro,cable.ctd=cable) #dathidro$prof.ctd<-dathidro$prof
        dathidro<-dplyr::rename(dathidro,hora.ctd=hora) #dathidro$prof.ctd<-dathidro$prof
        dathidro<-dplyr::rename(dathidro,fecha.ctd=fecha) #dathidro$prof.ctd<-dathidro$prof
        dathidro$fecha.ctd<-as.Date(ifelse(dathidro$fecha.ctd < "1980-12-31", format(dathidro$fecha.ctd, "20%y-%m-%d"), format(dathidro$fecha.ctd)))
        #as.Date(ifelse(lan$fecha < "1980-12-31", format(lan$fecha, "20%y-%m-%d"), format(lan$fecha)))
        dathidro$lat.ctd<-gradec(dathidro$latitud)*ifelse(dathidro$nosu=="N",1,-1)
        dathidro$long.ctd<-gradec(dathidro$longitud)*ifelse(dathidro$eswe=="W",-1,1)
        dathidro$lance<-as.numeric(dathidro$lance)
        dathidro$estn<-as.numeric(dathidro$estn)
        if(nrow(dathidro)==0) message("Fichero de CTDs sin datos")
        }
    dumb<-DBI::dbReadTable(ch1,paste0("CAMP",camp))
    #DBI::dbDisconnect(ch1)
    if (any(!dathidro$nosu %in% c("N","S"))) message(paste("En la estacion",dathidro[!dathidro$nosu %in% c("N","S"),"estn"],
                                                           "el campo nosu debe ser N o S y es",dathidro[!dathidro$nosu %in% c("N","S"),"nosu"]))
    if (any(!dathidro$eswe %in% c("W","E"))) message(paste("En la estación",dathidro[!dathidro$eswe %in% c("E","W"),"eswe"],
                                                            "el campo eswe debe ser E o W y es",dathidro[!dathidro$eswe %in% c("N","S"),"eswe"]))
    dathidro<-select(dathidro,-longitud,-latitud,-nosu,-eswe)
    dathidro<-select(dathidro,camp,estn,lance,hora.ctd,fecha.ctd,lat.ctd,long.ctd,sonda,cable.ctd,prof.ctd,
                     temp0,sali0,sigma0,temp50,sali50,sigma50,temp100,sali100,sigma100,
                     temp,sali,sigma,observ)
    # if(all(is.na(dathidro$sigma))) select(dathidro,-sigma)
    # if(all(is.na(dathidro$sigma0))) select(dathidro,-sigma0)
    # if(all(is.na(dathidro$sigma50))) select(dathidro,-sigma50)
    # if(all(is.na(dathidro$sigma100))) select(dathidro,-sigma100)
    for (i in c(1:nrow(dathidro))) {
      if (dathidro$lat.ctd[i]>48 & dathidro$lat.ctd[i]<52.5 & dathidro$long.ctd[i]>c(-18) & dathidro$long.ctd[i]<c(-12)) {dathidro$zona[i]<- "7k"}
      if (dathidro$lat.ctd[i]>52.5 & dathidro$lat.ctd[i]<54.5 & dathidro$long.ctd[i] > c(-18) & dathidro$long.ctd[i] < c(-12)) {dathidro$zona[i]<- "7c"}
      if (dathidro$lat.ctd[i]>52.5 & dathidro$lat.ctd[i]<54.5 & dathidro$long.ctd[i] > c(-12)) {dathidro$zona[i]<- "7b"}
      if (dathidro$lat.ctd[i]>43 & dathidro$lat.ctd[i]<44.5 & dathidro$long.ctd[i] > c(-2)) {dathidro$zona[i]<- "8b"}
      if (dathidro$lat.ctd[i]>44.5 & dathidro$lat.ctd[i]<46 & dathidro$long.ctd[i] > c(-4)) {dathidro$zona[i]<- "8b"}
      if (dathidro$lat.ctd[i]>43 & dathidro$lat.ctd[i]<44.5 & dathidro$long.ctd[i] > c(-11) & dathidro$long.ctd[i] < c(-2)) {dathidro$zona[i]<- "8c"}
      if (dathidro$lat.ctd[i]>35.95 & dathidro$lat.ctd[i]<43 & dathidro$long.ctd[i] > c(-11) & dathidro$long.ctd[i] < c(-8.75)) {dathidro$zona[i]<- "9a"}
      if (dathidro$lat.ctd[i]>35.95 & dathidro$lat.ctd[i]<37.75 & dathidro$long.ctd[i] > c(-7.5) & dathidro$long.ctd[i] < c(-5.50)) {dathidro$zona[i]<- "9a"}
      if (dns=="Medi" & dathidro$lat.ctd[i]>35.8 & dathidro$long.ctd[i]>c(-5.6556)) {dathidro$zona[i]<-"wm.37.1"}
    }
    if (any(is.na(dathidro$zona))) {message(paste0("Al menos una estación: ",paste(dathidro$estn[is.na(dathidro$zona)],collapse = ","),
                                                " sin Zona ICES asignada, revise resultados",dathidro$camp[is.na(dathidro$zona)]))}
    if (any(is.na(data.table::as.ITime(gsub("\\.",":",format(dathidro$hora,format="%H")))))) {message(paste0("Al menos una hora de largada (estn: ",
                      paste(dathidro[is.na(data.table::as.ITime(gsub("\\.",":",format(dathidro$hora,format="%H")))),c("estn")],collapse=","),") con hora inválida"))
    dathidro$sali[dathidro$sali==0]<-NA
    dathidro$temp[dathidro$temp==0]<-NA
    #as.Date(ifelse(lan$fecha < "1980-12-31", format(lan$fecha, "20%y-%m-%d"), format(lan$fecha)))
    dathidro$fecha.ctd<-as.Date(ifelse(dathidro$fecha.ctd < "1980-12-31", format(dathidro$fecha.ctd, "20%y-%m-%d"), format(dathidro$fecha)))
    dathidro$hora.ctd<-format(dathidro$hora,format="%H")
    #dathidro<-dathidro[,c(2,1,3:ncol(dathidro))]
    if(quarter==T) dathidro$quarter=substr(quarters(as.Date(dathidro$fecha.ctd)),2,2)
    if(year==T) dathidro$year=year(dathidro$fecha.ctd)
    }
  return(dathidro)
  }

