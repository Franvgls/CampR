#' Características del lance
#'
#' Función de acceso a datos:
#' Extrae las características de los lances para una campaña determinada
#'
#' Un problema que ocurre al utilizar el CampR con ficheros dbf de las primeras campañas
#' puede ser que al fichero lanceXXX.dbf le falte algún campo, habitualmente
#' el campo **ESTN** utilizado en las últimas versiones del **CAMP** para ligar lances con las estaciones de CTD.
#' El error usual es **$ operator is invalid for atomic vectors**
#' Si se detecta este error revisar la estructura de lanceXXX.dbf con la de
#' otros ficheros de lances de los últimos años
#'
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param incl0 Si T se incluyen los lances nulos
#' @param outhidro si T saca los datos del fichero hidro al final de todo el proceso como salida
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param redux Si T elimina datos de longitud y latitud de virada y muestra la media de las profundidades de largada y virada
#' @param year si T incluye una columna con el año al final de los datos
#' @param quarter si T incluye una columna con el trimestre de los datos teniendo en cuenta la fecha del lance, puede cambiar a mitad de la campaña, cuidado con campañas IBTS adscritas a un trimestre particular.
#' @param bio reduce el data.frame a los datos para los proyectos de biología, con datos en formato decimal y hexadecimal y las zonas ICES
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux. En cualquier caso incluye variables weight.time con el factor de calibración para lances con menos tiempo del estándar y arsect: el área del sector al que corresponde el lance dentro del muestreo
#' @seealso {\link{MapLansGPS}}
#' @examples
#'   print(datlan.camp(Nsh[24:28],"Cant",hidro=FALSE,excl.sect=c("A")))
#'   print(datlan.camp("P16","Porc",bio=T))
#' @export
datlan.camp<-function(camp,dns,incl2=TRUE,incl0=FALSE,excl.sect=NA,redux=FALSE,year=TRUE,quarter=TRUE,bio=FALSE) {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  on.exit(DBI::dbDisconnect(ch1), add = TRUE)
  lan<-DBI::dbGetQuery(ch1,paste0("select lance,validez,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,velocidad,
                            sector,estrato,cable,malletas,dista_p,abert_h,abert_v,recorrido,fecha,ewl,ewv,nsl,nsv,cuadricula,hora_l,hora_v,rumbo,dir_viento,
                                    vel_viento,est_mar,temp,sali,estn,arte from LANCE",camp[1]))
  lan$camp<-camp[1]
  dumb<-DBI::dbReadTable(ch1,paste0("CAMP",camp[1]))
  lan$haul.mins<-dumb$DURLAN
  lan$barco<-dumb$BARCO
  lan$sector<-paste0(lan$sector,lan$estrato)
  if(quarter==T) lan$quarter=substr(quarters(as.Date(lan$fecha)),2,2)
  if(year==T) lan$year=lubridate::year(lan$fecha)
  foop<-function(camp,dns,incl2=incl2,incl0=incl0) {
      # ch1<-DBI::dbConnect(odbc::odbc(), dns)
      # on.exit(DBI::dbDisconnect(ch1), add = TRUE)
      if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    lan<-DBI::dbGetQuery(ch1,paste0("select lance,validez,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,velocidad,
                            sector,estrato,cable,malletas,dista_p,abert_h,abert_v,recorrido,fecha,ewl,ewv,nsl,nsv,cuadricula,hora_l,hora_v,rumbo,dir_viento,
                                    vel_viento,est_mar,temp,sali,estn,arte from LANCE",camp))
    lan$lance<-as.integer(lan$lance)
    lan$validez<-as.integer(lan$validez)
    lan$sector<-as.integer(lan$sector)
    lan$estn<-as.integer(lan$estn)
    lan$arte<-as.integer(lan$arte)
    lan$camp<-camp
    # if (outhidro) {
    #   if (!DBI::dbExistsTable(ch1,paste0("HIDRO",camp))) {message(paste0("No existe fichero de CTDs para ",camp));outhidro=FALSE}
    #   else
    #     {dathidro<-dathidro.camp(camp,dns)
    #      if(nrow(dathidro)==0) message("Fichero de CTDs sin datos")
    #     }}
    dumb<-DBI::dbReadTable(ch1,paste0("CAMP",camp))
    lan$haul.mins<-dumb$DURLAN
    lan$barco<-dumb$BARCO
    lan$sector<-paste0(lan$sector,lan$estrato)
    area<-as.data.frame(cbind(sector=
                                as.character(substr(names(dumb[,21:45]),2,3)),arsect=as.numeric(t(dumb[,21:45]))))
    area<-area[!is.na(area$arsect),]
    # if(outhidro) {datos<-dplyr::full_join(lan,dathidro[,c("camp","lance","hora.ctd","fecha.ctd","lat.ctd","long.ctd","sonda","cable.ctd",
    #                          "prof.ctd","temp0","sali0","sigma0","temp50","sali50","sigma50","temp100","sali100","sigma100")])}
    # if (outhidro) datos<-dplyr::full_join(lan,dathidro,by=c("camp","lance","temp","sali","estn","zona"))
    #datos$arsect<-as.numeric(as.character(datos$arsect))
    #browser()
    #datos<-datos[,c(2,1,3:ncol(datos))]
    #names(datos)<-tolower(names(datos))
    if (any(!lan$nsl %in% c("N","S"))) message(paste("En el lance",lan[!lan$nsl %in% c("N","S"),"lance"],
                                                           "el campo nsl que debe ser N o S y es",lan[!lan$nsl %in% c("N","S"),"nsl"]))
    if (any(!lan$ewl %in% c("W","E"))) message(paste("En la estación",paste(lan[!lan$ewl %in% c("E","W"),"ewl"],collapse = ","),
                                                           "el campo ewl que debe ser E o W y es",lan[!lan$ewl %in% c("N","S"),"ewl"]))
    if (any(!lan$nsv %in% c("N","S"))) message(paste("En el lance",paste(lan[!lan$nsv %in% c("N","S"),"lance"],collapse=","),
                                                     "el campo nsv que debe ser N o S y es",lan[!lan$nsv %in% c("N","S"),"nsv"]))
    if (any(!lan$ewv %in% c("W","E"))) message(paste("En la estación",paste(lan[!lan$ewv %in% c("E","W"),"ewv"],collapse=","),
                                                     "el campo ewv que debe ser E o W y es",lan[!lan$ewv %in% c("N","S"),"ewv"]))
    lan$latitud_l<-round(sapply(lan$latitud_l,gradec)*ifelse(lan$nsl=="N",1,-1),4)
    lan$longitud_l<-round(sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="E",1,-1),4)
    lan$latitud_v<-round(sapply(lan$latitud_v,gradec)*ifelse(lan$nsv=="N",1,-1),4)
    lan$longitud_v<-round(sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="E",1,-1),4)
    lan$lat<-round((lan$latitud_l+lan$latitud_v)/2,4)
    lan$long<-round((lan$longitud_l+lan$longitud_v)/2,4)
    lan$prof<-(lan$prof_l+lan$prof_v)/2
    #lan<-lan[,-c(22:19)]
    lan$zona<-NA
    for (i in c(1:nrow(lan))) {
      if (lan$lat[i]>48 & lan$lat[i]<52.5 & lan$long[i]>c(-18) & lan$long[i]<c(-12)) {lan$zona[i]<- "7k"}
      if (lan$lat[i]>52.5 & lan$lat[i]<54.5 & lan$long[i] > c(-18) & lan$long[i] < c(-12)) {lan$zona[i]<- "7c"}
      if (lan$lat[i]>52.5 & lan$lat[i]<54.5 & lan$long[i] > c(-12)) {lan$zona[i]<- "7b"}
      if (lan$lat[i]>43 & lan$lat[i]<44.5 & lan$long[i] > c(-2)) {lan$zona[i]<- "8b"}
      if (lan$lat[i]>44.5 & lan$lat[i]<46 & lan$long[i] > c(-4)) {lan$zona[i]<- "8b"}
      if (lan$lat[i]>43 & lan$lat[i]<44.5 & lan$long[i] > c(-11) & lan$long[i] < c(-2)) {lan$zona[i]<- "8c"}
      if (lan$lat[i]>35.95 & lan$lat[i]<43 & lan$long[i] > c(-11) & lan$long[i] < c(-8.75)) {lan$zona[i]<- "9a"}
      if (lan$lat[i]>35.95 & lan$lat[i]<37.75 & lan$long[i] > c(-7.5) & lan$long[i] < c(-5.50)) {lan$zona[i]<- "9a"}
      if (dns=="Medi" & lan$lat[i]>35.8 & lan$long[i]>c(-5.6556)) {lan$zona[i]<-"wm.37.1"}
    }
    if (any(is.na(lan$zona))) {message(paste0("Al menos un lance: ",paste(lan$lance[is.na(lan$zona)],collapse = ","),
                                                " sin Zona ICES asignada, revise resultados",lan$camp[is.na(lan$zona)]))}
    if (any(format(lan$hora_l,format="%H")>format(lan$hora_v,format="%H"))) {message(paste0("Al menos un lance ",
                      paste(lan[format(lan$hora_l,format="%H")>format(lan$hora_v,format="%H"),c("lance")],collapse = ","),
                                            " con hora de virada antes de hora de largada"))}
    if (any(is.na(data.table::as.ITime(gsub("\\.",":",format(lan$hora_l,format="%H")))))) {message(paste0("Al menos una hora de largada (lance: ",
                      paste(lan[is.na(data.table::as.ITime(gsub("\\.",":",format(lan$hora_l,format="%H")))),c("lance")],collapse=","),") con hora inválida"))}
    if (any(is.na(data.table::as.ITime(gsub("\\.",":",format(lan$hora_v,format="%H")))))) {message(paste0("Al menos una hora de virada (lance: ",
                      paste(lan[is.na(data.table::as.ITime(gsub("\\.",":",lan$hora_v))),c("lance")],collapse = ","),") con hora inválida"))}
    #lan<-lan[,c(1:18,23:ncol(lan))]
    lan$dista_p[lan$dista_p==0]<-NA
    lan$abert_v[lan$abert_v==0]<-NA
    lan$abert_h[lan$abert_h==0]<-NA
    lan$sali[lan$sali==0]<-NA
    lan$temp[lan$temp==0]<-NA
    lan$fecha<-as.Date(ifelse(lan$fecha < "1980-12-31", format(lan$fecha, "20%y-%m-%d"), format(lan$fecha)))
      #format(lan$fecha,"%d-%m-%y")
    #durlan<-dumb$DURLAN
    lan$weight.time<-ifelse(lan$haul.mins==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6)))
    lan$weight.time<-round(lan$weight.time,3)
    lan$hora_l<-format(lan$hora_l,format="%H")
    lan$hora_v<-format(lan$hora_v,format="%H")
    if(quarter==T) lan$quarter=substr(quarters(as.Date(lan$fecha)),2,2)
    if(year==T) lan$year=lubridate::year(lan$fecha)
    #if (!any(redux | bio)) lan<-lan[,c(1:29,33:35)]
    #else lan<-lan[,c(1:2,30:32,9:29,33:35)]
    #print(names(lan))
    #barco<-dumb$BARCO
    if (!incl0) {lan<-lan[c(lan$validez!=0),]}
    if (!incl2) {lan<-lan[c(as.numeric(lan$validez)<=1),]}
    datos<-dplyr::left_join(lan,area,by="sector")
    datos$arsect<-as.numeric(as.character(datos$arsect))
    #if(quarter==T) datos$quarter=substr(quarters(as.Date(datos$fecha)),2,2)
    #datos<-dplyr::select(datos,-camp)
    datos[order(datos$lance),]
    }
  datos<-data.frame(camp=camp[1],foop(camp[1],dns=dns,incl2=incl2,incl0=incl0)) #,outhidro=outhidro
  if (length(camp)>1) {
    for (i in camp[2:length(camp)]) datos<-dplyr::bind_rows(datos,data.frame(foop(i,dns=dns,incl2=incl2,incl0=incl0),camp=i)) #,outhidro=outhidro
  }
#  if (length(datos$camp)==0) {datos$camp<-camp}
  if (any(is.na(datos$zona))) {message(paste0("Al menos un lance: ",datos$lance[is.na(datos$zona)],
                                            " sin Zona ICES asignada, revise resultados"))}
  if (any(!is.na(excl.sect))) {
    datos$sector<-gsub("NA","N",datos$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(datos$sector))>0)) datos<-datos[-grep(excl.sect[i],as.character(datos$sector)),]}
    #		  datos$sector<-factor(as.character(datos$sector))
  }
  datos$sector<-as.character(datos$sector)
  dplyr::arrange(datos,year,lance)
  if (redux) {
    datos<-dplyr::select(datos,-c("longitud_v","longitud_l","latitud_v","latitud_l","prof_v","prof_l"))
    datos<-dplyr::relocate(datos,c("camp","lance","validez","lat","long","prof"))
    }
  if (!redux & !bio) {
    #datos<-dplyr::select(datos,-c("long","lat","prof"))
    #datos$camp<-camp
    datos<-dplyr::relocate(datos,c("camp","lance","validez"))
    }
  #if (outhidro & redux) datos<-dplyr::select(datos,-c("longitud_v","longitud_l","latitud_v","latitud_l","prof_v","prof_l"))
  if (bio) datos<-datos[,c("camp","lance","sector","validez","lat","long","prof","estrato","fecha","zona")]
  if (!is.null(datos$camp.1)) {datos<-dplyr::select(datos,-camp.1)}
  return(datos)
  }

