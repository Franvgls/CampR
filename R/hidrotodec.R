#'Transforma fichero hidroXXX.dbf a datos decimales
#'
#'Añade variables lat y long a los datos en el fichero hidroXXX.dbf (no toca el fichero dbf)
#'@param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#'@param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las RODBCs
#'@param year si T incluye una columna con el año al final de los datos
#'@return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux. En cualquier caso incluye variables weight.time con el factor de calibración para lances con menos tiempo del estándar y arsect: el área del sector al que corresponde el lance dentro del muestreo
#'@family datos_especies
#'@examples camptoyear(Nsh)
#'@export
hidrotodec<- function(camp,dns,year=T) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  if (!DBI::dbExistsTable(ch1,paste0("HIDRO",camp))) {stop(paste0("No existe fichero de CTDs para ",camp));hidro=FALSE}
  else
  {dathidro<-DBI::dbReadTable(ch1,paste0("HIDRO",camp))
  DBI::dbDisconnect(ch1)
  if(nrow(dathidro)==0) message("Fichero de CTDs sin datos")
  }
  if (any(!dathidro$NOSU %in% c("N","S"))) message(paste("En la estación",dathidro[!dathidro$NOSU %in% c("N","S"),"ESTN"],
                                                         "el campo NOSU que debe ser N o S es",dathidro[!dathidro$NOSU %in% c("N","S"),"NOSU"]))
  if (any(!dathidro$ESWE %in% c("W","E"))) message(paste("En la estación",dathidro[!dathidro$NOSU %in% c("E","W"),"ESWE"],
                                                         "el campo ESWE que debe ser E o W es",dathidro[!dathidro$NOSU %in% c("N","S"),"ESWE"]))
  dathidro$long<-gradec(dathidro$LONGITUD)*ifelse(dathidro$ESWE=="W",-1,1)
  dathidro$lat<-gradec(dathidro$LATITUD)*ifelse(dathidro$NOSU=="N",1,-1)
  dathidro$camp<-camp
  dathidro<-dathidro[,c(1,25,5,26,3,6:23,27)]
  names(dathidro)<-tolower(names(dathidro))
  if (year) dathidro$year<-camptoyear(camp)
  dathidro
  }




