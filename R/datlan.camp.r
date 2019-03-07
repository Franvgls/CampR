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
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param incl0 Si T se incluyen los lances nulos
#' @param hidro Si T muestra datos de hidrografía. Necesita que exista fichero base de datos de hidro: HIDROXYY.dbf
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
datlan.camp<-function(camp,dns,incl2=TRUE,incl0=FALSE,hidro=FALSE,excl.sect=NA,redux=FALSE,year=T,quarter=T,bio=F) {
  foop<-function(camp,dns,incl2=incl2,incl0=incl0,hidro=hidro) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    ch1<-RODBC::odbcConnect(dsn=dns)
    RODBC::odbcSetAutoCommit(ch1, FALSE)
    lan<-RODBC::sqlQuery(ch1,paste("select lance,validez,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,velocidad,
                            sector,estrato,cable,malletas,dista_p,abert_h,abert_v,recorrido,fecha,ewl,ewv,cuadricula,hora_l,hora_v,rumbo                                   ,dir_viento,vel_viento,est_mar,temp,sali,estn,arte from LANCE",camp,sep=""))
    if (hidro) {
      dathidro<-RODBC::sqlFetch(ch1,paste("HIDRO",camp,sep=""))
      dathidro<-dathidro[,c(11:13,17:23)]
    }
    lan$latitud_l<-round(sapply(lan$latitud_l,gradec),4)
    lan$longitud_l<-round(sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="E",1,-1),4)
    lan$latitud_v<-round(sapply(lan$latitud_v,gradec),4)
    lan$longitud_v<-round(sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="E",1,-1),4)
    if (any(redux | bio)) {
      lan$lat<-round((lan$latitud_l+lan$latitud_v)/2,4)
      lan$long<-round((lan$longitud_l+lan$longitud_v)/2,4)
      lan$prof<-(lan$prof_l+lan$prof_v)/2
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
      }
      if (any(is.na(lan$zona))) {warning(paste0("Al menos un lance: ",lan$lance[is.na(lan$zona)],
                                                " sin Zona ICES asignada, revise resultados",lan$camp[is.na(lan$zona)]))}
    }
    lan$dista_p[lan$dista_p==0]<-NA
    lan$abert_v[lan$abert_v==0]<-NA
    lan$abert_h[lan$abert_h==0]<-NA
    lan$sali[lan$sali==0]<-NA
    lan$temp[lan$temp==0]<-NA
    lan$fecha<-format(lan$fecha,"%d-%m-%y")
    durlan<-RODBC::sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))$DURLAN
    lan$sector<-paste(lan$sector,lan$estrato,sep="")
    lan$weight.time<-ifelse(durlan==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6)))
    lan$weight.time<-round(lan$weight.time,3)
    lan$Haul.mins<-durlan
    lan$hora_l<-format(lan$hora_l,format="%H")
    lan$hora_v<-format(lan$hora_v,format="%H")
    if (!any(redux | bio)) lan<-lan[,c(1:33)]
    else lan<-lan[,c(1:2,32:34,9:31,35:36)]
    area<-NULL
    barco<-RODBC::sqlFetch(ch1,paste0("CAMP",camp),as.is=T)$BARCO
    dumb<-as.character(names(RODBC::sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))))
    for (i in 21:45) {
      area<-paste(area,dumb[i],sep=",")
    }
    area<-substr(area,2,nchar(area))
    area<-RODBC::sqlQuery(ch1,paste("select ",area," from CAMP",camp,sep=""))
    area<-area[-which(is.na(area) | area==0)]
    area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
    names(area)<-c("sector","arsect")
    RODBC::odbcClose(ch1)
    if (!incl0) {lan<-lan[c(lan$validez!=0),]}
    if (!incl2) {lan<-lan[c(as.numeric(lan$validez)<=1),]}
    datos<-merge(lan,area,by.x="sector",by.y="sector",all.x=TRUE)
    if (hidro) datos<-merge(datos,dathidro,by.x="lance",by.y="LANCE",all.x=TRUE)
    datos$arsect<-as.numeric(as.character(datos$arsect))
    datos$barco<-barco
    #browser()
    datos<-datos[,c(2,1,3:ncol(datos))]
    names(datos)<-tolower(names(datos))
    if(quarter==T) datos$quarter=as.character(cut(as.numeric(substr(datos$fecha,4,5)),c(0,3,6,9,12),labels=c(1:4)))
    if(year==T) datos$year=as.numeric(paste0(ifelse(as.numeric(substr(camp,2,3)>50),19,20),substr(camp,2,3)))
    datos[order(datos$lance),]
  }
  datos<-data.frame(foop(camp[1],dns=dns,incl2=incl2,incl0=incl0,hidro=hidro),camp=camp[1])
  if (length(camp)>1) {
    for (i in camp[2:length(camp)]) datos<-rbind(datos,data.frame(foop(i,dns=dns,incl2=incl2,incl0=incl0,hidro=hidro),camp=i))
  }
  if (any(is.na(datos$zona))) {warning(paste0("Al menos un lance: ",datos$lance[is.na(datos$zona)],
                                            " sin Zona ICES asignada, revise resultados"))}
  if (any(!is.na(excl.sect))) {
    datos$sector<-gsub("NA","N",datos$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(datos$sector))>0)) datos<-datos[-grep(excl.sect[i],as.character(datos$sector)),]}
    #		  datos$sector<-factor(as.character(datos$sector))
  }
  datos$sector<-factor(as.character(datos$sector))

#  datos
  if (bio) datos[,c("lance","sector","validez","lat","long","prof","estrato","fecha","zona","camp")] else datos
  }

