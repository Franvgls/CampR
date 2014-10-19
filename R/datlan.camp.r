#' Características del lance
#'
#' Función de acceso a datos:
#' Extrae las características de los lances para una campaña determinada
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param incl0 Si T se incluyen los lances nulos
#' @param hidro Si T muestra datos de hidrografía. Necesita que exista fichero base de datos de hidro: HIDROXYY.dbf
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param redux Si T elimina datos de longitud y latitud de virada y muestra la media de las profundidades de largada y virada
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux. En cualquier caso incluye variables weight.time con el factor de calibración para lances con menos tiempo del estándar y arsect: el área del sector al que corresponde el lance dentro del muestreo
#' @seealso {\link{MapLansGPS}}
#' @examples # datlan.camp(Nsh[24:28],"Cant",hidro=F,excl.sect=c("A"))
#' @export
datlan.camp<-function(camp,dns,incl2=T,incl0=F,hidro=F,excl.sect=NA,redux=F) {
  foop<-function(camp,dns,incl2=T,incl0=F,hidro=F) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    require(RODBC)
    ch1<-odbcConnect(dsn=dns)
    odbcSetAutoCommit(ch1, FALSE)
    lan<-sqlQuery(ch1,paste("select lance,validez,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,velocidad,
                            sector,estrato,cable,malletas,dista_p,abert_h,abert_v,recorrido,fecha,ewl,ewv,cuadricula,hora_l,hora_v from LANCE",camp,sep=""))
    if (hidro) {
      dathidro<-sqlFetch(ch1,paste("HIDRO",camp,sep=""))
      dathidro<-dathidro[,c(11:13,17:23)]
    }
    lan$latitud_l<-sapply(lan$latitud_l,gradec)
    #browser()
    lan$longitud_l<-sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="E",1,-1)
    lan$latitud_v<-sapply(lan$latitud_v,gradec)
    lan$longitud_v<-sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="E",1,-1)
    if (redux) {
      lan$lat<-(lan$latitud_l+lan$latitud_v)/2
      lan$long<-(lan$longitud_l+lan$longitud_v)/2
      lan$prof<-(lan$prof_l+lan$prof_v)/2
    }
    lan$dista_p[lan$dista_p==0]<-NA
    lan$abert_v[lan$abert_v==0]<-NA
    lan$abert_h[lan$abert_h==0]<-NA
    #lan[,3]<-(lan[,3]+lan[,4])/2
    #lan[,5]<--(lan[,5]+lan[,6])/2
    #browser()
    #lan[,7]<-(lan[,7]+lan[,8])/2
    lan$fecha<-format(lan$fecha,"%d-%m-%y")
    # lan<-lan[!is.na(lan$estrato),]
    durlan<-sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))$DURLAN
    lan$sector<-paste(lan$sector,lan$estrato,sep="")
    #browser()
    lan$weight.time<-ifelse(durlan==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6)))
    lan$Haul.mins<-durlan
    lan$hora_l<-format(lan$hora_l,format="%H")
    lan$hora_v<-format(lan$hora_v,format="%H")
    if (!redux) lan<-lan[,c(1:23,24:25)]
    else lan<-lan[,c(1:2,24:26,9:22,27:28)]
    area<-NULL
    dumb<-as.character(names(sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))))
    for (i in 21:45) {
      area<-paste(area,dumb[i],sep=",")
    }
    area<-substr(area,2,nchar(area))
    area<-sqlQuery(ch1,paste("select ",area," from CAMP",camp,sep=""))
    area<-area[-which(is.na(area) | area==0)]
    area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
    names(area)<-c("sector","arsect")
    odbcClose(ch1)
    if (!incl0) {lan<-lan[c(lan$validez!="0"),]}                                      
    if (!incl2) {lan<-lan[c(lan$validez!="2"),]}                                      
    datos<-merge(lan,area,by.x="sector",by.y="sector",all.x=T)
    if (hidro) datos<-merge(datos,dathidro,by.x="lance",by.y="LANCE",all.x=T)
    datos$arsect<-as.numeric(as.character(datos$arsect))
    #browser()
    datos<-datos[,c(2,1,3:ncol(datos))]
    names(datos)<-tolower(names(datos))
    datos[order(datos$lance),]
  }
  datos<-data.frame(foop(camp[1],dns=dns,incl2=incl2,incl0=incl0,hidro=hidro),camp=camp[1])
  if (length(camp)>1) {
    for (i in camp[2:length(camp)]) datos<-rbind(datos,data.frame(foop(i,dns=dns,incl2=incl2,incl0=incl0,hidro=hidro),camp=i))
  }
  if (any(!is.na(excl.sect))) {
    datos$sector<-gsub("NA","N",datos$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(datos$sector))>0)) datos<-datos[-grep(excl.sect[i],as.character(datos$sector)),]}
    #		  datos$sector<-factor(as.character(datos$sector))
  }
  datos$sector<-factor(as.character(datos$sector))
  datos
  }
