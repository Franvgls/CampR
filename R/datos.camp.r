#' Datos de abundancia y biomasa de una especie 
#'
#' Función de acceso a datos: 
#' Extrae los datos de abundancia y biomasa de una especie o conjunto de especies a partir de las faunísticas de una campaña. Crea una tabla con información del sector(número) y estrato (letra), lance, peso, número y arsect
#' Funciones para obtener data.frame de especies concretas a partir de los ficheros del Camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T datos en kgs, si F en gramos 
#' @return Devuelve un data.frame con los datos de la especie por lance: sector,lance,peso,numero,arsect (área del sector al que pertence el lance)
#' @export
datos.camp<-function(gr,esp,camp,dns,cor.time=T,kg=T) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  require(RODBC)
  esp<-format(esp,width=3,justify="r")
  ch1<-odbcConnect(dsn=dns)
  odbcSetAutoCommit(ch1, FALSE)
  fauna<-sqlFetch(ch1,paste("FAUNA",camp,sep=""))
  if (length(esp)==1) {
    if (gr!="9" & esp!="999") {
      absp<-fauna[fauna$GRUPO==as.integer(gr) & fauna$ESP==as.integer(esp),c(1,4:5)] }
    if (gr!="9" & esp=="999") {
      absp<-fauna[fauna$GRUPO==as.integer(gr),c(1,4:5)] }
    if (gr=="9" & esp=="999") {
      absp<-fauna[,c(1,4:5)] }
  }
  else {
    absp<-fauna[fauna$GRUPO==gr & fauna$ESP %in% as.integer(esp),c(1,4:5)]
  }
  durlan<-sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))$DURLAN
  lan<-sqlQuery(ch1,paste("select lance,sector,estrato,hora_l,hora_v from LANCE",camp," where validez='1'",sep=""))
  lan<-lan[!is.na(lan$estrato),]
  lan<-data.frame(lance=lan$lance,sector=paste(lan$sector,lan$estrato,sep=""),hora_v=as.numeric(lan$hora_v),
                  hora_l=as.numeric(lan$hora_l))
  lan<-data.frame(lance=lan$lance,sector=paste(lan$sector,lan$estrato,sep=""),
                  weight.time=ifelse(durlan==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6))))
  names(absp)<-gsub("_",".",tolower(names(absp)))
  if (any((gr=="9" | esp=="999" | length(esp)>1))) {
    absp<-data.frame(lance=names(tapply(absp$peso.gr,absp$lance,sum)),peso.gr=tapply(absp$peso.gr,absp$lance,sum),
                     numero=tapply(absp$numero,absp$lance,sum)) }
  absp$lance<-as.numeric(as.character(absp$lance))
  area<-NULL
  dumb<-as.character(names(sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))))
  for (i in 21:45) {
    area<-paste(area,dumb[i],sep=",")
  }
  area<-substr(area,2,nchar(area))
  area<-sqlQuery(ch1,paste("select ",area," from CAMP",camp,sep=""))
  #browser()
  if ((sum(is.na(area))/length(area))==1) {
    stop(paste("El fichero",paste("camp",camp,".dbf,",sep=""),"está vacío, define la estratificación de la campaña",camp))
  }
  #browser()
  if (dns=="Cant" & (sum(is.na(area))/length(area))>.4) {
    warning("Muchos NAs en fichero camp",camp,", está definida la campaña ",camp,"? Revisar antes de aceptar el resultado")
  }
  area<-area[-which(is.na(area) | area==0)]
  area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
  names(area)<-c("sector","arsect")
  odbcClose(ch1)
  names(lan)<-c("lance","sector","weight.time")
  names(absp)<-c("lance","peso","numero")
  especial<-sum(absp$peso)
  if (kg) { absp$peso<-absp$peso/1000 }
  mm<-merge(lan,absp,by.x="lance",by.y="lance",all.x=T)
  mm$numero[which(is.na(mm$numero))]<-0
  mm$peso[which(is.na(mm$peso))]<-0
  if (any(cor.time,camp=="N83",camp=="N84")) {
    mm$peso<-mm$peso/mm$weight.time
    mm$numero<-mm$numero/mm$weight.time
  }
#  mm<-mm[,-3]
  datos<-merge(mm,area,by.x="sector",by.y="sector")
  datos$arsect<-as.numeric(as.character(datos$arsect))
  #browser()
  if (especial>0 & sum(mm$numero)==0) {message(paste("campaña ",camp,","," capturas en lances especiales pero no en los lances válidos normales",sep="")) }
  if (length(esp)>1) {print(c("Códigos de especie: ",esp))}
  #print(lan[order(lan$lance),])
  datos[order(datos$lance),]
}
# datos.camp(1,50,"P10","Pnew",kg=F, cor.time=T)