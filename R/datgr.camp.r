#' Datos de biomasa y abundancia para una especie y campaña
#'
#' Función de acceso a datos:
#' Extrae los datos de peso y número de una especie o conjunto de especies a partir de las faunísticas de una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @return Devuelve un data.frame con información del lance, latitud, longitud, profundidad, peso (gramos) y numero 
#' @examples datgr.camp(1,50,"P10","Pnew",cor.time=T)
#' @export
datgr.camp<- function(gr,esp,camp,dns,cor.time=T) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  require(RODBC)
  esp<-format(esp,width=3,justify="r")
  ch1<-odbcConnect(dsn=dns)
  odbcSetAutoCommit(ch1, FALSE)
  fauna<-sqlFetch(ch1,paste("FAUNA",camp,sep=""))
  fauna$ESP<-as.numeric(as.character(fauna$ESP))
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
  #	if (gr!="9" & esp!="999") {
  #		absp<-sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",
  #		camp," where grupo='",gr,"' and esp='",esp,"'",sep="")) }
  #	if (gr!="9" & esp=="999") {
  #		absp<-sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep="")) }
  #	if (gr=="9" & esp=="999") {
  #		absp<-sqlQuery(ch1,paste("select lance,peso_gr,numero from FAUNA",camp,sep="")) }
  lan<-sqlQuery(ch1,paste("select lance,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,ewl,ewv,hora_l,hora_v from LANCE",
                          camp," where validez<>'0'",sep=""),errors=F)
  durlan<-sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))$DURLAN
  odbcClose(ch1)
  if (length(lan)==1) {
    mm<-data.frame(lan=0,lat=0,long=0,prof=0,numero=0,peso.gr=0)
  }
  else {
    names(absp)<-gsub("_",".",tolower(names(absp)))
    lan$latitud_l<-sapply(lan$latitud_l,gradec)
    lan$longitud_l<-sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="W",-1,1)
    lan$latitud_v<-sapply(lan$latitud_v,gradec)
    lan$longitud_v<-sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="W",-1,1)
    lan[,"lat"]<-(lan[,"latitud_l"]+lan[,"latitud_v"])/2
    lan[,"long"]<-(lan[,"longitud_l"]+lan[,"longitud_v"])/2
    lan[,"prof"]<-(lan[,"prof_l"]+lan[,"prof_v"])/2
    lan$weight.time<-ifelse(durlan==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6)))
    lan<-lan[,c("lance","lat","long","prof","weight.time")]
    names(lan)<-c("lan","lat","long","prof","weight.time")
    if (any((gr=="9" | esp=="999" | length(esp)>1))) {
      absp<-data.frame(lance=names(tapply(absp$peso.gr,absp$lance,sum)),peso.gr=tapply(absp$peso.gr,absp$lance,sum),
                       numero=tapply(absp$numero,absp$lance,sum)) }
    if (nrow(absp)==0) {absp<-data.frame(lance=lan$lan,peso.gr=0,numero=0)}
    mm<-try(merge(lan,absp,by.x="lan",by.y="lance",all.x=T),silent=T)
    #		browser()
    if (!identical(as.numeric(which(is.na(mm[,7]))),numeric(0))) {
      mm[which(is.na(mm[,6])),6:7]<-0
    }
  if (any(cor.time,camp=="N83",camp=="N84")) {
    mm$peso.gr<-mm$peso.gr/mm$weight.time
    mm$numero<-round(mm$numero/mm$weight.time,0)
    }
  mm<-mm[,c("lan","lat","long","prof","peso.gr","numero")]
  }
  mm
}
