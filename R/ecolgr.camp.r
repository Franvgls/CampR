#' Índices ecológicos para un grupo en una campaña
#' 
#' Utiliza los datos del Camp para calcular la riqueza, diversidad y dominancia
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp ha de ser 999 cuando se quiere incluir todas las especies del grupo, o elegir todas las especies deseadas con los codigos de las especies
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param ind Elige el valor sobre el que se calculan los índices de diversidad, dominancia....
#' @return Devuelve un data.frame con campos lan,lat,long,prof,div(Diversidad de Shanon),numbesp (riqueza: número de especies),simp (diversidad de simpson),domsimp (indice de dominancia de simpson). 
#' @seealso MapEcol.camp {\link{MapEcol.camp.r}}
#' @examples ecolgr.camp(1,999,"P08","Pnew",ind="n")
#' @export
ecolgr.camp<- function(gr,esp=999,camp,dns="Pnew",ind="n") {
  if (length(camp)>1) {
    stop("Seleccionadas más de una campaña, sólo se sacan resultados de campañas de una en una")
  }
  require(RODBC)
  require(vegan)
  grupo<-as.character(gr)
  esp<-format(esp,width=3,justify="r")
  ch1<-odbcConnect(dsn=dns)
  if (gr!="9" & esp!="999") {
    absp<-sqlQuery(ch1,paste("select lance,grupo,esp,peso_gr,numero from FAUNA",
                             camp," where grupo='",gr,"' and esp='",esp,"'",sep="")) }
  if (gr!="9" & esp=="999") {
    absp<-sqlQuery(ch1,paste("select lance,grupo,esp,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep="")) }
  if (gr=="9" & esp=="999") {
    absp<-sqlQuery(ch1,paste("select lance,grupo,esp,peso_gr,numero from FAUNA",camp,sep=""))
    absp<-absp[absp$grupo!=6,]
  }
  lan<-sqlQuery(ch1,paste("select lance,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v from LANCE",camp," where validez<>'0'",sep=""))
  odbcCloseAll()
  ch1<-odbcConnect(dsn="camp")
  especies<-sqlQuery(ch1,"select grupo,esp,especie from Especies")
  odbcCloseAll()
  especies$especie<-as.character(especies$especie)
  especies$ke<-paste(especies$grupo,format(especies$esp,width=3,justify="r"),sep=".")
  absp$ke<-paste(absp$grupo,format(absp$esp,width=3,justify="r"),sep=".")
  absp$especie<-especies$especie[match(absp$ke,especies$ke)]
  names(lan)<-gsub("_",".",names(lan))
  names(absp)<-gsub("_",".",names(absp))
  lan$latitud.l<-sapply(lan$latitud.l,function(x){trunc(x)+(x-trunc(x))*100/60 })
  lan$longitud.l<-sapply(lan$longitud.l,function(x){trunc(x)+(x-trunc(x))*100/60 })
  lan$latitud.v<-sapply(lan$latitud.v,function(x){trunc(x)+(x-trunc(x))*100/60 })
  lan$longitud.v<-sapply(lan$longitud.v,function(x){trunc(x)+(x-trunc(x))*100/60 })
  lan[,8]<-(lan[,2]+lan[,3])/2
  lan[,9]<--(lan[,4]+lan[,5])/2
  lan[,10]<-(lan[,6]+lan[,7])/2
  lan<-lan[,c(1,8,9,10)]
  names(lan)<-c("lan","lat","long","prof")
  absp<-absp[absp$lance %in% lan$lan,]
  if (ind=="p") ecol<-tapply(absp$peso.gr,absp[,c(1,7)],sum)
  if (ind=="n") ecol<-tapply(absp$numero,absp[,c(1,7)],sum)
  ecol[is.na(ecol)]<-0
  # browser()
  numbesp<-specnumber(ecol)
  div<-diversity(ecol)
  simp<-diversity(ecol,"simpson")
  invsimp<-diversity(ecol,"invsimpson")
  ecolinds<-data.frame(lance=names(div),div=div,numbesp=numbesp,simp=simp,domsimp=invsimp)
  mm<-merge(lan,ecolinds,by.x="lan",by.y="lance",all.x=T,all.y=T)
  if (!identical(as.numeric(which(is.na(mm[,4]))),numeric(0))) {
    mm[which(is.na(mm[,4])),4]<-0
    mm[which(is.na(mm[,5])),5]<-0
  }
  mm
}
