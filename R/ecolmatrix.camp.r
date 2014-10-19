#' Extrae los datos del FAUNA de una especie en concreto. 
#' 
#' Crea un data frame con campos lance, peso, numero, subestrato (en doble cifra), área subestrato  y posición geográfica. Usando códigos "9" "999" extrae los datos para el conjunto de todas las especies y grupos
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp ha de ser 999 cuando se quiere incluir todas las especies del grupo, o elegir todas las especies deseadas con los codigos de las especies
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param ind Elige el valor (n)úmero o (p)eso que se utiliza en el resultado
#' @param incl6 Si F se excluye el grupo 6 de otros (en general material no orgánico o antropogénico). Si T lo incluye dentro del grupo de "especies"
#' @return Devuelve un data.frame con columnas lan, una con el peso o número de cada especie del grupo solicitado, lat, long, prof
#' @seealso MapEcol.camp {\link{MapEcol.camp.r}} ecolgr.camp {\link{ecolgr.camp.r}}
#' @examples ecolmatrix.camp(1,999,"P01","Pnew",ind="p")
#' @export
ecolmatrix.camp<- function(gr,esp,camp,dns,ind,incl6=F) {
  require(RODBC)
  #	require(vegan)
  grupo<-as.character(gr)
  esp<-format(esp,width=3,justify="r")
  ch1<-odbcConnect(dsn=dns)
  fauna<-sqlFetch(ch1,paste("FAUNA",camp,sep=""))
  if (length(esp)==1) {
    if (grupo!="9" & esp!="999") {
      absp<-fauna[fauna$GRUPO==as.integer(grupo) & fauna$ESP==as.integer(esp),] 
      }
    if (grupo!="9" & esp=="999") {
      absp<-fauna[fauna$GRUPO==as.integer(grupo),] }
    if (grupo=="9" & esp=="999") {
      if (incl6) {absp<-fauna}
      else absp<-fauna[fauna$GRUPO!=6,]
      }
      }
  else {
    absp<-fauna[fauna$GRUPO==grupo & fauna$ESP %in% as.integer(esp),]
    }
  names(absp)<-tolower(names(absp))
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
  if (ind=="p") ecol<-tapply(absp$peso.gr,absp[,c(1,6)],sum)
  if (ind=="n") ecol<-tapply(absp$numero,absp[,c(1,6)],sum)
  ecol[is.na(ecol)]<-0
  m.names<-gsub(" ","",colnames(ecol))
  ecol<-data.frame(lan=as.numeric(rownames(ecol)),ecol)
  names(ecol)<-c("lan",m.names)
  ecol<-merge(ecol,lan,by="lan",all.x=T,all.y=T)
  ecol[is.na(ecol)]<-0
  ecol
}
