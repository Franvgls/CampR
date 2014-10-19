#' Calcula las abundancias estratificadas por edad 
#'  
#' Función de resultados: abundancias estratificadas por edad para cada estrato batimétrico a partir de los datos del camp.
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param AltAlk ALK alternativa tomada de un fichero de edad del Camp edadXYY.dbf
#' @seealso GetAlk.camp {\link{GetAlk.camp}} 
#' @examples edadstr.camp("1"," 45","P01","Pnew",8)
#' @export
edadstr.camp<-function(gr,esp,camp,dns="Pnew",plus=8,cor.time=T,AltAlk=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("Sólo se puede incluir una especie en esta función")}
  esp<-format(esp,width=3,justify="r")
  require(RODBC)
  ch1<-odbcConnect(dns)
  ntalls<-sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                             " where grupo='",gr,"' and esp='",esp,"'",sep=""))
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls$lance<-as.numeric(as.character(ntalls$lance))
  ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  durlan<-sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))$DURLAN
  lan<-sqlQuery(ch1,paste("select lance,sector,estrato,hora_l,hora_v from LANCE",camp," where validez='1'",sep=""))
  lan<-lan[!is.na(lan$estrato),]
  lan<-data.frame(lance=lan$lance,sector=paste(lan$sector,lan$estrato,sep=""),hora_v=as.numeric(lan$hora_v),
                  hora_l=as.numeric(lan$hora_l))
  lan<-data.frame(lance=lan$lance,sector=paste(lan$sector,lan$estrato,sep=""),
                  weight.time=ifelse(durlan==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6))))
  ntalls<-ntalls[ntalls$lance %in% as.character(lan$lance),]
  if (any(cor.time,camp=="N83",camp=="N84")) {
    ntalls<-merge(ntalls,lan,by.x="lance",by.y="lance")
    ntalls$numer<-ntalls$numer/ntalls$weight.time
    ntalls<-ntalls[,1:6]
  }
  edad<-GetAlk.camp(gr,esp,camp,dns,plus,AltAlk)
  # identifica si la ALK est? hecha por sexos o conjunta
  agebysex<-ifelse(any(edad$sexo!=3),T,F)
  if (agebysex) {
    if (all(ntalls$sexo==3)) {
      print("ALK por sexos datos tallas no, simplifique la ALK",quote=F)
      agebysex<-F
      b<-1
      break
    }
    edadsx<-split(edad,factor(edad$sexo))
    ntallssx<-split(ntalls,factor(ntalls$sexo))
    talsdumb<-vector("list",length(ntallssx))
    edaddumb<-vector("list",length(edadsx))
    for (i in 1:length(ntallssx)) talsdumb[[i]]<-levels(factor(ntallssx[[i]][,4]))
    for (i in 1:length(edadsx)) edaddumb[[i]]<-levels(factor(edadsx[[i]][,1]))
    b<-vector("list",length(edadsx))
    for (i in 1:length(edadsx)) b[[i]]<-which(match(talsdumb[[i]],edaddumb[[i]],nomatch=0)==0,T)
    bb <- vector("list", length(edadsx))
    for (i in 1:length(edadsx)) bb[[i]]<-which(match(edaddumb[[i]],talsdumb[[i]],nomatch=0)==0,T)
  }
  else {
    a<-as.numeric(names(tapply(ntalls$numer,ntalls$talla,sum)))
    b<-which((match(a,edad$talla,nomatch=0)==0),T)
    bb <- vector("list",3)
  }
  if (any(sapply(b,length)>0)) { # | any(sapply(bb,length)>0)
    if (agebysex) {
      print("Tallas que no aparecen en ALK:",quote=F)
      print(paste("sex",names(edadsx),b))
      print("Tallas en ALK que no aparecen en distribuci?n:",quote=F)
      print(paste("sex",names(edadsx),bb))
    }
    else {
      print("Las tallas: ",quote=F)
      print(a[b])
      print("no estan en la clave talla edad",quote=F)
    }
  }
  else {
    sonedad<-which(substr(names(edad),1,1)=="E",T)
    for (i in sonedad) {edad[,i]<-edad[,i]/rowSums(edad[,sonedad])}
    ch1<-odbcConnect(dns)
    lan<-sqlQuery(ch1,paste("select lance,sector,estrato from LANCE",camp," where validez='1'",sep=""))
    lan<-lan[!is.na(lan$estrato),]
    lan<-as.data.frame(cbind(as.numeric(as.character(lan$lance)),paste(lan$sector,lan$estrato,sep="")))
    area<-NULL
    dumb<-as.character(names(sqlQuery(ch1,paste("select * from CAMP",camp,sep=""))))
    for (i in 21:45) {
      area<-paste(area,dumb[i],sep=",")
    }
    area<-substr(area,2,nchar(area))
    area<-sqlQuery(ch1,paste("select ",area," from CAMP",camp,sep=""))
    odbcCloseAll()
    area<-area[-which(is.na(area) | area==0)]
    area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
    names(area)<-c("sector","arsect")
    names(lan)<-c("lance","sector")
    lan<-merge(lan,area,by.x="sector",by.y="sector")
    dumbtal<-data.frame(talla=c(0:(trunc(max(ntalls[,4])/10)*10+10)))
    ntalls<-merge(dumbtal,ntalls,by.x="talla",by.y="talla",all.x=T)
    edad<-merge(dumbtal,edad,by.x="talla",by.y="talla",all.x=T)
    for (i in 1:ncol(ntalls)) {
      if (!identical(as.numeric(which(is.na(ntalls[,i]))),numeric(0))) {ntalls[which(is.na(ntalls[,i])),i]<-0}
    }
    for (i in 1:ncol(edad)) {
      if (!identical(as.numeric(which(is.na(edad[,i]))),numeric(0))) {edad[which(is.na(edad[,i])),i]<-0}
    }
    if (agebysex) {
      ntalls<-ntalls[ntalls$sexo>0,]
      sexos<-names(edadsx)
      if ("1" %in% sexos) {
        lantalmac<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,1]
        lantalmac[which(is.na(lantalmac))]<-0
        lanedadmac<-as.data.frame((as.matrix(t(lantalmac)) %*% as.matrix((edad[edad$sexo==1,sonedad]))))
        lanedad<-lanedadmac
      }
      if ("2" %in% sexos) {
        lantalhem<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,2]
        lantalhem[which(is.na(lantalhem))]<-0
        lanedadhem<-as.data.frame((as.matrix(t(lantalhem)) %*% as.matrix((edad[edad$sexo==2,sonedad]))))
        lanedad<-lanedadhem+lanedad
      }
      if ("3" %in% sexos) {
        lantalund<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,3]
        lantalund[which(is.na(lantalund))]<-0
        lanedadund<-as.data.frame((as.matrix(t(lantalund)) %*% as.matrix((edad[edad$sexo==3,sonedad]))))
        lanedad<-lanedadund+lanedad
      }
    }
    else {
      lantal<-tapply(ntalls$numer,ntalls[,c(1,2)],sum)
      lantal[which(is.na(lantal))]<-0
      lanedad<-as.data.frame((as.matrix(t(lantal)) %*% as.matrix((edad[,sonedad]))))
    }
    nedad<-substr(names(lanedad),2,nchar(names(lanedad)))
    lanedad<-cbind(as.numeric(as.character(dimnames(lanedad)[[1]])),lanedad)
    names(lanedad)<-c("lance",nedad)
    lanedad<-merge(lan,lanedad,by.x="lance",by.y="lance",all.x=T)
    for (i in 1:ncol(lanedad)) {
      if (!identical(as.numeric(which(is.na(lanedad[,i]))),numeric(0))) {lanedad[which(is.na(lanedad[,i])),i]<-0}
    }
    lansect<-as.vector(tapply(lanedad$sector,lanedad$sector,length))
    areas<-as.vector(tapply(as.numeric(as.character(lanedad$arsect)),lanedad$sector,mean))
    taplan<- function(x) {tapply(x,lanedad$sector,mean)}
    edadsect<-apply(lanedad[,c(4:ncol(lanedad))],2,taplan)
    weiman<- function(x) {weighted.mean(x,areas)}
    edadsect<-as.data.frame(t(rbind(edadsect,apply(edadsect,2,weiman))))
    names(edadsect)<-c(names(edadsect)[c(1:(ncol(edadsect)-1))],"total")
    edadsect
    #		print(lanedad[order(as.numeric(as.character(lanedad$lance))),])
  }
}
