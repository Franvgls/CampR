#' Abundancia estratificada por talla y sexo
#'
#' Función de acceso a datos:
#' Extrae los datos de abundancia por talla y sexo de una especie o conjunto de especies a partir de las distribuciones de talla
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sex Si T muestra los datos por sexo
#' @param verbose Si T muestra avisos problemas de tallas entre distintas especies
#' @return Devuelve un data.frame con variables: talla, machos, hembras e indet(erminados) si existen todos y si sex=TRUE
#' @seealso {\link{datos.camp}}
#' @examples dattal.camp("1"," 50",paste0("P0",7),"Porc",excl.sect=c("B","C"))
#' @export
dattal.camp<- function(gr,esp,camp,dns,cor.time=TRUE,excl.sect=NA,sex=TRUE,verbose=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  abesp<-datos.camp(gr,esp,camp,dns,cor.time=cor.time)
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  if (length(esp)>1 | any(esp=="999")) {
    increm<-NULL;medida<-NULL
    for (i in esp) {
      increm<-c(increm,as.numeric(unid.camp(gr,i)["INCREM"]))
      medida<-c(medida,ifelse(unid.camp(gr,i)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm")))
    }
    if (length(unique(increm))>1 | length(unique(medida))>1) stop("Seleccionadas especies medidas en distintas unidades (mm y cm o .5 cm) o a la aleta anal")
    else increm<-unique(increm);medida<-unique(medida)
  }
  else {
    increm<-unid.camp(gr,esp)["INCREM"]
    medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm"))
  }
  if (length(esp)==1) {
    if (esp!="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"' and esp='",esp,"'"))
      #browser()
      if (nrow(ntalls)==0 | sum(abesp$numero)==0) {ntalls<-data.frame(lance=abesp[1,"lance"],peso_gr=0,peso_m=.1,talla=1,sexo="3",numer=0,stringsAsFactors=FALSE)}
    }
    if (esp=="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'"))
    }
  }
  if (length(esp)>1) {
    ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                               " where grupo='",gr,"' and esp='",esp[1],"'"))
    for (i in 2:length(esp)) {
      ntalls<-rbind(ntalls,DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                                              " where grupo='",gr,"' and esp='",esp[i],"'")))
    }
    ntalls$sexo<-3
  }
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls$lance<-as.numeric(as.character(ntalls$lance))
  ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  lan<-datlan.camp(camp,dns,incl2=FALSE)[,c("lance","sector","weight.time","estrato")]
  lan<-lan[!is.na(lan$estrato),]
  lan<-lan[,c("lance","sector","weight.time")]
  if (any(cor.time,camp=="N83",camp=="N84")) {
    ntalls<-merge(ntalls,lan,by.x="lance",by.y="lance")
    if (any(ntalls$weight.time==0)) {
      ntalls$weight.time[ntalls$weight.time==0]=.1
      message("Hay lances con duración 0 minutos, revisa validez")
    }
    ntalls$numer<-ntalls$numer/ntalls$weight.time
    ntalls<-ntalls[,1:6]
  }
  DBI::dbDisconnect(ch1)
  if (any(!is.na(excl.sect))) {
    abesp$sector<-gsub("NA","N",abesp$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(abesp$sector)))>0) abesp<-abesp[-grep(excl.sect[i],as.character(abesp$sector)),]}
    print(abesp)
    abesp$sector<-factor(as.character(abesp$sector))
  }
  dumb<-merge(abesp,ntalls,by.x="lance",by.y="lance",all.x=TRUE)
  for (i in 1:ncol(dumb)) {
    if (!identical(as.numeric(which(is.na(dumb[,i]))),numeric(0))) {
      dumb[which(is.na(dumb[,i])),i]<-0
    }
  }
  dumb$lance<-as.numeric(dumb$lance)
  dumb$arsect<-as.numeric(dumb$arsect)
    if (any(levels(factor(dumb$sexo,exclude=0))!="3")) dumb$sexo<-factor(dumb$sexo,levels=c(1:3),exclude=0) else dumb$sexo=3
  abesp$arsect<-as.numeric(abesp$arsect)
  if (all(is.na(dumb$sexo))) {
    dumb$sexo<-as.character(dumb$sexo)
    dumb[is.na(dumb$sexo),"sexo"]<-3
  }
  if (sum(dumb$peso.m,na.rm=TRUE)==0) dumb[1,c("peso.m","talla")]<-c(.1,1)
  dumb1<-tapply(dumb$numer,dumb[,c("talla","sector","sexo")],sum)
  dumb1[which(is.na(dumb1))]<-0
  lansect<-as.vector(tapply(abesp$sector,abesp$sector,length))
  lansect[which(is.na(lansect))]<-0
  sxs<- match(c(1:3),dimnames(dumb1)$sexo)
  #browser()
  if (!is.na(sxs[1])) {
    i<-which(!is.na(match(dimnames(dumb1)$sexo,1)))
    dumb1m<-dumb1[,,i]
    if (ncol(dumb1)>1) {
      for (i in (1:nrow(dumb1m))) {dumb1m[i,]<-dumb1m[i,]/lansect}
    }
    else dumb1m<-dumb1m/lansect
  }
  if (!is.na(sxs[2])) {
    i<-which(!is.na(match(dimnames(dumb1)$sexo,2)))
    dumb1h<-dumb1[,,i]
    if (ncol(dumb1)>1) {
      for (i in (1:nrow(dumb1h))) {dumb1h[i,]<-dumb1h[i,]/lansect}
    }
    else dumb1h<-dumb1h/lansect
  }
  if (!is.na(sxs[3])) {
    i<-which(!is.na(match(dimnames(dumb1)$sexo,3)))
    dumb1i<-dumb1[,,i]
    if (ncol(dumb1)>1) {
      for (i in (1:nrow(dumb1i))) {dumb1i[i,]<-dumb1i[i,]/lansect}
    }
    else dumb1i<-dumb1i/lansect
  }
  areas<-as.vector(tapply(abesp$arsect,abesp$sector,mean))
  weiman<-function(a,b) {weighted.mean(a,b,na.rm=TRUE)}
  if (length(areas)>1) {
    #browser()
    dtall<- data.frame(cbind(if (!is.na(sxs[1])) apply(dumb1m,1,weiman,b=areas),
                             if (!is.na(sxs[2])) apply(dumb1h,1,weiman,b=areas),
                             if (!is.na(sxs[3])) apply(dumb1i,1,weiman,b=areas)))
  }
  else dtall<-data.frame(cbind(if (!is.na(sxs[1])) dumb1m,
                               if (!is.na(sxs[2])) dumb1h,
                               if (!is.na(sxs[3])) dumb1i))
  dtalln<-c("machos","hembras","indet")
  dtall<-as.data.frame(cbind(as.numeric(dimnames(dtall)[[1]]),dtall))
  names(dtall)<-c("V1",dtalln[which(!is.na(sxs))])
  dumb<-as.data.frame(c(1:(trunc(ifelse(nrow(dtall)>0,max(dtall[,1]),1)/10)*10+10)))
  names(dumb)<-"talla"
  dtall<-merge(dumb,dtall,by.x="talla",by.y="V1",all.x=TRUE)
  for (i in 2:ncol(dtall)) {
    if (!identical(as.numeric(which(is.na(dtall[,i]))),numeric(0))) {
      dtall[which(is.na(dtall[,i])),i]<-0
    }
  }
  # if (length(esp)>1 & verbose) {
  #   print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  # }
  if (!sex & ncol(dtall)>2) { dtall<-data.frame(talla=dtall[,1],numero=rowSums(dtall[,2:ncol(dtall)])) }
  if (!sex & ncol(dtall)==2) names(dtall)<-c("talla","numero")
  #browser()
  as.data.frame(dtall)
}
