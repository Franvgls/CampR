#' Biomasa y abundancia para un rango de talla
#'
#' Muestra los datos de abundancia de una especie o conjunto de especies de un rango de tallas determinado a partir de las distribuciones de tallas. También puede mostrar los datos de biomasa a partir de la relación talla-peso
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param tmin Talla mínima
#' @param tmax Talla máxima
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @return Devuelve un data.frame con columnas lan,lat,long,prof,weight.time,peso
#' @seealso {\link{datgr.camp}}
#' @examples dattalgr.camp("1",c(44),"N94","Cant",0,45,ind="p")
#' @export
dattalgr.camp<- function(gr,esp,camp,dns="Porc",tmin=1,tmax=999,cor.time=TRUE,incl2=TRUE,sex=NA,ind="n") {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
#  tallas<-DBI::dbGetQuery(ch1,paste0("select * from NTALL",camp))
   if (length(esp)>1 | any(esp=="999")) {
    if (!is.na(sex)) {
      stop("No tiene sentido elegir sexo de más de una especie")
    }
    if (ind=="p") stop("No se pueden sacar pesos de más de una especie")
   }
  if (length(gr)>1 | as.character(gr)=="9") {stop("No tiene sentido mezclar grupos y considerar tallas de diferentes grupos taxonómicos")}
  if (length(esp)==1) {
    if (gr!="9" & esp!="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'AND esp='",esp,"'"))} # tallas[tallas$GRUPO==as.character(gr) & tallas$ESP==format(esp,width=3,justify="r"),c(1,4,7,6,8,5,9)] }
    if (gr!="9" & esp=="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'"))}
    if (gr=="9" & esp=="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where NOT grupo='",6,"'")) }
  }
  else {
    ntalls<-DBI::dbGetQuery(ch1,paste0("select esp,lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'"))
    ntalls<-ntalls[ntalls$esp %in% format(esp,width=3,justify="r"),c("lance","cate","peso_gr","peso_m","talla","sexo","numer")]
    #ntalls<-  #tallas[tallas$GR==gr & tallas$ESP %in% as.integer(esp),c(1,4,7,6,8,5,9)]
  }
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,incl2=incl2)
  lan<-lan[,c("lance","lat","long","prof","weight.time")]
  names(lan)<-c("lan","lat","long","prof","weight.time")
  names(ntalls)<-gsub("_", ".",tolower(names(ntalls)))
  if (!is.na(sex)) ntalls<-ntalls[ntalls$sexo==sex,]
  if (!any(ntalls$talla<=tmax & ntalls$talla>=tmin)) {
    if (ind=="n") mm<-data.frame(lan=lan$lan,lat=lan$lat,long=lan$long,prof=lan$prof,weight.time=lan$weight.time,numero=0)
    else mm<-data.frame(lan=lan$lan,lat=lan$lat,long=lan$long,prof=lan$prof,weight.time=lan$weight.time,peso=0) ; ntalls.error=0
  }
  else {
    ntalls$lance<-as.numeric(as.character(ntalls$lance))
    ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
    if (ind=="p") {
      ab<-talpes.camp(gr,esp)
      ntalls$peso<-(ntalls$numer*ab[1]*(ntalls$talla+.5)^ab[2])/1000
      ntalls.tot<-tapply(ntalls$peso,ntalls[,c("lance","cate")],sum)
      ntalls.capts<-tapply(ntalls$peso.gr/1000,ntalls[,c("lance","cate")],mean)
      ntalls.error<-c(ntalls.capts-ntalls.tot)/ntalls.tot
      #      browser()
      ntalls.corr<-1+(ntalls.capts-ntalls.tot)/ntalls.tot
      ntalls$corr<-NA
      for (i in 1:nrow(ntalls)) {ntalls$corr[i]<-ntalls.corr[as.character(ntalls$lance[i]),as.character(ntalls$cate[i])]}
      ntalls$peso.c<-ntalls$peso*ntalls$corr
      #      browser()
    }
    ntalls<-ntalls[which(ntalls$talla>=tmin & ntalls$talla<=tmax),]
    if (ind=="p") {
      ntalls<-tapply(ntalls$peso.c,ntalls$lance,sum)
      absp<-data.frame(lance=as.numeric(as.character(names(ntalls))),peso=ntalls)
    }
    else {
      ntalls<-tapply(ntalls$numer,ntalls$lance,sum)
      absp<-data.frame(lance=as.numeric(as.character(names(ntalls))),numero=ntalls)
    }
    mm<-merge(lan,absp,by.x="lan",by.y="lance",all.x=TRUE)
    if (!identical(as.numeric(which(is.na(mm[,6]))),numeric(0))) {
      mm[which(is.na(mm[,6])),6]<-0
    }
    if (any(cor.time,camp=="N83",camp=="N84",na.rm=TRUE)) {
      if (any(mm$weight.time==0)) {
        mm$weight.time[mm$weight.time==0]=.1
        message("Hay lances con duración 0 minutos, revisa validez")
      }
      if(ind=="n") mm$numero<-round(mm$numero/mm$weight.time,1)
      if(ind=="p") mm$peso<-round(mm$peso/mm$weight.time,3)
    }
  }
  if (length(esp)>1) {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  }
  if (ind=="p") {
    if (exists("ntalls.error") & any(ntalls.error>.3,na.rm=TRUE)) {
      texto<-paste(paste0("qcLW.camp(",gr),esp,camp,paste0(dns,")"),sep=",")
      print(paste("Estimaciones peso regresión mayores que datos en un 30% para algún lance, compruebe",texto))
    }
  }
  mm
}
