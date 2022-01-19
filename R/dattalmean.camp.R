#' **Talla media** y **percentil** de la distribución de tallas por lance en la campaña **Xxx**
#'
#' Muestra la talla media, el percentil **quant** de la distribución de tallas de las especie *esp* en cada lance en la campaña seleccionada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año comcreto (xx): Demersales "Nxx", Porcupine "Pxx", Arsa primavera "1XX", Arsa otoño "2xx" Medits "Mxx"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" y Mediterráneo "Medi", gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param quant elije el percentil deseado, 0.5 para la mediana
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados (3), NA escoge sin tener en cuenta el sexo
#' @return Devuelve un data.frame con campos: lan, lat, long, prof, weight.time, numero, tallamed, qtile
#' @seealso {\link{dattalgr.camp}}
#' @examples db<-dattalmean.camp(gr=1,esp=50,camp="N94",dns="Cant",quant=0.85);MapNort(places=T);points(lat~long,db,pch="_",cex=(tallamed/hablar::max_(tallamed))*10,bg="blue");title("Talla media ejemplares capturados en lance");print(db)
#' @export
dattalmean.camp<- function(gr,esp,camp,dns="Porc",quant=.5,cor.time=TRUE,incl2=TRUE,sex=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  if (length(gr)>1 | as.character(gr)=="9") {stop("No tiene sentido mezclar grupos y considerar tallas de diferentes grupos taxonómicos")}
  if (length(esp)>1 | as.character(esp)=="999") {stop("No tiene sentido mezclar especies para calcular su talla")}
  if (length(esp)==1) {
    if (gr!="9" & esp!="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'AND esp='",esp,"'"))} # tallas[tallas$GRUPO==as.character(gr) & tallas$ESP==format(esp,width=3,justify="r"),c(1,4,7,6,8,5,9)] }
    if (gr!="9" & esp=="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'"))}
    if (gr=="9" & esp=="999") {
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,cate,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where NOT grupo='",6,"'")) }
  }
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,incl2=incl2)
  lan<-lan[,c("lance","lat","long","prof","weight.time")]
  names(lan)<-c("lan","lat","long","prof","weight.time")
  names(ntalls)<-gsub("_", ".",tolower(names(ntalls)))
  if (!is.na(sex)) ntalls<-ntalls[ntalls$sexo==sex,]
  mm<-data.frame(lan=lan$lan,lat=lan$lat,long=lan$long,prof=lan$prof,weight.time=lan$weight.time,numero=0)
#  else {
    ntalls$lance<-as.numeric(as.character(ntalls$lance))
    ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
    ntalls$talnum<-(ntalls$talla+.5)*ntalls$numer
    dumbmed<-tapply(ntalls$numer,ntalls[,c("talla","lance")],sum,na.rm=T)
    dumbmed[is.na(dumbmed)]<-0
    meantal<-colSums(tapply(ntalls$talnum,ntalls[,c("talla","lance")],sum,na.rm=T),na.rm=T)/tapply(ntalls$numer,ntalls[,c("lance")],sum)
    qtile<-quantile(rep(as.numeric(dimnames(dumbmed)$talla)+.5,as.vector(dumbmed[,1])),quant)
    for (i in 2:dim(dumbmed)[2]) {
      qtile<-c(qtile,quantile(rep(as.numeric(dimnames(dumbmed)$talla)+.5,as.vector(dumbmed[,i])),quant))
      }
    ntalls<-tapply(ntalls$numer,ntalls$lance,sum)
    absp<-data.frame(lance=as.numeric(as.character(names(ntalls))),numero=ntalls,
                     tallamed=as.numeric(meantal),qtile=as.numeric(qtile))
    mm<-merge(lan,absp,by.x="lan",by.y="lance",all.x=TRUE)
    if (!identical(as.numeric(which(is.na(mm[,6]))),numeric(0))) {
      mm[which(is.na(mm[,6])),6]<-0
      mm[which(is.na(mm[,7])),7]<-0
      mm[which(is.na(mm[,8])),8]<-0
    }
    if (any(cor.time,camp=="N83",camp=="N84",na.rm=TRUE)) {
      if (any(mm$weight.time==0)) {
        mm$weight.time[mm$weight.time==0]<-c(.1)
        message("Hay lances con duración 0 minutos, revisa validez")
      }
      mm$numero<-round(mm$numero/mm$weight.time,1)
    }
    mm
  }

