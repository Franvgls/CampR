#' Datos de tallas y sexo con geografía (para uso en especies a evaluar con datos de tallas: L. circularis, Phycis...)
#'
#' Función de salida de datos:
#' Extrae los datos de abundancia por talla y sexo de una especie y les añade los datos de su ubicación geográfica y profundidad
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sex Si T muestra los datos por sexo
#' @param verbose Si T muestra avisos problemas de tallas entre distintas especies
#' @return Devuelve un data.frame con variables: Species,	year,	survey,	station,	length,	sex,	number,	latitude,	longitude,	depth,
#' @seealso {\link{datos.camp}}
#' @examples dattal.camp("1"," 50",paste0("P0",7),"Porc",excl.sect=c("B","C"))
#' @export
datTalaGeo.camp<- function(gr,esp,camps,dns,cor.time=TRUE,excl.sect=NA,sex=TRUE,verbose=TRUE) {
  #if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  esp<-format(esp,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  ntalls<-cbind(DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camps[1],
                                     " where grupo='",gr,"' and esp='",esp,"'")),camp=camps[1])
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      ntalls2<-cbind(DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",i,
                                       " where grupo='",gr,"' and esp='",esp,"'")),camp=i)
      ntalls<-rbind(ntalls,ntalls2)
    }
  }
  dumblan<-datlan.camp(camps,dns,redux=T,incl0=T)
  dumblan$sector<-as.character(dumblan$sector)
  ntalls$num<-ifelse(ntalls$peso_gr!=ntalls$peso_m,ntalls$numer*ntalls$peso_gr/ntalls$peso_m,ntalls$numer)
  cefNtal<-ntalls[,c("lance","camp","talla","sexo","numer")]

  #names(cefNtal)<-tolower(names(cefNtal))
  names(cefNtal)[1]<-"lan"
  names(dumblan)[2]<-"lan"
  cefNtal$lan<-as.numeric(cefNtal$lan)
  cefNtal$camp<-as.character(cefNtal$camp)
  dumblan$lan<-as.numeric(dumblan$lan)
  dumblan$camp<-as.character(dumblan$camp)
  cefNtal.lan<-merge(cefNtal,dumblan[,c("camp","lan","lat","long","prof","weight.time","sector","validez")],all.y=F)
  cefNtal.lan$lan<-as.numeric(cefNtal.lan$lan)
  cefNtal.lan$year<-camptoyear(cefNtal.lan$camp)
  cefNtal.lan<-cefNtal.lan[order(cefNtal.lan$year,cefNtal.lan$lan,cefNtal.lan$talla),]
  cefNtal.lan$species<-buscaesp(gr,esp)
  cefNtal.lan<-cefNtal.lan[,c("species","year","camp","lan","validez","talla","sexo","numer","lat","long","prof","sector","weight.time")]
  names(cefNtal.lan)<-c("species","year","survey","station","valid","length","sex","number","latitude","longitude","depth","sector","weight.time")
#  write.csv(cefNtal.lan,"clipboard",row.names=F)
  as.data.frame(cefNtal.lan)
}
