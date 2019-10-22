#' Listado de campañas presentes en el directorio e información sobre ellas
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @return Un data.frame con columnas con la lista de archivos campXXX,lanceXXX,faunaXXX,ntallXXX e hidroXXX en el directorio, rellena los que falten con "."
#' @examples CampsDNS.camp("Porc")
#' @family datos
#' @export
CampsDNS.camp<- function(dns) {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
  dumb<-DBI::dbListTables(ch1)
  f_names<-c("LANCE","GRUPO","ESP","PESO_GR","NUMERO")
  dumb<-unlist(dumb)
  dumb<-dumb[nchar(dumb)<9]
  camps.c<-dumb[grepl("CAMP",dumb)]
  camps.l<-dumb[grepl("LANCE",dumb)]
  camps.f<-dumb[grepl("FAUNA",dumb)]
  camps.t<-dumb[grepl("NTALL",dumb)]
  camps.h<-dumb[grepl("HIDRO",dumb)]
  if (nrow(DBI::dbGetQuery(ch1,paste0("select IDENT from ",camps.c[1])))>0){
    NomCamp<-cbind(camp=camps.c[1],DBI::dbGetQuery(ch1,paste0("select IDENT from ",camps.c[1])))
  }
  else NomCamp<-data.frame(camp=NA,ident=NA)
  for (i in 2:length(camps.c)) {
    if (nrow(DBI::dbGetQuery(ch1,paste0("select IDENT from ",camps.c[i])))>0) {
      NomCamp<-rbind(NomCamp,cbind(camp=camps.c[i],DBI::dbGetQuery(ch1,paste0("select IDENT from ",camps.c[i]))))
    }
  }
  DBI::dbDisconnect(ch1)
  nombres<-NomCamp$IDENT
  anyos<-camptoyear(substr(as.character(camps.c),5,7))
  Narchs<-max(length(camps.c),length(anyos),length(camps.l),length(camps.f),length(camps.t),length(camps.h),length(nombres))
  if (length(nombres)<Narchs) nombres<-c(nombres,rep(".",Narchs-length(nombres)))
  if (length(camps.c)<Narchs) camps.c<-c(camps.c,rep(".",Narchs-length(camps.c)))
  if (length(anyos)<Narchs) anyos-c(anyos,rep(".",Narchs-length(anyos)))
  if (length(camps.l)<Narchs) camps.l<-c(camps.l,rep(".",Narchs-length(camps.l)))
  if (length(camps.f)<Narchs) camps.f<-c(camps.f,rep(".",Narchs-length(camps.f)))
  if (length(camps.t)<Narchs) camps.t<-c(camps.t,rep(".",Narchs-length(camps.t)))
  if (length(camps.h)<Narchs) camps.h<-c(camps.h,rep(".",Narchs-length(camps.h)))
  message(paste("Directorio:",dumbdir))
  DD<-data.frame(NomCamp=nombres,Year=anyos,Camp=camps.c,Lance=camps.l,Fauna=camps.f,Tallas=camps.t,Hidro=camps.h)
  DD[order(as.character(DD$Year),DD$Camp),]
}
