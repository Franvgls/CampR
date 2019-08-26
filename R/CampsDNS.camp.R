#' Listado de campañas presentes en el directorio e información sobre ellas
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @return Un data.frame con columnas con la lista de archivos campXXX,lanceXXX,faunaXXX,ntallXXX e hidroXXX en el directorio, rellena los que falten con "."
#' @examples CampsDNS.camp("Porc")
#' @export
CampsDNS.camp<- function(dns) {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
  dumb<-DBI::dbListTables(ch1)
  DBI::dbDisconnect(ch1)
  dumb<-unlist(dumb)
  dumb<-dumb[nchar(dumb)<9]
  camps.c<-dumb[grepl("CAMP",dumb)] #substr(dumb[grepl("CAMP",dumb)],5,7)
  camps.l<-dumb[grepl("LANCE",dumb)] #substr(dumb[grepl("LANCE",dumb)],6,8)
  camps.f<-dumb[grepl("FAUNA",dumb)]  #substr(dumb[grepl("FAUNA",dumb)],6,8)
  camps.t<-dumb[grepl("NTALL",dumb)]  #substr(dumb[grepl("NTALL",dumb)],6,8)
  camps.h<-dumb[grepl("HIDRO",dumb)]
  Narchs<-max(length(camps.c),length(camps.l),length(camps.f),length(camps.t),length(camps.h))
  if (length(camps.c)<Narchs) camps.c<-c(camps.c,rep(".",Narchs-length(camps.c)))
  if (length(camps.l)<Narchs) camps.l<-c(camps.l,rep(".",Narchs-length(camps.l)))
  if (length(camps.f)<Narchs) camps.f<-c(camps.f,rep(".",Narchs-length(camps.f)))
  if (length(camps.t)<Narchs) camps.t<-c(camps.t,rep(".",Narchs-length(camps.t)))
  if (length(camps.h)<Narchs) camps.h<-c(camps.h,rep(".",Narchs-length(camps.h)))
  message(paste("Directorio:",dumbdir))
  print(data.frame(Camp=camps.c,Lance=camps.l,Fauna=camps.f,Tallas=camps.t,Hidro=camps.h))
  }
