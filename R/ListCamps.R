#' Listado de campañas presentes en dns con ficheros que contienen cada una utilizar mejor campsDNS.camp
#'
#' Función de acceso a datos: A partir de ficheros de camp.dbf,fauna.dbf,ntall.dbf da un listado de campañas presentes en el directorio con sus ficheros
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param tablas Si TRUE muestra al final una lista con todos los ficheros de definición (CAMP con su identificador de campaña), fauna y tallas presentes en el directorio.
#' @return Devuelve el listado de ficheros dbf campañas que hay en el directorio; campXXX.dbf,faunaXXX.dbf, ntallXXX.dbf, edadXXX.dbf, hidroxxx.dbf
#' @examples ListCamps(dns="Porc",tablas=T)
#' @export
ListCamps<-function(dns,tablas=TRUE) {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
  dumb<-DBI::dbListTables(ch1)
  dumb<-unlist(dumb)
  dumb<-dumb[nchar(dumb)<9]
  typear<-c("EDAD","CAMP","LANCE","FAUNA","NTALL","HIDRO")
  dumb_b<-dumb[c(grepl(typear[1],substr(dumb,1,4)) & nchar(dumb)==7)]
  dumb_b<-c(dumb_b,dumb[grepl(typear[2],substr(dumb,1,4))])
  for (i in typear[3:5]) {
    dumb_b<-c(dumb_b,dumb[grepl(i,substr(dumb,1,5))])
  }
  dumb_b<-data.frame(fic=dumb_b[nchar(dumb_b)<9],camp=ifelse(nchar(dumb_b)==7,substr(dumb_b,5,7),substr(dumb_b,6,8)))  #fichasnxx.dbf
  for (i in as.character(unique(dumb_b$camp))) {
    print(c(i,as.character(dumb_b[dumb_b$camp==i,]$fic)))
  }
}
