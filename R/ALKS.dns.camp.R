#' Datos de claves talla edad en campañas en dns
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa", Mediterraneo "Medit"
#' @param camp Permite elegir las campañas para filtrar sólo las que interesen, N para demersales, P para Porcupine, 1 o 2 para Arsa, M para Medits... Si es "" como es por defecto saca todos los datos de los ficheros edadXXX
#' @return Devuelve un data.frame con las especies con datos de edad en las campañas presentes en el "directorio" dns
#' @examples ALKs.dns.camp("Porc","P")
#' @examples ALKs.dns.camp("Arsa","1")
#' @family edades
#' @export
ALKs.dns.camp<- function(dns,camp="") {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
   dumb<-DBI::dbListTables(ch1)
   dumb<-subset(dumb,nchar(dumb)==7)
   camps.ed<-substr(dumb[grepl("EDAD",dumb,1,7)],1,7)
   if (length(camps.ed)==0) stop(paste0("No hay claves talla edad en ",dns))
   camps.ed<-subset(camps.ed,subset=substr(camps.ed,1,nchar(paste0("EDAD",camp)))==paste0("EDAD",camp))
   if (nrow(DBI::dbReadTable(ch1,camps.ed[1]))>0) ages<-cbind(camp=camps.ed[1],DBI::dbReadTable(ch1,camps.ed[1]))
   else {
     ages<-data.table::data.table(matrix(nrow=0,ncol=22))
     names(ages)<-c("camp",names(DBI::dbReadTable(ch1,camps.ed[1])))
   }
   if (length(camps.ed)>1) {
   for (i in 2:length(camps.ed)) {
     if (nrow(DBI::dbReadTable(ch1,camps.ed[i]))>0) ages<-rbind(ages,cbind(camp=camps.ed[i],DBI::dbReadTable(ch1,camps.ed[i])))
   }
   }
   DBI::dbDisconnect(ch1)
   results<-tapply(ages$NL,ages[,c("camp","ESP")],"length")
   especies<-buscaesp(1,colnames(results)[1])
   if (ncol(results)>1) for (i1 in 2:ncol(results)) {especies<-c(especies,buscaesp(1,colnames(results)[i1]))} else {especies<-buscaesp(1,colnames(results))}
   colnames(results)<- especies
   rownames(results)<-substr(rownames(results),5,8)
   print(t(results))
}
