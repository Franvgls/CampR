#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código del grupo, especie y la familia a partir del nombre o fragmento de la especie. Es independiente de mayúsculas y minúsculas y puede sacar más de una especie si una parte de sus nombres coincide.
#' @param nomb Nombre científico de la especie o fragmento del nombre entre ""
#' @param dns origen de base de datos a utilizar en la busqueda, camp debería ser el que esté dirigido al del camp por defecto donde está el especies.dbf del camp, se puede crear uno de red. Se puede utilizar "Arsa" como dns y si se tiene el especies del golfo de Gádiz en el directorio ARSA salen los códigos del Golfo de Cádiz, hay que incluir un camp MED numérico de ancho 1 en ese especies
#' @param buscfam en vez de buscar el nombre de la especie busca a través de la familia y la especie, si T no muestra la lista de especies, sólo la familia, si F no selecciona por familia
#' @family datos_especies
#' @examples buscacod("sph")
#' @export
buscacod<- function(nomb,dns="Camp",buscfam=FALSE) {
  ch1<-DBI::dbConnect(odbc::odbc(), ifelse(dns=="Camp","Camp","Arsa"))
  on.exit(DBI::dbDisconnect(ch1), add = TRUE)
  if (length(nomb)>1) stop("Esta función no permite más de una especie por vez")
  else especies<-data.table::as.data.table(DBI::dbGetQuery(ch1,"select * from ESPECIES"))
  #DBI::dbDisconnect(ch1)
#  Encoding(especies$ESPECIE)  <- "UTF-8"
  if (buscfam) print(especies[grep(nomb,paste(especies$ESPECIE,especies$FAMILIA,ignore.case=T),c()),c(1,3,4,2,17)])
  else print(especies[grep(nomb,especies$ESPECIE,ignore.case=T),c(1,3,4,2,17)])
}
