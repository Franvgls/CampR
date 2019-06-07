#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código del grupo, especie y la familia a partir del nombre o fragmento de la especie. Es independiente de mayúsculas y minúsculas y puede sacar más de una especie si una parte de sus nombres coincide.
#' @param nomb Nombre científico de la especie o fragmento del nombre entre ""
#' @family datos_especies
#' @examples buscacod("sph")
#' @export
buscacod<- function(nomb) {
  ch1<-DBI::dbConnect(odbc::odbc(), "camp")
  if (length(nomb)>1) stop("Esta función no permite más de una especie por vez")
  else especies<-data.table::as.data.table(DBI::dbGetQuery(ch1,"select * from ESPECIES"))
  DBI::dbDisconnect(ch1)
#  Encoding(especies$ESPECIE)  <- "UTF-8"
  print(especies[grep(nomb,especies$ESPECIE,ignore.case=T),c(1,3,4,2)])
  }
