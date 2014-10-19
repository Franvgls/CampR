#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código del grupo, especie y la familia a partir del nombre o fragmento de la especie. Es independiente de mayúsculas y minúsculas y puede sacar más de una especie si una parte de sus nombres coincide.
#' @param nomb Nombre científico de la especie o algún fragmento del nombre usando "".
#' @seealso buscaesp {/link[buscaesp]}
#' @export
buscacod<- function(nomb) {
  require(RODBC)
  ch1<-odbcConnect(dsn="camp")
  odbcSetAutoCommit(ch1, FALSE)
  if (length(nomb)>1) stop("Esta función no permite más de una especie por vez")
  else especies<-sqlFetch(ch1,"ESPECIES",as.is=T)
  odbcClose(ch1)
  print(especies[grep(tolower(nomb),tolower(especies$ESPECIE)),c(1,3,4,2)])
  }