#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código el código AphiaID (WormS) a partir de los códigos del especies.dbf de CAMP
#' @param grupo 1=Peces, 2= crustáceos, 3 moluscos, 4 equinodermos, 5 otros invertebrados, 6 basuras sin AphiaID
#' @param esp Número que identifica la especie en el fichero especies.dbf del camp
#' @param dns origen de base de datos a utilizar en la busqueda, camp debería ser el que esté dirigido al del camp por defecto donde está el especies.dbf del camp, se puede crear uno de red.
#' @examples buscaesp(1,87);SacaAphiaID(1,87)
#' @export
SacaAphiaID<- function(grupo,esp,dns="Camp") {
  ch1<-DBI::dbConnect(odbc::odbc(), ifelse(dns=="Camp","Camp","Arsa"))
  if (length(esp)>1 | length(grupo)>1) stop("Esta función no permite más de una especie por vez")
  else name<-sub(" ","_",buscaesp(grupo,esp,dns = dns))
  DBI::dbDisconnect(ch1)
   worrms::wm_name2id(name = name)
#  Encoding(especies$ESPECIE)  <- "UTF-8"
}
