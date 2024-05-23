#' Unidades en que se mide una especie
#'
#' Da información de las unidades en que se miden las especies y permite añadir datos a los gráficos
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param dns Es el ODBC donde se busca el fichero especies, deberías ser Camp por defecto pero se puede utilizar "Arsa" si se tiene el especies del Golfo de Cádiz añadiéndole el campo MED numérico y longitud 1
#' @examples
#' unid.camp(1,50)
#' @export
unid.camp<-function(gr,esp,dns="Camp"){
  esp<-format(esp,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), ifelse(dns=="Camp","Camp","Arsa"))
  if (length(gr)>1 | length(esp)>2) {
    stop("Esta función no permite más de una especie por vez")
  }
  else especie<-DBI::dbGetQuery(ch1,paste("select ESPECIE,MED,INCREM from Especies where grupo='",gr,"' and esp='",esp,"'",sep=""))
  DBI::dbDisconnect(ch1)
  especie
}
