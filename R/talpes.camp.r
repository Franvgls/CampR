#' Parámetros a y b relación talla-peso
#'
#' Extrae los parámetros a y b de la relación talla-peso de una especie determinada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @family datos_especies
#' @export
talpes.camp<-function(gr,esp,dns="Camp") {
  esp<-format(esp,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), ifelse(dns=="Camp","Camp","Arsa"))
  if (length(gr)>1 | length(esp)>1) {
    stop("Esta funcion no permite mas de una especie por vez")
  }
  else especie<-DBI::dbGetQuery(ch1,paste("select ESPECIE,A,B from Especies where grupo='",gr,"' and esp='",esp,"'",sep=""))
  DBI::dbDisconnect(ch1)
  if (especie$B==0 | especie$A==0) stop(paste("No existen A o B para",especie$ESPECIE))
  else especie<-as.vector(especie[,2:3],mode="numeric")
  especie
}
