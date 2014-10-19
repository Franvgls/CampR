#' Función de búsqueda del nombre de una especie
#'
#' Busca el nombre de una especie a partir del código del grupo y especie.Puede dar el nombre científico, común o en inglés
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param id Nombre científico de la especie ("l"), nombre común ("e"), o nombre en inglés ("i")
#' @seealso buscacod {/link[buscacod]}
#' @export
buscaesp<- function(gr,esp,id="l") {
  require(RODBC)
  esp<-format(esp,width=3,justify="r")
  values<-c("i","e","l")
  if (!id %in% values) stop("Campo id debe ser l: latín, i: inglés o e: español")
  ch1<-odbcConnect(dsn="camp")
  odbcSetAutoCommit(ch1, FALSE)
  if (length(esp)>1) {
    if (id=="l" | id=="e") { especie<-"Varias especies" }
    if (id=="i") { especie<-"Several species" }
  }
  else {
    if (gr!="9" & esp!="999") {
      if (id=="l") {
        especie<-sqlQuery(ch1,paste("select especie from Especies where grupo='",gr,
                                    "' and esp='",esp,"'",sep=""))[[1]]}
      if (id=="i") {
        especie<-sqlQuery(ch1,paste("select nombrei from Especies where grupo='",gr,
                                    "' and esp='",esp,"'",sep=""))[[1]]}
      if (id=="e") {
        especie<- sqlQuery(ch1,paste("select nombree from Especies where grupo='",gr,
                                     "' and esp='",esp,"'",sep=""))[[1]]}
    }
    if (gr!="9" & esp=="999") {
      if (id=="i") {especie<-paste("All",c("Fish","Crustaceans","Molluscs","Echinoderms","Other Invertebrates")[as.numeric(gr)])}
      else {especie<-paste("Total",c("peces","crustáceos","moluscos","equinodermos","otros invertebrados","otros")[as.numeric(gr)])}
      id<-"e"}
    if (gr=="9" & esp=="999") {
      if (id=="i") {especie<-"All Species"}
      else {especie<-"Total especies"}
      id<-"e"}
    odbcClose(ch1)
  }
  if (length(especie)==0) especie<-c("ERROR CODIGO DESCONOCIDO")
  as.character(especie)
}