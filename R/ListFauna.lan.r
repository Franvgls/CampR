#' Capturas de un lance específico
#'
#' Muestra un listado de las especies capturadas en peso y número en un lance concreto. Se pueden seleccionar grupos de especies
#' @param camp Campaña de la que se extrae los datos del lance (lan) un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param lan Lance del que se extrae la faunística
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. Si NA, muestra todos los grupos a excepción de 6 desechos y otros no orgánicos
#' @param out salida de la función, por defecto TRUE, pero si FALSE no saca el resultado en pantalla (lo asigna a variable para ListFauna.lans)
#' @return Devuelve un data.frame con columnas grupo,esp,lanceui,especie,peso,numero
#' @seealso {\link{ListFauna.camps}}, {\link{ListFaunaTals.camps}}
#' @examples ListFauna.lan("12C","Cant",1,gr=c(1,2))
#' @export
ListFauna.lan<- function(camp,dns="Porc",lan,gr=NA,out=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  lan<-format(lan,width=3,justify="r")
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  listsps<-DBI::dbGetQuery(ch1,paste0("select grupo,esp,lance,peso_gr,numero from FAUNA",camp," where lance='",lan,"'"))
  DBI::dbDisconnect(ch1)
  if (nrow(listsps)==0) {message(paste("Sin capturas en el lance",lan))}
  if (any(!is.na(gr))) listsps<-listsps[listsps$grupo %in% gr,]
  if (nrow(listsps)==0) {message(paste("Sin capturas del grupo",gr," en el lance",lan))}
  if (nrow(listsps)>0){
  listsps$especie=NA
  listsps$peso=listsps$peso_gr/1000
  for (i in 1:nrow(listsps)) {
    listsps$especie[i]=buscaesp(listsps$grupo[i],listsps$esp[i]) }
  listsps<-listsps[,c("grupo","esp","lance","especie","peso","numero")]
  if (out) print(setorder(listsps,grupo,-peso))
  else setorder(listsps,grupo,-peso_gr)}
  }
