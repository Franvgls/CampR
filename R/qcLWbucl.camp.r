#' Outliers en capturas segun la relación talla-peso de todos los peces de una campaña
#'
#' Busca outliers en las capturas según los datos en el fauna y las tallas medidas segun la relación talla-peso de todos los peces que hayan aparecido en una campaña. Se puede salir de la función cerrando el gráfico y se puede obtener una función para la campaña y usarla en cada lance
#' @param camp Campaña de la que se procesan y comprueban los posibles outliers en el peso de las capturas de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa", Medits: "Medi"
#' @param nlans Limita la comprobación a las especies que hayan sido capturadas en más lances de nlans (por defecto nlans=4) 
#' @return Produce un gráfico por cada especie de pez medida en más de nlans lances. Para salir a mitad cerrar la ventana de gráfico o escape en Rstudio.
#' @seealso {\link{qcLW.camp}}
#' @examples qcLWbucl.camp("P11","Pnew",nlans=1)
#' @family Control de calidad
#' @export
qcLWbucl.camp<- function(camp="P12",dns="Pnew",nlans=2) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dumblist<-ListFauna.camp(gr=1,camp,dns=dns)
  ch1<-odbcConnect(dsn="camp")
  odbcSetAutoCommit(ch1, FALSE)
  especie<-sqlQuery(ch1,paste("select ESP,ESPECIE,A,B from Especies where grupo='",1,"'",sep=""))
  odbcClose(ch1)
  especie<-especie[especie$ESP %in% dumblist$esp & especie$A>0,]
  dumblist<-dumblist[dumblist$esp %in% especie$ESP,]
  dumblist<-dumblist[order(dumblist$nlan,decreasing=TRUE),]
  #browser()
  if (!par("ask")) par(ask=TRUE)
  for (i in 1:nrow(dumblist)) {
    qcLW.camp(1,dumblist$esp[i],camp=camp,dns=dns,out.dat=FALSE)
    if (dumblist$nlan[i]==nlans) break
  }
  par(ask=FALSE)
}