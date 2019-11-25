#' Outliers en capturas segun la relación talla-peso de todos los peces de una campaña
#'
#' Busca outliers en las capturas según los datos en el fauna y las tallas medidas segun la relación talla-peso de todos los peces que hayan aparecido en una campaña. Se puede salir de la función cerrando el gráfico y se puede obtener una función para la campaña y usarla en cada lance
#' @param camp Campaña de la que se procesan y comprueban los posibles outliers en el peso de las capturas de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa", Medits: "Medi"
#' @param nlans Limita la comprobación a las especies que hayan sido capturadas en más lances de nlans (por defecto nlans=4)
#' @param out.dat FALSE si TRUE da un output con todos los errores detectados y no produce los plots por especie en la campaña
#' @return Produce un gráfico por cada especie de pez medida en más de nlans lances. Para salir a mitad cerrar la ventana de gráfico o escape en Rstudio.
#' @seealso {\link{qcLW.camp}}
#' @examples qcLWbucl.camp("P11","Porc",nlans=1)
#' @family Control de calidad
#' @export
qcLWbucl.camp<- function(camp="P12",dns="Porc",nlans=2,out.dat=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dumblist<-ListFauna.camp(gr=1,camp,dns=dns)
#  ch2<-DBI::dbConnect(odbc::odbc(), dns)
  ch1<-DBI::dbConnect(odbc::odbc(), "Camp")
  especie<-DBI::dbGetQuery(ch1,paste0("select esp,especie,a,b from Especies where grupo='",1,"'"))
  DBI::dbDisconnect(ch1)
  especie<-especie[especie$esp %in% formatC(dumblist$esp,width=3,flag=" ") & especie$a>0,]
  dumblist<-dumblist[dumblist$esp %in% especie$esp,]
  dumblist<-dumblist[order(dumblist$nlan,decreasing=TRUE),]
  if (!par("ask")) par(ask=TRUE)
  if (out.dat) dats<-NULL
  for (i in 1:nrow(dumblist)) {
    if (out.dat) dats<-rbind(dats,cbind(qcLW.camp(1,dumblist$esp[i],camp=camp,dns=dns,out.dat=out.dat,plot = F),especie=buscaesp(1,dumblist$esp[i])))
    else qcLW.camp(1,dumblist$esp[i],camp=camp,dns=dns,out.dat=out.dat)
    if (dumblist$nlan[i]==nlans) break
  }
  par(ask=FALSE)
  if (out.dat) {returnValue(dats)}
}

