#' Capturas de un todos los lances en una campaña
#'
#' Muestra un listado de las especies capturadas en peso y número en un lance concreto. Se pueden seleccionar grupos de especies
#' @param camp Campaña de la que se extrae los datos del lance (lan) un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param lan Lance del que se extrae la faunística
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 basura. Si NA, muestra todos los grupos
#' @param excl.sect Sirve para excluir uno o más sectores o estratos, ponerlos como un vector por separado, i.e. c("A","B")
#' @param incl2 Si TRUE se incuyen los lances especiales
#' @return Devuelve un data.frame con columnas grupo,esp, lance,especie,peso,numero
#' @family ListadosFauna
#' @examples ListFauna.lans("12C","Cant",gr=c(1,2))
#' @export
ListFauna.lans<- function(camp,dns="Porc",gr=NA,excl.sect=NA,incl2=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  lans<-datlan.camp(camp=camp,dns=dns,excl.sect = excl.sect,redux=T,incl2=incl2)
  datos<-ListFauna.lan(camp,dns,lans$lance[1],gr=gr,out=FALSE)
  for (i in lans$lance[2:length(lans)]) {
    datos<-rbind(datos,ListFauna.lan(camp,dns,i,gr=gr,out=FALSE))
  }
  print(setorder(datos,lance,grupo,-peso))
}
