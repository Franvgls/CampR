#' Capturas medias por lance de cada especie de gr (grupo) de varias campañas
#'
#' @description Ver documentación en ListFaunaTals.camp
#' @param camp campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits "Medi"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param verbose si T imprime resultados en pantalla si F no.....
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr en cada campaña. Columnas: camp,gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) LOs valores NaN corresponden a especies que sólo aparecen en lances especiales
#' @family ListadosFauna
#' @examples
#' ListFauna.groups(Nsh[length(Nsh)],"Cant",excl.sect=NA,incl2=TRUE)
#' @export
ListFaunaTals.groups<- function(camp,dns,cor.time=F,excl.sect=NA,profrange=NA,incl2=TRUE) {
  dumb<-ListFaunaTals.camp(1,camp,dns,cor.time=cor.time,excl.sect=excl.sect,profrange=profrange,incl2=incl2)
    for (i in 2:5) {
      dumb<-rbind(dumb,ListFaunaTals.camp(i,camp,dns,cor.time=cor.time,excl.sect=excl.sect,profrange=profrange,incl2=incl2))
    }
  dplyr::arrange(dumb,gr,desc(peso_gr))
  }
