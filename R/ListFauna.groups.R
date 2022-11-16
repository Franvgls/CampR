#' Capturas medias por lance de cada especie de gr (grupo) de varias campañas *Solo lances estándar, para incluir especiales utilizar fauna.camp o ListFauna.lans*
#'
#' Ver documentación ListFauna.camp
#' @details Por defecto extrae los datos de todos los grupos taxonómicos menos el 6 (deshechos y no orgánicos), sólo se puede utilizar para los lances estándares y los resultados responden a las abundancias estandarizadas, sólo incluyen los lances estándare, ni extras  ni fuera de estatos áreas de la campaña. Para incluir estos lances utilizar las funciones de ListFaunaTals
#' @param camp campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits "Medi"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param verbose si T imprime resultados en pantalla si F no.....
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr en cada campaña. Columnas: camp,gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) LOs valores NaN corresponden a especies que sólo aparecen en lances especiales
#' @family ListadosFauna
#' @examples
#' ListFauna.groups(Nsh[length(Nsh)],"Cant",excl.sect=NA,incl2=TRUE)
#' @export
ListFauna.groups<- function(camp,dns,excl.sect=NA,verbose=TRUE,cor.time=TRUE,kg=TRUE) {
  dumb<-ListFauna.camp(1,camp,dns,excl.sect=excl.sect,verbose=verbose,cor.time=cor.time)
    for (i in 2:5) {
      dumb<-rbind(dumb,ListFauna.camp(i,camp,dns,excl.sect,cor.time = cor.time))
    }
  dplyr::arrange(dumb,gr,desc(peso))
  }
