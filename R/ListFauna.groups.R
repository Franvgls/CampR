#' Capturas medias por lance de cada especie de gr (grupo) de varias campañas
#'
#' Ver documentación ListFauna.camp
#' @param camp campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits "Medi"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad, por defecto NA no filtra por profunidades, debe ser o NA o un rango con dos profundidades
#' @param incl2 Si T incluye los lances especiales "2", en estos algunas abundancias salen como NaN al no poder calcular los índices estratificados por no tener asociada un área a los estratos
#' @param verbose si T imprime resultados en pantalla si F no.....
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr en cada campaña. Columnas: camp,gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) LOs valores NaN corresponden a especies que sólo aparecen en lances especiales
#' @seealso {\link{ListFauna.camp}}
#' @examples ListFauna.groups(Nsh[length(Nsh)],"Cant",excl.sect=NA,incl2=TRUE)
#' @export
ListFauna.groups<- function(camp,dns,excl.sect=NA,profrange=NA,incl2=FALSE,verbose=TRUE,cor.time=TRUE,kg=TRUE) {
  dumb<-ListFauna.camp(1,camp,dns,excl.sect=excl.sect,profrange=profrange,incl2=incl2,verbose=verbose,cor.time=cor.time)
    for (i in 2:5) {
      dumb<-rbind(dumb,ListFauna.camp(i,camp,dns,excl.sect,profrange=profrange,incl2,cor.time = cor.time))
    }
  dplyr::arrange(dumb,gr,desc(peso))
  }
