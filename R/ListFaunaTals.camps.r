#' Capturas y tallas mínima y máxima de grupos de especies para todos los lances de varias campaña
#'
#' Ver documentación ListFaunaTals.camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. 6 desechos no se puede elegir en esta función
#' @param camps campañas (años) a representar en el mapa: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad, por defecto NA no filtra por profunidades, debe ser o NA o un rango con dos profundidades
#' @param incl2 Si T incluye los lances especiales "2"
#' @examples ListFaunaTals.camps(gr="1",Nsh[7:27],"Cant",excl.sect=NA,profrange=NA,incl2=TRUE)
#' @examples ListFaunaTals.camps(gr="1",Nsh,"Cant",excl.sect=NA,incl2=TRUE,profrange=c(400,999))
#' @family ListadosFauna
#' @export
ListFaunaTals.camps<-function(gr="1",camps,dns,excl.sect=NA,cor.time=TRUE,profrange=NA,incl2=TRUE) {
  dumb<-cbind(camp=camps[1],ListFaunaTals.camp(gr,camps[1],dns,cor.time = cor.time,excl.sect=excl.sect,
                                               profrange=profrange,incl2))
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      dumb<-rbind(dumb,cbind(camp=i,ListFaunaTals.camp(gr,i,dns,cor.time=cor.time,excl.sect,profrange=profrange,incl2)))
    }
  }
  arrange(dumb,camp,-peso_gr)
}
#ListFaunaTals.camps(gr="1",Nsh[7:27],"Cant",excl.sect=NA,incl2=TRUE)
#ListFaunaTals.camps(gr="1",Msh,"Cant",excl.sect=NA,incl2=TRUE)
