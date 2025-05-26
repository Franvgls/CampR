#' Capturas medias por lance de cada especie de gr (grupo) de varias campañas *Solo lances estándar, para incluir lances especiales utilizar Fauna.camp o ListFauna.lans*
#'
#' Ver documentación ListFauna.camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros.
#' @param camps campañas de las que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits "Medi"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr en cada campaña. Columnas: camp,gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) ordenadas por campaña y abundancia en peso decreciente dentro de la campaña
#' @examples ListFauna.camps(gr="1",Nsh[25:27],"Cant",excl.sect=NA)
#' @examples ListFauna.camps(gr="1",Nsh[25:26],"Cant",excl.sect="A")
#' @family ListadosFauna
#' @export
ListFauna.camps<- function(gr="1",camps,dns,cor.time=TRUE,excl.sect=NA) {
  dumb<-cbind(camp=camps[1],ListFauna.camp(gr,camps[1],dns,cor.time=cor.time,excl.sect))
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      dumb<-rbind(dumb,cbind(camp=i,ListFauna.camp(gr,i,dns,cor.time=cor.time,excl.sect)))
    }
  }
  dplyr::arrange(dumb,camp,-peso)
}
