#' Listado de especies por campaña sin  datos de las especies del grupo, sólo nombres
#'
#' @details Muestra un listado de todas las especies capturadas del grupo gr. *Es importante tener en cuenta que sólo sirve para los lances estándares* y no tiene en cuenta los datos de los lances especiales. *Para sacar resultados con lances especiales y fuera de estratos de profundidad utilizar ListFaunaTals.camp*.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros.
#' @param camp Campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa", Medits "Medi"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 si T incluye los lances especiales con listado de especies en los lances especiales y en los estándar
#' @param getaphia si T busca en internet (debe estar disponible) los códigos AphiaID de las especies en cuestión
#' @param verbose si T saca en pantalla, si F no salen en pantalla
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr. Columnas: gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) Los valores NaN en las abundancias corresponden a especies que sólo han aparecido en los lances especiales, y que no puede calcularse la abundancia estratificada al no contar con áreas para los estratos en que aparecen
#' @examples ListFauna.camp(gr=1,camp="N12",dns="Cant",excl.sect=NA)
#' @family ListadosFauna
#' @export
Fauna.camp<- function(gr="1",camp,dns,excl.sect=NA,incl2=TRUE,verbose=FALSE,getaphia=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(gr)>1 | gr==9) {stop("no se pueden mezclar grupos en esta función")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  listsps<-DBI::dbGetQuery(ch1,paste0("select esp,lance from FAUNA",camp," where grupo='",gr,"'"))
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2==incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","validez")]
  lan$lance<-format(lan$lance,width=3,justify = "r")
  dumb<-merge(listsps,lan)
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  listaesp<-unique(dumb$esp)
  ndat<-length(listaesp)
  # dumbtap<-tapply(dumb$esp,dumb[,c("esp","validez")],length)
  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,AphiaID=NULL,nlan=NULL)
  for (i in 1:ndat) {
    tempesp<-buscaesp(gr,listaesp[i])
    dumbres<-rbind(dumbres,data.frame(gr=gr,esp=listaesp[i],especie=tempesp)                                )
  }
  dumbres$especie<-as.character(dumbres$especie)
  if (any(!is.na(excl.sect))) print(paste("Excluidos los sectores/estratos",excl.sect))
  if (getaphia) {
    for (i in 1:nrow(dumbres)) dumbres$AphiaID[i]<-worms::wormsbynames(dumbres$especie[i],verbose=verbose)$AphiaID
  }
  dumbres[order(as.character(dumbres$especie,decreasing=FALSE)),]
}
