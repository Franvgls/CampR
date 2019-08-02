#' Capturas medias por lance de cada especie de gr (grupo) en una campaña
#'
#' Muestra un listado de la biomasa y número medio de todas las especies capturadas del grupo gr
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros.
#' @param camp Campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", Medits "Medi"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 Si T incluye los lances especiales "2"
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr. Columnas: gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) Los valores NaN en las abundancias corresponden a especies que sólo han aparecido en los lances especiales, y que no puede calcularse la abundancia estratificada al no contar con áreas para los estratos en que aparecen
#' @examples ListFauna.camp(gr=1,camp="N12",dns="Cant",excl.sect=FALSE,incl2=FALSE)
#' @export
ListFauna.camp<- function(gr="1",camp,dns,cor.time=TRUE,excl.sect=NA,incl2=FALSE,kg=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(gr)>1 | gr==9) {stop("no se pueden mezclar grupos en esta función")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  # ch1<-RODBC::odbcConnect(dsn=dns)
  # RODBC::odbcSetAutoCommit(ch1, FALSE)
  listsps<-DBI::dbGetQuery(ch1,paste0("select esp,lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'"))
#  listsps<-RODBC::sqlQuery(ch1,paste("select esp,lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep=""))
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","validez","arsect","weight.time")]
  lan$lance<-format(lan$lance,width=3,justify = "r") #RODBC::odbcClose(ch1)
  dumb<-merge(listsps,lan)
  #browser()
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  # str(listsps)
  listaesp<-levels(factor(dumb$esp))
  ndat<-length(listaesp)
  #print(ndat)
  dumbtap<-tapply(dumb$esp,dumb$esp,length)
  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,peso=NULL,numero=NULL,nlan=NULL)
  #browser()
  for (i in 1:ndat) {
    dumbmedio<-CV.camps(gr=gr,esp=listaesp[i],camp=camp,dns=dns,excl.sect = excl.sect,cor.time=cor.time)
    dumbres<-rbind(dumbres,cbind(gr=gr,esp=listaesp[i],especie=buscaesp(gr,listaesp[i]),
                                 peso=dumbmedio$weight,
                                 numero=dumbmedio$number,
                                 nlan=dumbtap[as.vector(dimnames(dumbtap)[[1]])==listaesp[i]]))
  }
  #browser()
  for (i in 4:6) {dumbres[,i]<-as.numeric(as.character(dumbres[,i]))}
  dumbres[order(as.numeric(as.character(dumbres[,4])),decreasing=TRUE),]
}

