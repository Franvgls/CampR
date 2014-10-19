#' Capturas medias por lance de cada especie de gr (grupo) en una campaña 
#'
#' Muestra un listado de la biomasa y número medio de todas las especies capturadas del grupo gr 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param camp Campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", Medits "Medi"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 Si T incluye los lances especiales "2"
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr. Columnas: gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp)
#' @export
ListFauna.camp<- function(gr="1",camp,dns,excl.sect=NA,incl2=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  require(RODBC)
  ch1<-odbcConnect(dsn=dns)
  odbcSetAutoCommit(ch1, FALSE)
  listsps<-sqlQuery(ch1,paste("select esp,lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep=""))
  lan<-sqlQuery(ch1,paste("select lance,sector,estrato from LANCE",camp," where validez<>'0'",sep=""),errors=F)
  lan<-data.frame(lance=lan$lance,sector=paste(lan$sector,lan$estrato,sep=""))
  datcamp<-datlan.camp(camp,dns,incl2=ifelse(incl2,T,F),excl.sect=excl.sect)[,c("lance","sector","validez","arsect")]
  odbcClose(ch1)
  dumb<-merge(lan,datcamp)
  dumb<-merge(listsps,dumb)
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
    dumbres<-rbind(dumbres,cbind(gr=gr,esp=listaesp[i],especie=buscaesp(gr,listaesp[i]),
                                 peso=format(round(strmean.camps(dumb$peso_gr[dumb$esp==listaesp[i]],dumb$sector[dumb$esp==listaesp[i]],
                                                                  dumb$arsect[dumb$esp==listaesp[i]],camps=rep(camp,nrow(dumb[dumb$esp==listaesp[i],])))/1000,2),digits=2,nsmall=2),
                                 numero=format(round(strmean.camps(dumb$numero[dumb$esp==listaesp[i]],dumb$sector[dumb$esp==listaesp[i]],
                                                                    dumb$arsect[dumb$esp==listaesp[i]],camps=rep(camp,nrow(dumb[dumb$esp==listaesp[i],]))),2),digits=2,nsmall=2),
                                 nlan=dumbtap[as.vector(dimnames(dumbtap)[[1]])==listaesp[i]]))
  }
  #browser()
  for (i in 4:6) {dumbres[,i]<-as.numeric(as.character(dumbres[,i]))}
  dumbres[order(as.numeric(as.character(dumbres[,4])),decreasing=T),]
}
