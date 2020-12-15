#' Capturas y tallas mínima y máxima de grupos de especies para todos los lances de una campaña
#'
#' Muestras las abundancias totales sin estratificar, en número y peso y las tallas mínima y máxima de un grupo de especies para todos los lances de una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. 6 desechos y no orgánicos no se puede incluir.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad, por defecto NA no filtra por profunidades, debe ser o NA o un rango con dos profundidades
#' @param incl2 Si T incluye los lances especiales "2"
#' @family ListadosFauna
#' @examples ListFaunaTals.camp(gr="1",camp="N90",dns="Cant",excl.sect=NA)
#' @examples ListFaunaTals.camp(gr="1",camp="N90",dns="Cant",excl.sect=NA,profrange=c(400,500))
#' @export
ListFaunaTals.camp<- function(gr="1",camp,dns,cor.time=TRUE,excl.sect=NA,profrange=NA,incl2=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (gr==6) {stop("No tiene sentido tallas de deshechos")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  listsps<-DBI::dbGetQuery(ch1,paste("select grupo,esp,lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep=""))
  talls<-DBI::dbGetQuery(ch1,paste0("select lance,esp,talla,numer,peso_m,peso_gr from NTALL",camp," where grupo='",gr,"'"))
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","prof","weight.time")]
  lan$lance<-format(lan$lance,width = 3)
  talls$npond<-talls$numer*talls$peso_gr/talls$peso_m
  if (any(!is.na(excl.sect))) {
    lan$sector<-gsub("NA","N",lan$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(lan$sector)))>0) lan<-lan[-grep(excl.sect[i],as.character(lan$sector)),]}
    lan$sector<-factor(as.character(lan$sector))
  }
  if (any(!is.na(profrange))) lan<-filter(lan,prof>min(profrange) & prof<max(profrange))
  dumb<-merge(listsps,lan)
  if (cor.time){
  dumb$peso<-dumb$peso_gr/dumb$weight.time
  dumb$num<-dumb$numero/dumb$weight.time
  }
  else dumb$peso=dumb$peso_gr;dumb$num=dumb$numero
  dumbtal<-merge(dumb,talls,all.x = T)
  listaesp<-levels(factor(dumb$esp))
  ndat<-length(listaesp)
  if (sum(dim(tapply(dumb$num,dumb[,c(1,2)],sum)))==0) stop(paste("Ningún lance tras la exclusión de sector",ifelse(!is.na(excl.sect),excl.sect,profrange)))
  dumbtap<-rowSums(!is.na(tapply(dumb$numero,dumb[,c("esp","lance")],sum,na.rm=T)),na.rm=T)
  dumbpes<-round(rowMeans(tapply(dumb$peso,dumb[,c("esp","lance")],mean,na.rm=T),na.rm=T),1)
  dumbsum<-round(rowMeans(tapply(dumb$num,dumb[,c("esp","lance")],mean,na.rm=T),na.rm=T),1)
  dumbmax<-tapply(dumbtal$talla,dumbtal$esp,max,na.rm=T)
  dumbmin<-tapply(dumbtal$talla,dumbtal$esp,min,na.rm=T)
  dumbres<-data.frame(camp=camp,gr=gr,esp=as.numeric(listaesp),especie=NA,nlans=dumbtap,peso_gr=dumbpes,num=dumbsum,Lmin=dumbmin,Lmax=dumbmax)
#  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,nlans=NULL,num=NULL,peso_gr=NULL,Lmin=NULL,Lmax=NULL)
  for (i in 1:nrow(dumbres)) {
    dumbres$especie[i]<-buscaesp(dumbres$gr[i],dumbres$esp[i])
    }
  if (any(!is.na(profrange))) dumbres<-data.frame(camp=dumbres$camp,profmin=profrange[1],profrange[2],dumbres[,2:9])
  if (any(!is.na(excl.sect))) print(paste("Excluidos los sectores/estratos",excl.sect))
  dumbres[order(dumbres$peso_gr,decreasing=TRUE),]
}
