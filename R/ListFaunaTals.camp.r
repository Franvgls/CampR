#' Capturas y tallas mínima y máxima de grupos de especies para todos los lances de una campaña
#'
#' Muestras las abundancias totales sin estratificar, en número y peso y las tallas mínima y máxima de un grupo de especies para todos los lances de una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. 6 desechos y no orgánicos no se puede incluir.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param incl2 Si T incluye los lances especiales "2"
#' @seealso {\link{ListFauna.camp}}
#' @examples ListFaunaTals.camp(gr="1",camp="N90",dns="Cant",excl.sect=NA)
#' @export
ListFaunaTals.camp<- function(gr="1",camp,dns,excl.sect=NA,incl2=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (gr==6) {stop("No tiene sentido tallas de deshechos")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  listsps<-DBI::dbGetQuery(ch1,paste("select esp,lance from FAUNA",camp," where grupo='",gr,"'",sep=""))
  talls<-DBI::dbGetQuery(ch1,paste0("select lance,esp,talla,numer,peso_m,peso_gr from NTALL",camp," where grupo='",gr,"'"))
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector")]
  talls$npond<-talls$numer*talls$peso_gr/talls$peso_m
  dumb<-merge(listsps,lan)
  dumb<-merge(dumb,talls)
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  listaesp<-levels(factor(dumb$esp))
  ndat<-length(listaesp)
  if (sum(dim(tapply(dumb$npond,dumb[,c(1,2)],sum)))==0) stop(paste("Ning?n lance tras la exclusi?n de sector",excl.sect[i]))
  dumbtap<-colSums(!is.na(tapply(dumb$npond,dumb[,c(1,2)],sum)))
  dumbpes<-tapply(dumb$peso_gr,dumb$esp,sum)
  dumbsum<-tapply(dumb$npond,dumb$esp,sum)
  dumbmax<-tapply(dumb$talla,dumb$esp,max)
  dumbmin<-tapply(dumb$talla,dumb$esp,min)
  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,nlans=NULL,num=NULL,peso_gr=NULL,Lmin=NULL,Lmax=NULL)
  for (i in 1:ndat) {
    dumbres<-rbind(dumbres,data.frame(gr=gr,esp=as.numeric(as.character(listaesp[i])),especie=buscaesp(gr,listaesp[i]),
                                      nlans=dumbtap[names(dumbtap)==listaesp[i]],
                                      num=round(dumbsum[as.vector(dimnames(dumbsum)[[1]])==listaesp[i]],2),
                                      peso_gr=round(dumbpes[as.vector(dimnames(dumbpes)[[1]])==listaesp[i]],2),
                                      Lmin=dumbmin[as.vector(dimnames(dumbmin)[[1]])==listaesp[i]],
                                      Lmax=dumbmax[as.vector(dimnames(dumbmax)[[1]])==listaesp[i]]))
  }
  dumbres[order(as.character(dumbres[,3]),decreasing=FALSE),]
}
