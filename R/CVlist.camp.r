#' Abundancias en número y peso totales para todas las especies de un grupo en una campaña
#'
#' Función de resultados
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param camp Campaña de la que se solicitan los resultados un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew" o "Porc", Cantábrico "Cant" o "Cnew", Golfo de Cádiz "Arsa" y Mediterráneo "Medi" P
#' @param cor.time si T corrige abundancias a duración de lance de 30 minutos
#' @param kg Si T saca los resultados en Kg, si F en gramos
#' @param dec Número de decimales de los resultados
#' @param percents Si T devuelve los resultados en porcentaje del total
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @param minim Excluye por defecto los valores con peso menor de el valor que se elija (atención si kg=T), también se puede hacer a posteriori para cualquiera de los campos con tabla tabla$NombColumn>XX,
#' @return Devuelve un data.fram con valores en orden decreciente de abundancia con columnas gr,esp,especie,familia,camp,weight,number,nlan (número de lances en que aparece cada especie)
#' @examples CVlist.camp(gr=1,camp="P06",dns="Porc")
#' @export
#'
CVlist.camp<- function(gr="1",camp,dns,kg=TRUE,dec=3,percents=FALSE,cor.time=TRUE,excl.sect=NA,minim=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  gr<-as.character(gr)
  ch1<-RODBC::odbcConnect(dsn=dns)
  listsps<-RODBC::sqlQuery(ch1,paste("select grupo, esp from FAUNA",camp,sep=""),as.is=TRUE)
  ch2<-RODBC::odbcConnect(dsn="camp")
  espfam<-RODBC::sqlQuery(ch2,"select grupo,esp,especie,familia from Especies",as.is=TRUE)
  RODBC::odbcCloseAll()
  if (gr!="9") {
    listsps<-listsps[listsps$grupo==gr,]
    espfam<-espfam[espfam$grupo==gr,]
  }
  else {
    listsps<-listsps[listsps$grupo!=6,]
    espfam<-espfam[espfam$grupo!=6,]
  }
  espfam$esp<-format(espfam$esp,width=3,justify="r")
  listsps$esp<-sapply(listsps$esp,format,justify="r",width=3)
  listesp<-levels(factor(listsps$esp))
  listke<-levels(factor(paste(listsps$grupo,listsps$esp,sep="")))
  espfam$ke<-paste(espfam$grupo,espfam$esp,sep="")
  dumb<-data.frame(gr=substr(listke,1,1),esp=substr(listke,2,5),especie=espfam$especie[match(listke,espfam$ke)],
                   familia=espfam$familia[match(listke,espfam$ke)],camp=camp,weight=NA,number=NA,nlan=NA,stringsAsFactors=FALSE)
  for (i in 1:nrow(dumb)) {
    temp<-CV.camps(dumb$gr[i],dumb$esp[i],camp,dns,cor.time=cor.time,kg,dec,excl.sect)
    dumb$weight[i]<-as.numeric(temp[2])
    dumb$number[i]<-as.numeric(temp[3])
    dumb$nlan[i]<-sum(datgr.camp(gr=dumb$gr[i],esp=dumb$esp[i],camp,dns,cor.time=cor.time)[,5]>0)
  }
  dumb$especie[is.na(dumb$especie)]<-"ERROR: CODIGO DESCONOCIDO"
  dumbnam<-colnames(dumb)
  if (percents) {
    dumb<-cbind(dumb[,1:5],round(dumb[,6]*100/sum(dumb[,6]),dec),round(dumb[,7]*100/sum(dumb[,7]),dec),
                dumb[,8])
    colnames(dumb)<-dumbnam
  }
  row.names(dumb)<-NULL
  if (!is.logical(minim)) {dumb<-dumb[dumb$weight>minim,]}
  dumb[order(dumb[,6],decreasing=TRUE),]
}

