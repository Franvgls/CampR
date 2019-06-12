#' Capturas del día para las especies peces
#'
#' Da un resumen de las especies capturadas en el día solicitado (o los días solicitados). Sirve para dar los resúmenes de capturas que piden en el "Vizconde de Eza" y otros barcos peces y crust permiten seleccionar las especies que a extraer
#' @param camp Es la campaña de la que queremos tomar los datos
#' @param dns Origen de Base de Datos seleccionado. Se usa sobretodo para "Porc" Porcupine pero ver "Cant", "Arsa", "Medi"
#' @param dias Los días del mes en que queremos tomar los datos, pueden pedirse varios juntos. No acepta mezclar meses distintos
#' @param peces Las especies de peces que se quieren incluir en los datos
#' @param crust Las especies de crustáceos que se incluir en los datos
#' @return Devuelve la fecha de la que se toman los datos, un listado de lances contemplados y un data.frame con variables Especie,captura (kgs) ordenados por abundancia en las capturas
#' @export
captdia.camp<-function(camp="C14",dns="Cant",dias,peces=c(50,42,43,44,45,50,51,80,90,60,99),crust=c(19,29)) {
  for (i in 1:length(dias)) {
    if (nchar(dias[i])==1) dias[i]<-paste(0,dias[i],sep="")
  }
  #browser()
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  fauna<-DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camp))
  DBI::dbDisconnect(ch1)
  lan<-datlan.camp(camp,dns,redux=TRUE,incl2=TRUE,incl0=FALSE)
  lan<-lan[,c("lance","fecha")]
  lansdia<-substr(as.Date(lan$fecha,format="%d-%m-%y"),9,10)
  #browser()
  fecha<-levels(as.factor(lan[substr(as.Date(lan$fecha,format="%d-%m-%y"),9,10) %in% dias,2]))
  lan<-lan[substr(as.Date(lan$fecha,format="%d-%m-%y"),9,10) %in% dias,1]
  fecha<-paste("Fecha:",paste(substr(fecha,9,10),substr(fecha,6,7),substr(fecha,3,4),sep="/"))
  fauna<-fauna[fauna$LANCE %in% lan,]
  total<-sum(fauna$PESO)
  totlan<-tapply(fauna$PESO_GR,fauna$LANCE,sum)
  fauna<-fauna[c(fauna$GRUPO=="1" & fauna$ESP %in% peces) | c(fauna$GRUPO==2 & fauna$ESP%in% crust),]
  capt<-tapply(fauna$PESO_GR,fauna[,c(3:2)],sum)
  if (ncol(capt)>1) capt<-data.frame(grupo=c(rep("1",nrow(capt)),rep("2",nrow(capt))),
                                     esp=rep(rownames(capt),2),peso=c(capt[,1],capt[,2]))
  if (ncol(capt)==1) capt<-data.frame(grupo=1,esp=rownames(capt),peso=capt[,1])
  capt<-capt[!is.na(capt$peso),]
  capt$spc<-"0"
  for (i in 1:nrow(capt)) {
    capt$spc[i]<-buscaesp(capt$grupo[i],capt$esp[i])
  }
  dumb<-data.frame(Especie=c(capt$spc,"Total"),Captura=c(round(c(capt$peso,total)/1000,1)))
  print(fecha)
  print(paste("Lances: ",paste(lan,collapse=", ",sep=""),".",sep=""))
  nlansscapt<-length(lan[!lan %in% names(totlan)])
  if (nlansscapt>0) print(paste(ifelse(nlansscapt==1,"Lance: ","Lances: "),paste(lan[!lan %in% names(totlan)],collapse=" "),"sin captura"))
  dumb[order(dumb$Captura,decreasing=TRUE),]
}
