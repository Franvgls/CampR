#' Datos del muestreo de la especie esp en la campaña
#' 
#' Saca un resumen de los ejemplares y peso muestreados de la especie esp en la campaña camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @return Devuelve un list con nombre de la especie, número total medido (sexado si se sexa) del fichero de tallas  peso total capturado (del fichero de capturas en kg) y rango de tallas
#' @export
datmuest.camp<-function(gr,esp,camp,dns="Cant",excl.sect=NA) {
  esp<-format(esp,width=3,justify="r")
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de m?s de una")}
  require(RODBC)
  ch1<-odbcConnect(dns)
  lan<-datlan.camp(camp,dns,redux=T,excl.sect=excl.sect)
  ntalls<-sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                             " where grupo='",gr,"' and esp='",esp,"'",sep=""))
  names(ntalls)<-gsub("_", ".",names(ntalls))
  if (any(!is.na(excl.sect))) {
    ntalls<-ntalls[ntalls$lance %in% lan$lance,]
    }
  rgtal<-range(ntalls$talla)  
  Ntot<-sum(ntalls$numer)
  Ptot<-round(sum(tapply(ntalls$peso.gr,ntalls$lance,mean))/1000)
  Pmue<-round(sum(tapply(ntalls$peso.m,ntalls$lance,mean))/1000,1)
  output<-list(especie=paste(buscaesp(gr,esp),"en",camp),
               numero.medido=Ntot,peso.total.kg=Ptot,
               peso.muestreado.kg=Pmue,rango.tallas=rgtal)
  output
}
