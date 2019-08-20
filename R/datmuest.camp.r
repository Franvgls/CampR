#' Datos del muestreo de la especie esp en la campaña
#'
#' Saca un resumen de los ejemplares y peso muestreados de la especie esp en la campaña camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. Utilizar buscacod(gr,esp) para ver codigos
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @return Devuelve un list con nombre de la especie, número total medido (sexado si se sexa) del fichero de tallas,  peso total capturado (del fichero de capturas en kg), peso total muestreado del fichero de tallas y rango de tallas.
#' @examples  datmuest.camp(2,19,"P16","Porc")
#' @export
datmuest.camp<-function(gr,esp,camp,dns="Cant",excl.sect=NA) {
  esp<-format(esp,width=3,justify="r")
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de mas de una")}
  if (any(length(esp)>1 | esp==999)) {stop("seleccionadas más de una especie, no tiene sentido sacar resultados sobre el muestreo de varias especies, sacarlos por especie y sumarlos")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect)
  ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                             " where grupo='",gr,"' and esp='",esp,"'"))
  DBI::dbDisconnect(ch1)
  names(ntalls)<-gsub("_", ".",names(ntalls))
  if (any(!is.na(excl.sect))) {
    ntalls<-ntalls[ntalls$lance %in% lan$lance,]
    }
  rgtal<-range(ntalls$talla)
  if (any(is.na(ntalls$numer)))
    {
    warning("Detectado valores perdidos en algún registro de talla")
    print(ntalls[is.na(ntalls$numer),])
  }
  Ntot<-sum(ntalls$numer,na.rm=TRUE)
  if (any(is.na(ntalls$peso.gr))) {
    warning("Detectado valores nulos en algún registro de peso muestra peso.gr fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.gr),])
    }
  Ptot<-round(sum(tapply(ntalls$peso.gr,ntalls$lance,mean,na.rm=TRUE))/1000)
  if (any(is.na(ntalls$peso.m))) {
    warning("Detectados valores nulos en algún registro de peso muestra peso.m fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.m),])
    }
  Pmue<-round(sum(tapply(ntalls$peso.m,ntalls$lance,mean,na.rm=TRUE))/1000,1)
  output<-list(especie=paste(buscaesp(gr,esp),"en",camp),
               numero.medido=Ntot,peso.total.kg=Ptot,
               peso.muestreado.kg=Pmue,rango.tallas=rgtal)
  output
}
