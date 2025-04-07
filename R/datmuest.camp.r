#' Datos del muestreo de la especie esp en la campaña
#'
#' Saca un resumen de los ejemplares y peso muestreados de la especie esp en la campaña camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. Utilizar buscacod(gr,esp) para ver codigos
#' @param camps Campañas de las que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @return Devuelve un data.frame con datos del muestreo en los años/campañas elegidas y contendio: nombre especie,campaña, número de lances en la campaña,
#'      número de lances con la especie,  pesos y número totales muestreados, numero total capturado, peso medio de los bichos medidos y el rango de tallas,
#'       talla mínima, talla máxima y número total capturado
#' @examples  datmuest.camp(2,19,c("P16","P17"),"Porc")
#' @examples datmuest.camp(1,50,Psh,"Porc")
#' @export
datmuest.camp<-function(gr,esp,camps,dns="Cant",excl.sect=NA) {
  esp<-format(esp,width=3,justify="r")
  #if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de mas de una")}
  if (any(length(esp)>1 | esp==999)) {stop("seleccionadas más de una especie, no tiene sentido sacar resultados sobre el muestreo de varias especies, sacarlos por especie y sumarlos")}
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  lan<-datlan.camp(camps[1],dns,redux=TRUE,excl.sect=excl.sect)
  ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camps[1],
                             " where grupo='",gr,"' and esp='",esp,"'"))
  DBI::dbDisconnect(ch1)
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls$lance<-as.numeric(ntalls$lance)
  ntalls$ncapt<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  if (any(!is.na(excl.sect))) {
    ntalls<-ntalls[ntalls$lance %in% lan$lance,]
    }
  talmin<-hablar::min_(ntalls$talla)
  talmax<-hablar::max_(ntalls$talla)
  if (any(is.na(ntalls$numer)))
    {
    warning("Detectado valores perdidos en algún registro de talla")
    print(ntalls[is.na(ntalls$numer),])
  }
  Ntot<-sum(ntalls$numer,na.rm=TRUE)
  Ncapt<-sum(ntalls$ncapt,na.rm=TRUE)
  nhaulstot<-nrow(lan)
  nhaulsp<-length(unique(ntalls$lance))
  if (any(is.na(ntalls$peso.gr))) {
    warning("Detectado valores nulos en algún registro de peso muestra peso.gr fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.gr),])
    }
  Ptot<-round(sum(tapply(ntalls$peso.gr,ntalls$lance,mean,na.rm=TRUE))/1000)
  if (any(is.na(ntalls$peso.m))) {
    warning("Detectados valores nulos en algún registro de peso muestra peso.m fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.m),])
    }
  Pmue<-round(sum(tapply(ntalls$peso.m,ntalls$lance,mean,na.rm=TRUE)),1)
  output<-data.frame(especie=buscaesp(gr,esp),
               camp=camps[1],nhaulstot=nhaulstot,nhaulsp=nhaulsp,
               numero.medido=Ntot,numero.capturado=Ncapt,peso.total.kg=Ptot,
               peso.muestreado.kg=Pmue,peso.medio=Pmue*1000/Ntot,talmin=talmin,talmax=talmax)
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      ch1<-DBI::dbConnect(odbc::odbc(), dns)
      lan<-CampR::datlan.camp(i,dns,redux=TRUE,excl.sect=excl.sect)
      ntalls<-DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",i,
                                         " where grupo='",gr,"' and esp='",esp,"'"))
      DBI::dbDisconnect(ch1)
      names(ntalls)<-gsub("_", ".",names(ntalls))
      ntalls$lance<-as.numeric(ntalls$lance)
      ntalls$ncapt<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
      if (any(!is.na(excl.sect))) {
        ntalls<-ntalls[ntalls$lance %in% lan$lance,]
      }
      talmin<-hablar::min_(ntalls$talla)
      talmax<-hablar::max_(ntalls$talla)
      nhaulstot<-nrow(lan)
      nhaulsp<-length(unique(ntalls$lance))
      if (any(is.na(ntalls$numer)))
      {
        warning("Detectado valores perdidos en algún registro de talla")
        print(ntalls[is.na(ntalls$numer),])
      }
      Ntot<-sum(ntalls$numer,na.rm=TRUE)
      Ncapt<-sum(ntalls$ncapt,na.rm=TRUE)
      if (any(is.na(ntalls$peso.gr))) {
        warning("Detectado valores nulos en algún registro de peso muestra peso.gr fichero ntallXXX.dbf")
        print(ntalls[is.na(ntalls$peso.gr),])
      }
      Ptot<-round(sum(tapply(ntalls$peso.gr,ntalls$lance,mean,na.rm=TRUE))/1000)
      if (any(is.na(ntalls$peso.m))) {
        warning("Detectados valores nulos en algún registro de peso muestra peso.m fichero ntallXXX.dbf")
        print(ntalls[is.na(ntalls$peso.m),])
      }
      Pmue<-round(sum(tapply(ntalls$peso.m,ntalls$lance,mean,na.rm=TRUE)),1)
      output<-rbind(output,data.frame(especie=buscaesp(gr,esp),camp=i,nhaulstot=nhaulstot,nhaulsp=nhaulsp,numero.medido=Ntot,numero.capturado=Ncapt,
                                    peso.total.kg=Ptot,peso.muestreado.kg=Pmue,peso.medio=Pmue*10^3/Ntot,talmin=talmin,talmax=talmax))
    }
  }
  output
}
