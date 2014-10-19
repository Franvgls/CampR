#' Gráfico con los lances en segmentos
#'
#' gráfica todos los lances de una campaña con las distancias recorridas en cada uno de ellos
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, Solo para el Cantabrico por defecto -10.25 y -1.4 oeste
#' @param ylims Define los limites latitudinales del mapa, Solo para el Cantabrico por defecto 41.82 y 44.48 norte
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux
#' @seealso {\link{datlan.camp}}
#' @examples MapLansGPS("12C","Cant")
#' @export
MapLansGPS<-function(camp,dns="Pnew",incl0=F,xlims=c(-10.25,-1.4),ylims=c(41.82,44.48)) {
  require(RODBC)
  ch1<-odbcConnect(dsn=dns)
  odbcSetAutoCommit(ch1, FALSE)
  lan<-sqlQuery(ch1,paste("select lance,validez,latitud_l,latitud_v,longitud_l,longitud_v,prof_l,prof_v,
                          sector,estrato,cable,dista_p,abert_h,abert_v,recorrido,fecha,ewl,ewv,cuadricula from LANCE",camp,sep=""))
  odbcClose(ch1)
  lan$latitud_l<-sapply(lan$latitud_l,gradec)
  #browser()
  lan$longitud_l<-sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="E",1,-1)
  lan$latitud_v<-sapply(lan$latitud_v,gradec)
  lan$longitud_v<-sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="E",1,-1)
  lannul<-lan[lan$validez==0,c("longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  lan<-lan[lan$validez!=0,c("longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  if (dns=="Pnew") {
    mapporco()
    }
  if (dns=="Cant" | dns=="Cnew") {
    MapNort(xlims=xlims,ylims=ylims)
  }
  if (dns=="Arsa") {
    MapArsa()
  }
  segments(lan$longitud_l,lan$latitud_l,lan$longitud_v,lan$latitud_v,col=1,lwd=2)
  if (incl0) segments(lannul$longitud_l,lannul$latitud_l,lannul$longitud_v,lannul$latitud_l,col=2,lwd=2)
}