#' Extrae ALK para una especie de los ficheros del camp 
#'  
#' Función de acceso a datos: recupera la clave talla edad de una especie y campaña determinadas
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param n.ots Número de otolitos o proporción? Si T da el número de otolitos  
#' @param AltAlk ALK alternativa tomada de un fichero edadXYY.dbf sin ruta ni extensión
#' @examples GetAlk.camp(1,43,"N93","Cant",8) 
#' @examples GetAlk.camp(1,45,"P03","Porc",AltAlk="edadXYY") 
#' @family edades
#' @export
GetAlk.camp<-function(gr,esp,camp,dns="Porc",plus=8,n.ots=FALSE,AltAlk=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("Sólo se puede incluir una especie en esta función")}
  esp<-format(esp,width=3,justify="r")
  ch1<-RODBC::odbcConnect(dns)
  if (is.logical(AltAlk) | is.na(AltAlk)) {
    edad<-RODBC::sqlQuery(ch1,paste("select * from EDAD",camp," where grupo='",
                             gr,"' and esp='",esp,"'",sep=""))

    if (nrow(edad)==0) stop(paste("no existe clave talla edad para la especie",buscaesp(gr,esp),"en la campaña",camp))
    }  
  else {
    edad<-RODBC::sqlFetch(ch1,AltAlk)
    edad$ESP<-format(edad$ESP,width=3,justify="r")
    edad<-edad[edad$GRUPO==gr & edad$ESP==esp,]
    }   
  edad[is.na(edad)]<-0
  edad<-edad[which(rowSums(edad[5:20],na.rm=TRUE)>0),-c(1,2,21)]
  edad<-edad[order(edad$TALLA,edad$SEXO),]
  if (plus<15) edad<-data.frame(edad[,1:(plus+2)],plus=rowSums(edad[,(plus+3):ncol(edad)]))
  if (!n.ots) edad<-data.frame(edad[,2:1],edad[,3:ncol(edad)]/rowSums(edad[,-c(1:2)])) else edad<-data.frame(edad[,2:1],edad[,3:ncol(edad)])
  names(edad)<-c("talla","sexo",paste("E",0:(plus-1),sep=""),paste("E",plus,"+",sep=""))
  # identifica si la ALK est? hecha por sexos o conjunta
  agebysex<-ifelse(any(edad$SEXO!=3),T,F)
  RODBC::odbcCloseAll()
  edad[,c(1,3:ncol(edad))]
}
