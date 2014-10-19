#' Extrae ALK para una especie de los ficheros del camp 
#'  
#' Función de acceso a datos: recupera la clave talla edad de una especie y campaña determinadas
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param AltAlk ALK alternativa tomada de un fichero edadXYY.dbf
#' @seealso grafedtal.camp {\link{grafedtal.camp}} 
#' @examples GetAlk.camp("1"," 43","N93","Cant",8) 
#' @examples \dontrun GetAlk.camp(1,45,"P03","Pnew",AltAlk="path\\edadXYY.DBF")
#' @export
GetAlk.camp<-function(gr,esp,camp,dns="Pnew",plus=8,AltAlk=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("Sólo se puede incluir una especie en esta función")}
  require(RODBC)
  esp<-format(esp,width=3,justify="r")
  if (is.na(AltAlk)) {
    ch1<-odbcConnect(dns)
    edad<-sqlQuery(ch1,paste("select * from EDAD",camp," where grupo='",
                             gr,"' and esp='",esp,"'",sep=""))

    if (nrow(edad)==0) stop(paste("no existe clave talla edad para la especie",buscaesp(gr,esp),"en la campaña",camp))
    }
  else {
    require(foreign)
    edad<-read.dbf(AltAlk,as.is=T) 
    edad$ESP<-format(edad$ESP,width=3,justify="r")
    edad<-edad[edad$GRUPO==gr & edad$ESP==esp,]
    }   
  edad[is.na(edad)]<-0
  edad<-edad[which(rowSums(edad[5:20],na.rm=T)>0),-c(1,2,21)]
  edad<-edad[order(edad$TALLA,edad$SEXO),]
  if (plus<15) edad<-data.frame(edad[,1:(plus+2)],plus=rowSums(edad[,(plus+3):ncol(edad)]))
  edad<-data.frame(edad[,2:1],edad[,3:ncol(edad)]/rowSums(edad[,-c(1:2)]))
  names(edad)<-c("talla","sexo",paste("E",0:(plus-1),sep=""),paste("E",plus,"+",sep=""))
  # identifica si la ALK est? hecha por sexos o conjunta
  agebysex<-ifelse(any(edad$SEXO!=3),T,F)
  odbcCloseAll()
  edad
}
