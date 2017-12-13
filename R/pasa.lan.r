#' Funcion auxiliar para el PescaWin
#' 
#' Saca un fichero de lances de la campaña con el formato de datos del PescaWin
#' @param camp Campaña de las que se cogen los resultados un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param gps Si T exporta el fichero directamente como camp.lan al directorio de lances del PescaWin
#' @param valid Si T incluye sólo los lances válidos (>0) y excluye los nulos (=0)
#' @return Devuelve un data.frame con datos long_l,lat_l,prof_l,long_v,lat_v,prof_v en grados decimales con signo
#' @family PescaWin
#' @export
pasa.lan<-function(camp,dns="Porc",gps=FALSE,valid=TRUE) {
  # Pasa los datos del fichero LancesXXX.dbf a formato PescaWin coloc?ndolos directamente en
  #   el directorio del GPS como XXX.lan
  ch1<-RODBC:odbcConnect(dsn=dns)
  RODBC:odbcSetAutoCommit(ch1, FALSE)
  lan<-RODBC::sqlQuery(ch1,paste("select latitud_l,longitud_l,prof_l,latitud_v,longitud_v,prof_v,validez from LANCE",camp,sep=""))
  if (valid) {lan<-lan[as.numeric(lan$validez)>0,1:7] }
  lan[,c(1,4)]<-trunc(lan[,c(1,4)])+(lan[,c(1,4)]-trunc(lan[,c(1,4)]))/.6
  lan[,c(2,5)]<-(trunc(lan[,c(2,5)])+(lan[,c(2,5)]-trunc(lan[,c(2,5)]))/.6)*(-1)
  lan<-lan[,c(2,1,3,5,4,6)]
  print(lan)
  if (gps) write.table(lan,paste("c:\\gps\\lan\\",camp,".lan",sep=""),sep=",",row.names=FALSE,col.names=FALSE)
}
